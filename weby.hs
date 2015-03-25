{-# LANGUAGE OverloadedStrings #-}

module Main where

import Happstack.Server (nullConf, simpleHTTP)
import Happstack.Server.RqData
import Happstack.Server.Client
import Happstack.Server.Internal.Types
import Happstack.Server.Internal.Monads
import Happstack.Server.Internal.Cookie
import Happstack.Server.Proxy
import Happstack.Server.Cookie

import Control.Monad.IO.Class

import qualified Data.Map as M
import qualified Network.HTTP as HC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8

import Data.IORef
import Control.Concurrent.MVar
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
   ["--cookieName", cookieName,
    "--verificationUrl", verificationUrl] -> do
           sessionsRef <- (newIORef (M.fromList [])) :: IO (IORef (M.Map String Bool))
           sessionsMutex <- newEmptyMVar

           simpleHTTP nullConf $ myProxy (cookieName, verificationUrl) sessionsRef sessionsMutex

myProxy cfg@(cookieName, verificationUrl) sessionsRef sessionsMutex = do
  liftIO $ putMVar sessionsMutex ()
  sessions <- liftIO $ readIORef sessionsRef
  liftIO $ print sessions
  liftIO $ takeMVar sessionsMutex
  rq <- askRq

  liftIO $ print (rqCookies rq)

  -- Is the session cookie present?
  case lookup cookieName (rqCookies rq) of
    Nothing -> proxyUnauth cfg rq sessionsRef sessionsMutex
    Just cookie ->
      case M.lookup (cookieValue cookie) sessions of
        Nothing -> proxyUnauth cfg rq sessionsRef sessionsMutex
        Just _ -> proxyThrough cookieName rq (cookieValue cookie)

proxyUnauth cfg rq sessionsRef sessionsMutex = do
  -- Need to call the verifier about that...
  let inputs = rqInputsQuery rq
  case lookup "accesstoken" inputs of
    Nothing -> error "Illegal" -- this is illegal
    Just accessToken -> do
      case inputValue accessToken of
        Right bs -> do 
          askVerifier cfg (toStrict bs) rq sessionsRef sessionsMutex

proxyThrough cookieName rq token = do
  resp <- liftIO $ getResponse (unrproxify "localhost" [] (rq { rqSecure = True}) )
  liftIO $ print rq
  case resp of
    Right q -> do
      let headers = addHeader "X-Powered-By" "ZHAW" $ rsHeaders q
      let headers' = addHeader "Set-Cookie" (cookieName ++ "=" ++ token ++ "; path=/;") $ headers
      return $ q { rsHeaders = headers' }

askVerifier (cookieName, verificationUrl) bs rq sessionsRef sessionsMutex = do
  dat <- liftIO $
            HC.simpleHTTP (HC.getRequest $ verificationUrl ++ B8.unpack bs)
                 >>= HC.getResponseBody
  liftIO $ print dat
  if dat /= "resource\n" then error "Wrong resource"
  else do 
    let token = B8.unpack bs
    liftIO $ addSession sessionsRef sessionsMutex (token)
    proxyThrough cookieName rq token


addSession sessionsRef sessionsMutex token = do
  putMVar sessionsMutex ()
  sessions <- readIORef sessionsRef
  writeIORef sessionsRef (M.insert token True sessions) 
  takeMVar sessionsMutex
  

toStrict :: BL.ByteString -> BS.ByteString
toStrict x = BS.concat . BL.toChunks $ x
