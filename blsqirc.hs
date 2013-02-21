import System.Timeout
import Control.Exception hiding (catch, bracket_, bracket)
import Data.List
import Network
import System.IO
import System.Time
import System.Exit
import Control.Monad.Reader
-- import Control.Exception -- for base-3, with base-4 use Control.OldException
import Control.OldException
import Text.Printf
import Prelude hiding (catch)
import System.Process

import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval hiding (run)
import Burlesque.Display


runProgramWrapper :: String -> IO String
runProgramWrapper p = do
 readProcess "Burlesque.exe" ["--ircbot",p] "" >>= return . (" "++) . take 80 . head . lines . (++" ")
 
server = "irc.freenode.org"
port   = 6667
chan   = "#esoteric"
nick   = "blsqbot"
 
--
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
-- A socket and the bot's start time.
--
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }
 
--
-- Set up actions to run on start and end, and run the main loop
--
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = (runReaderT run st)
 
--
-- Connect to the server and return the initial bot state
--
connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a
 
--
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
--
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :tutorial bot")
    write "JOIN" chan
    asks socket >>= listen
 
--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io $ catch ((putStrLn s)) (const $ return ())
    if ping s then pong s else Main.eval (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)
 
--
-- Dispatch a command
--
eval :: String -> Net ()
eval     "!blsq_uptime"             = uptime >>= privmsg
eval     "blsqbot please do quit"   = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!blsq " `isPrefixOf` x = privmsg (drop 6 x)
eval     _                     = return () -- ignore everything else
 
--
-- Send a privmsg to the current chan + server
--
privmsg :: String -> Net ()
privmsg s = do
    ss <- foo s
    io $ catch (putStrLn ("El resulto: " ++ ss)) (const $ return())
    write "PRIVMSG" (chan ++ " :" ++ ss)
    where foo p = do result <- io $ timeout (3*10^6) (runProgramWrapper p)
                     case result of
                         Nothing -> return $ "Ain't nobody got time fo' that!"
                         Just q -> return $ q
 
--
-- Send a message out to the server we're currently connected to
--
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ catch (printf    ">> %s %s\n" s t) (const $ return ())
    io $ catch (hPrintf h "%s %s\r\n" s t) (const $ return ())
    io $ catch (printf    "> %s %s\n" s t) (const $ return ())
 
--
-- Calculate and pretty print the uptime
--
uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero
 
--
-- Pretty print the date in '1d 9h 9m 17s' format
--
pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s
 
--
-- Convenience.
--
io :: IO a -> Net a
io = liftIO