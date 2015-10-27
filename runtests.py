import os
import os.path

print "**** BURLESQUE ****"
print "******* TESTSUITE ******"

win=''
try:
  os.remove('blsq'+win)
except:
  pass
os.system('ghc --make -o blsq'+win+' main_golf.hs')

tests = os.listdir('./tests')
for test in tests:
  print '** ' + test 
  if(not os.path.isfile('./tests/'+test)):
    print '... is not a file!'
    continue
  if(not test.endswith('.blsq')):
    continue
  print '... running it ...'
  ph = os.path.abspath('./tests/'+test)
  phin = ph + '.in'
  phout = ph + '.out'
  phtmp = ph + '.tmp'
  
  proc = os.system('cat ' + phin + ' | ./blsq'+win+' --file ' + os.path.abspath('./tests/'+test) + ' > ' + phtmp)
  
  if(proc != 0):
    print '** FAIL ** FAIL **'
    quit()
  
  fout = open(phout)
  fouttxt = fout.read()
  ftmp = open(phtmp)
  ftmptxt = ftmp.read()

  while(ftmptxt.endswith("\n")):
    ftmptxt = ftmptxt[:-1]
  while(ftmptxt.endswith("\r\n")):
    ftmptxt = ftmptxt[:-2]
    
  fouttxt = fouttxt.replace("\r\n","\n")
  ftmptxt = ftmptxt.replace("\r\n","\n")
  
  if(fouttxt != ftmptxt):
    print '** FAIL ** FAIL **'
    quit()
  
  print '..... [%d]' % proc
