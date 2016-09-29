Burlesque
=========

Burlesque is a concatenative, dynamic typed, lazy, probably esoteric programming language. Burlesque is a neat command line utility that can be used as a little desktop calculator, homework helper, to manipulate files, do some statistics or even some data analysis. It's also used as a language for code-golf. If a feature is missing you can let me know through a bug report. If you encounter a bug, incorrect documentation please also create a bug report.

```
blsq ) 1234 16j0 9rn.+4cosp
7 5 1 6
1 2 1 9
1 5 2 0
3 0 6 5
blsq ) 4ro)rosp
1
1 2
1 2 3
1 2 3 4
blsq ) "abc"su\[)**
{97 98 99 97 98 98 99 97 98 99}
```

Current stable version: 1.7.3 (used on anagol). Latest version: 1.7.4 (not on anagol). Also check out http://fmnssun.github.com/Burlesque.

If you don't know where to start read the ```TUTORIAL.md``` and try to learn from examples on anagol: http://golf.shinh.org/l.rb?blsq. If you need help you can also consult ```#esoteric``` on ```irc.freenode.net```.

Compile
=======

You need a decently recent ghc and haskell-plattform. 

Install the required packages:

    cabal install split  haskeline mtl regex-compat parsec digits statistics

Then compile

    ghc --make main.hs -o burlesque -O3 -fforce-recomp

If you just want the "--file" option (like on golf.shinh.org) use

    ghc --make main_golf.hs -o blsq -O3 -fforce-recomp
    
Tests
=====

With Burlesque 1.7.2f I added some little tests. However, it's too late to write a comprehensive testsuite.
If you like you can fork this repo and add tests and send me a pull request. Whenever I solve a golf challenge
I'll add my solution to the testsuite (once the challenge is post mortem of course :) )

Contributing
============

I really welcome any help. However, for most of the things you'll need to know Haskell.
If you know Haskell and wanna help: VERY GOOD, contact me! (PM, Push requests whatever, IRC).

If you don't know Haskell:

You can contribute userdefined builtins. If your builtin can be expressed using other
builtins you can create a new builtin through the `Prelude.blsq` file. Just have a look at the file.
I WILL ONLY ACCEPT SUCH BUILTINS IF YOU ALSO PROVIDE THE DOCUMENTATION FOR IT AS WELL AS TESTCASES!
Thus, have a look at `tests/` and `runtests.py`.


License
=======

Copyright (c) 2012, Roman Müntener
 All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:
Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer 
in the documentation and/or other materials provided with the distribution.
Neither the name of the <ORGANIZATION> nor the names of its contributors may be
used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
