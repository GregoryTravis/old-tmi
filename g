rm -f tmi tmi.hi
rm -f out
(ghc -Werror -fwarn-incomplete-patterns -fprof-auto -fprof-cafs -prof -o tmi tmi.hs) 2>&1 | tee out
(./tmi +RTS -xc ) 2>&1 | tee -a out
