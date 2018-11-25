rm -f tmi tmi.hi
rm -f out
(ghc -fprof-auto -fprof-cafs -prof -o tmi tmi.hs) 2>&1 | tee out
(./tmi) 2>&1 | tee -a out
