rm -f tmi tmi.hi
ghc -fprof-auto -fprof-cafs -prof -o tmi tmi.hs
./tmi
