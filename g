rm -f tmi tmi.hi
rm -f out
# -fwarn-incomplete-patterns
(ghc -Werror -fprof-auto -fprof-cafs -prof -o tmi tmi.hs) 2>&1 | tee out
(./tmi +RTS -xc ) 2>&1 | tee -a out
