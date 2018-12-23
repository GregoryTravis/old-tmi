rm -f tmi tmi.hi
rm -f out
# -fwarn-incomplete-patterns
(ghc -Werror -ferror-spans -fprof-auto -fprof-cafs -prof -o tmi tmi.hs) 2>&1 | tee out
(time ./tmi +RTS -xc ) 2>&1 | tee -a out
