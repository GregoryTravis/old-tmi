rm -f tmi tmi.hi
rm -f out
# -fwarn-incomplete-patterns
# -prof -- caused link error when I started using Data.Vector
(ghc -Werror -ferror-spans -fprof-auto -fprof-cafs -o tmi tmi.hs) 2>&1 | tee out
# -cx -- removed because removed -prof
(time ./tmi +RTS) 2>&1 | tee -a out
grep Um out
