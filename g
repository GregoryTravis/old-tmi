racket -r compile.ss input.tmi
exit

rm -f out
echo ==== | tee -a out
wc input.tmi | tee -a out
racket -r compile.ss input.tmi | tee -a out
cat out >> timings
cat timings
