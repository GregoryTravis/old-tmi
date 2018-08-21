rm -f tyc.c
send-quit > /dev/null
echo '(begin (dload "hmd.ss") (main))' | nc localhost 5001
echo --
cat tyc.c | clang-format
echo --
gcc -o tyc tyc.c && ./tyc
