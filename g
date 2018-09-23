#send-quit > /dev/null
(echo '(begin (dload "hmd.ss") (main))' | nc localhost 5001) 2>&1 | tee out
