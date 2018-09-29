#send-quit > /dev/null
(echo '(begin (dload "hmd.ss") (main "input.tmi"))' | nc localhost 5001) 2>&1 | tee out
