send-quit > /dev/null
echo '(begin (dload "compile.ss") (main "old.tmi"))' | nc localhost 5001
