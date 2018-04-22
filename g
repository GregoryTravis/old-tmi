send-quit > /dev/null
echo '(begin (dload "compile.ss") (main "input.tmi"))' | nc localhost 5001
