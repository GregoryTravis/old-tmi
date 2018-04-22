send-quit
echo '(begin (dload "compile.ss") (main "tests.tmi"))' | nc localhost 5001
