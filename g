send-quit > /dev/null
echo '(begin (dload "compile.ss") (main "old.tmi"))' >> /tmp/racket-daemon-ctrl
