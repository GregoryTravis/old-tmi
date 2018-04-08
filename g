rm -f db2.json db3.json
send-quit > /dev/null

echo '(begin (dload "compile.ss") (main "input.tmi"))' >> /tmp/racket-daemon-ctrl
#racket -r compile.ss input.tmi
exit
