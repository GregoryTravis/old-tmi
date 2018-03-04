echo '(begin (dload "compile.ss") (main "input.tmi"))' >> /tmp/racket-daemon-ctrl
#racket -r compile.ss input.tmi
exit
