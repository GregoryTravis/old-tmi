racket -r test.ss
diff <(racket -r compile.ss tests.tmi) <(echo 0)
