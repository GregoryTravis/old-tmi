(define native-preamble
  '(begin
    (define == equal?)
    (define (!= a b) (not (equal? a b)))))
