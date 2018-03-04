(define == equal?)
(define (!= a b) (not (equal? a b)))

(define (native-unconsify e)
  (mtch e
    ('Cons a d) (cons (native-unconsify a) (native-unconsify d))
    'Nil '()
    (a . d) (fail 'native-data e)
    x x))

(define (driver-main command)
  ;(shew 'command command)
  (mtch command
    ('Command ('Cons command-name args) k)
      (let ((command (string->symbol command-name))
            (args (native-unconsify args)))
        ;(shew `(native command (,command . ,args)))
        (driver-main (k (apply (eval command) args)))) ;(eval (cons command args)))))
    ;; TODO this should be ('Command ('Done))
    ('Command 'Done)
      (void)))
