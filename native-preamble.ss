(define == equal?)
(define (!= a b) (not (equal? a b)))

(define (native-unconsify e)
  (mtch e
    ('Cons a d) (cons (native-unconsify a) (native-unconsify d))
    'Nil '()
    (a . d) (fail 'native-data e)
    x x))

(define (traceo f . args)
  (shew 'traceo f args)
  (let ((result (apply f args)))
    (shew 'traceo-ret result)
    result))

(define (driver-main io)
  (mtch io
    ;; TODO instead of renaming 'args' why not use hygienic macros?
    ('Command ('Cons proc-name args2))
      (let ((proc (eval (string->symbol proc-name)))
            (args2 (native-unconsify args2)))
        (apply proc args2))
    ('Return x)
      x
    ('Seq io kio)
      (driver-main (kio (driver-main io)))))
;(tracefun driver-main)

(define t-int? integer?)
(define t-string? string?)

;; Only for native scheme operators used without overloading; eventually most of these will disappear
(define op* *)
(define op/ /)
(define op+ +)
(define op- -)
(define op< <)
(define op> >)
(define op== ==)
; This isn't used yet except in precedence
; (define op$$ $$)

(define native-+ +)
