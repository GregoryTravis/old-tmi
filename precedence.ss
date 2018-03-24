(define operator-precedence-levels
  '(($$)
    (* /)
    (+ -)
    (< > ==)))

(define (wrapapp-wrap xs)
  (map (lambda (x)
         (mtch x
           ('operator . _) x
           x `(wrapapp ,x)))
       xs))
(asseq
  '((wrapapp (identifier "a")) (operator "$$") (wrapapp (identifier "b")))
  (wrapapp-wrap '((identifier "a") (operator "$$") (identifier "b"))))

(define (operator? e) (mtch e ('operator . _) #t _ #f))

(define (add-$$ e)
  (mtch e
    (a b . d)
      (if (and (not (operator? a)) (not (operator? b)))
        `(,a (operator "$$") . ,(add-$$ `(,b . ,d)))
        `(,a . ,(add-$$ `(,b . ,d))))
    (a) e))

(define (nest-ops levels e)
  (mtch levels
    '() e
    (ops . opses) (nest-ops opses (nest-ops-1 ops e))))

(define (nest-ops-1 ops e)
  (mtch e
    (a)
      e
    (a ('operator op . opd) b . rest)
      (if (member (string->symbol op) ops)
        (nest-ops-1 ops `((binop ,a (operator ,op . ,opd) ,b) . ,rest))
        `(,a (operator ,op . ,opd) . ,(nest-ops-1 ops`(,b . ,rest))))))
;(tracefun nest-ops-1)

(define (un-wrapapp-wrap e)
  ;(shew 'UM e)
  (mtch e
    ('wrapapp e)
      e
    ('binop ('binop a ('operator "$$") b) ('operator "$$") c)
      (mtch (un-wrapapp-wrap `(binop ,a (operator "$$") ,b))
        ('app xs)
          `(app ,(append xs `(,(un-wrapapp-wrap c)))))
    ('binop a ('operator "$$") b)
      `(app (,(un-wrapapp-wrap a) ,(un-wrapapp-wrap b)))
    ('binop a op b)
      `(binop ,(un-wrapapp-wrap a) ,op ,(un-wrapapp-wrap b))))
;(tracefun un-wrapapp-wrap)

(define (precedence-app xs)
  (mtch (nest-ops operator-precedence-levels (add-$$ (wrapapp-wrap xs)))
    (b) (un-wrapapp-wrap b)))

(define (precedence-1 e)
  (mtch e
    ('app xs) (precedence-app xs)
    x x))

(define (precedence e)
  (general-recurser-s precedence-1 id e))
