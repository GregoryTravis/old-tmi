;; The numeric levels only matter relatively; they won't be used in precedence declarations.
(define operators-precedence
  '(($$ . 0)
    (* . 10)
    (/ . 10)
    (+ . 20)
    (- . 20)
    (< . 30)
    (> . 30)
    (== . 30)
    ($ . 100)))
(define (get-operator-precedence-levels)
  (map cdr operators-precedence))
(define (get-operator-precedence-level op)
  (let ((p (assoc op operators-precedence)))
    (if (pair? p)
      (cdr p)
      (let* ((first-char-op (string->symbol (list->string (list (car (string->list (symbol->string op)))))))
             (p (assoc first-char-op operators-precedence)))
        (assert (pair? p) `(cant-determine-precedence ,op))
        (cdr p)))))
(asseq 10 (get-operator-precedence-level '*))
(asseq 10 (get-operator-precedence-level '**++**))

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
    (level . levels) (nest-ops levels (nest-ops-1 level e))))

(define (nest-ops-1 level e)
  (mtch e
    (a)
      e
    (a ('operator op . opd) b . rest)
      (if (eq? level (get-operator-precedence-level (string->symbol op)))
        (nest-ops-1 level `((binop ,a (operator ,op . ,opd) ,b) . ,rest))
        `(,a (operator ,op . ,opd) . ,(nest-ops-1 level `(,b . ,rest))))))
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

(define (precedence xs)
  (let ((levels (unique (sort (get-operator-precedence-levels) <))))
    (mtch (nest-ops levels (add-$$ (wrapapp-wrap xs)))
      (b) (un-wrapapp-wrap b))))
