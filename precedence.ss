(define operator-precedence-levels
  '(($$)
    (* /)
    (+ -)))

(define (reduce-once ops app-list)
  (mtch app-list
    (a ('operator op . x) b . d)
      (if (member (string->symbol op) ops)
        `((qapp (,a (operator ,op . ,x) ,b)) . ,d)
        `(,a (operator ,op ,x) . ,(reduce-once ops `(,b . ,d))))
    (a) `(,a)))

(define (combine-these ops app-list)
  (if (< (length app-list) 5)
    app-list
    (let ((reduced-maybe (reduce-once ops app-list)))
    ;(shew 'hey app-list reduced-maybe (eq? (length app-list) (length reduced-maybe)))
      (if (eq? (length app-list) (length reduced-maybe))
        app-list
        (combine-these ops reduced-maybe)))))
;(tracefun combine-these)

(define (apply-precedence-levels-1 opls app-list)
  (if (null? opls)
    app-list
    (apply-precedence-levels-1 (cdr opls) (combine-these (car opls) app-list))))
(define (apply-precedence-levels app-list)
  (apply-precedence-levels-1 operator-precedence-levels app-list))
;(define apply-precedence-levels id)
;(tracefun apply-precedence-levels apply-precedence-levels-1 combine-these)

(define (is-operator e)
  (mtch e
    ('operator . x) #t
    x #f))

(define (add-explicit-app-op-to-list e)
  (mtch e
    (a b . d)
      (if (and (not (is-operator a)) (not (is-operator b)))
        `(,a (operator "$$") . ,(add-explicit-app-op-to-list `(,b . ,d)))
        `(,a . ,(add-explicit-app-op-to-list `(,b . ,d))))
    x x))
(define (add-explicit-app-op-1 e)
  (mtch e
    ('app xs) `(app ,(apply-precedence-levels (add-explicit-app-op-to-list xs)))
    x x))
(define (add-explicit-app-op e) (general-recurser add-explicit-app-op-1 id e))
;(tracefun add-explicit-app-op) (tracefun add-explicit-app-op-to-list) (tracefun add-explicit-app-op-1) 

; At this point all expressions are (app (a b c)) where b is an operator, including $$.
; There may also be (app (a)).
; Convert $$-chains to regular multi-arg app nodes, and the rest to binops.
(define (separate-app-op-1 sem)
  (mtch sem
    ('app (a ('operator "$$" . d) b))
      `(app ,(map separate-app-op (unfold-real-app sem)))
    ('qapp (a ('operator "$$" . d) b))
      `(app ,(map separate-app-op (unfold-real-app sem)))
    ('app (a ('operator . d) b))
      `(binop ,(separate-app-op a) (operator . ,d) ,(separate-app-op b))
    ('qapp (a ('operator . d) b))
      `(binop ,(separate-app-op a) (operator . ,d) ,(separate-app-op b))
    x x))
(define (separate-app-op e)
  (general-recurser separate-app-op-1 id e))

(define (unfold-real-app sem)
  (mtch sem
    ('app (a ('operator "$$" . d) b))
      (append (unfold-real-app a) `(,b))
    ('qapp (a ('operator "$$" . d) b))
      (append (unfold-real-app a) `(,b))
    x `(,x)))
    ;('app (a op b))
      ;`(,sem)))
;(tracefun unfold-real-app separate-app-op-1)

(define (precedence e)
  (separate-app-op (add-explicit-app-op e)))
;(tracefun precedence)
