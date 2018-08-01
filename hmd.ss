(dload "lib.ss")
(dload "mtch.ss")

#|
Typing rules

TLD - left and right of def have the same type
APP - (((f :: a) (x :: b)) :: c) => f :: (b) -> c
xx PAIR - (x :: b, y :: c) :: a => a = (b, c)
CTOR - type List a = Cons a (List a) | Nil => (Cons (x :: a) y) :: List a
SCOPE - /. x :: a -> ... x :: a ...
CASELAMBDA - (/./. p0 b0 p1 b1 ...) => (sum p0 p1 ...) => (sum b0 b1 ..)

todo
- lambda (same as caselambda?); repeated vars
- should global ref be a var or something else?
|#

(define (make-type-symgen)
  (let ((n 0))
    (lambda ()
      (assert (< n (* 26 27)))
      (let ((sym (string->symbol
                   (list->string
                     (map integer->char
                       (if (> n 25)
                         (list (+ 97 (quotient n 26)) (+ 97 (modulo n 26)))
                         (list (+ 97 n))))))))
        (set! n (+ n 1))
        sym))))

(define ty (make-type-symgen))

; (type ((typector T) (var a) ...) (pat pat ...))
; (pat exp)    ;; Except pats don't have apps
; (body exp)
; (exp (app exps) | (cton exps) | (var id) | (ctor C))
(define types
  '((type ((typector List) (var a))
          ((pat (cton ((ctor Cons) (var a) (type ((typector List) (var a))))))
           (pat (cton ((ctor Nil))))))))

(define initial-type-env
  ;;'((+ (F (P Int Int) Int))))
  '((+ . (F (C Int) (F (C Int) (C Int))))))

;(define foo '(L (V f) (L (V x) (A (V +) (P (A (V f) (V x)) (V x))))))
; (Int -> Int) -> Int -> Int
; /. f /. x (f x) + x
(define foo '(L (V f) (L (V x) (A (A (V +) (A (V f) (V x))) (V x)))))

(define (tinf e)
  (tinf0 e initial-type-env '()))

; (exp, env, unis) -> (typed-exp, unis)
(define (tinf0 e env unis)
  (mtch e
    ('L ('V var) body)
      (let ((var-t (ty)))
        (mtch (tinf0 body `((,var . (TV ,var-t)) . ,env) unis)
          ((T body body-t) unis)
            ;`((T (L (T (V ,var) (TV ,var-t)) (T ,body ,body-t)) (F (TV ,var-t) (TV ,body-t))) ,unis)))
            `((T (L (T (V ,var) (TV ,var-t)) (T ,body ,body-t)) (F (TV ,var-t) ,body-t)) ,unis)))
    ('A fun arg)
      (let ((result-t (ty)))
        (mtch (tinf0 fun env unis)
          ((T fun fun-t) unis)
            (mtch (tinf0 arg env unis)
              ((T arg arg-t) unis)
                `((T (A (T ,fun ,fun-t) (T ,arg ,arg-t)) (TV ,result-t)) ((,fun-t (F ,arg-t (TV ,result-t))) . ,unis)))))
    ('V x)
      `((T ,e ,(cdr (assoc x env))) ,unis)))
(tracefun tinf0)

(define (ut a b)
  (if (not (equal? a b))
    (begin
      (shew a b)
      (err 'test-failure))
    '()))

(ut 1 1)
;(ut 1 2)

(define (unify unis)
  (mtch unis
    ()
      '()
    (uni . unis)
      (let ((unifiers (find-unifiers uni)))
        (append unifiers (unify (apply-unifiers unifiers unis))))))
        ;(append unifiers (unify unis)))))

;; Feels a little pre-optimized to me
(define (find-unifiers uni)
  (mtch uni
    (('TV a) ('TV b))
      `(((TV ,a) (TV ,(ty))) ((TV ,b) (TV ,(ty))))
    (('TV a) b)
      `(((TV ,a) ,b))
    (b ('TV a))
      `(((TV ,a) ,b))
    (('F a b) ('F c d))
      (append (find-unifiers `(,a ,c)) (find-unifiers `(,b ,d)))
    ((C a) (C b))
      (if (eq? a b) '() (err 'type-mismatch a b))
      ))

(define (apply-unifiers unifiers unis)
  (map (lambda (uni)
    (mtch uni
      (a b)
        `(,(apply-unifiers-to-type-term unifiers a) ,(apply-unifiers-to-type-term unifiers b))))
    unis))

(define (apply-unifiers-to-type-term unifiers term)
  (mtch term
    ('TV x)
      (mtch (assoc `(TV ,x) unifiers)
        #f
          term
        (('TV _) rewrite)
          rewrite)
    ('C x)
      term
    ('F a b)
      `(F ,(apply-unifiers-to-type-term unifiers a) ,(apply-unifiers-to-type-term unifiers b))
    ))
;(tracefun apply-unifiers apply-unifiers-to-type-term )
(tracefun apply-unifiers-to-type-term )

(define (apply-unifiers-to-term unifiers e)
  (mtch e
    ('T e t)
      `(T ,(apply-unifiers-to-term unifiers e) ,(apply-unifiers-to-type-term unifiers t))
    ('L v body)
      `(L ,(apply-unifiers-to-term unifiers v) ,(apply-unifiers-to-term unifiers body))
    ('A fun arg)
      `(A ,(apply-unifiers-to-term unifiers fun) ,(apply-unifiers-to-term unifiers arg))
    ('V x)
      e))

    #|
    ('T ('L ('T ('V var) var-t) ('T body body-t)) lambda-t)
      `(T (L (T (V ,var) ,(apply-unifiers-to-type-term unifiers var-t))
             (T ,(apply-unifiers-to-term unifiers body) ,(apply-unifiers-to-type-term unifieres body-t)))
          ,(apply-unifiers-to-type-term unifiers lambda-t))
    ('T ('A a b) t)
      `(T (A ,(apply-unifiers-to-term unifiers a) ,(apply-unifiers-to-term unifiers b))
          ,(apply-unifiers-to-type-term unifiers t))
    ('T ('V x) t)
      `(T (V x) ,(apply-unifiers-to-type-term unifiers t))))
      |#

;(tracefun apply-unifiers-to-term)

(define (main)
  (set! ty (make-type-symgen))
  (shew foo)
  (mtch (tinf foo)
    (typed-exp unis)
      (begin
        (shew typed-exp)
        (shew unis)
        ;(shew (find-unifiers '((TV x) (TV y))))
        ;(shew (find-unifiers '((TV d) (F (TV b) (TV c)))))
        ;(shew (find-unifiers '((F (C Int) (F (C Int) (C Int))) (F (TV e) (TV d)))))
        ;(shew (find-unifiers '((TV a) (F (TV b) (TV e)))))
        ;(shew (find-unifiers '((F (C Int) (TV a)) (F (TV b) (F (C Int) (C Int))))))
        ;(shew (map find-unifiers unis))
        ;(shew (unify unis))
        ;(shew (unify unis))
        ;(shew (unify (reverse (unify unis))))
        ;(shew (unify (reverse (unify unis))))
        (let ((unifiers (unify (reverse (unify unis)))))
          (shew unifiers)
          (shew (apply-unifiers-to-term unifiers typed-exp)))
          (shew typed-exp)
        )))
