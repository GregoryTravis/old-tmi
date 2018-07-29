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
  '((+ (F Int (F Int Int)))))

;(define foo '(L (V f) (L (V x) (A (V +) (P (A (V f) (V x)) (V x))))))
; (Int -> Int) -> Int -> Int
; /. f /. x (f x) + x
(define foo '(L (V f) (L (V x) (A (A (V +) (A (V f) (V x))) (V x)))))

;; This isn't really an env so much as a set of equations to unify; maybe
;; it should be in env (var->exp map) form.  Not clear when I should be
;; adding bindings/equations or not.
(define (tinf e)
  (mtch (tinf0 e initial-type-env)
    (t env)
      (let ((t-var (ty)))
        `(,t-var ,(unify `((,t-var . ,t) . ,env))))))
(define (tinf0 e env)
  (mtch e
    ('L var body)
      (mtch (tinf0 var env)
        (var-t env)
          (mtch (tinf0 body env)
            (body-t env)
              `((F ,var-t ,body-t) ,env)))
    ('V ident)
      (mtch (assoc ident env)
        (_ . ident-t)
          `(,ident-t ,env)
        #f
          (let ((ident-t (ty)))
            `(,ident-t ((,ident . ,ident-t) . ,env))))
    ('A f a)
      (let ((f-arg-t (ty)) ;; ONO maybe don't need a fresh arg var here
            (f-result-t (ty)))
        (mtch (tinf0 f env)
          (f-t env)
            (mtch (tinf0 a env)
              (a-t env)
                `(,f-result-t
                  ((,f-t . (F ,f-arg-t ,f-result-t))
                   (,f-result-t . ,a-t)
                   .
                   ,env)))))
    #|
    ('P a d)
      (let ((pair-t (ty)))
        (mtch (tinf0 a env)
          (a-t env)
            (mtch (tinf0 d env)
              (d-t env)
                `((P ,a-t ,d-t) ,env))))
    |#
                ))  ; Should I generating a binding here?
(tracefun tinf0)

; Return a list of substitutions that unifies the equ
; (l . r) -> ((var . subst) ...)
(define (unify-equation equ)
  (mtch equ
    (l . r)
    (cond
      ((atom? l) `(,l . ,r))
      ((atom? r) `(,r . ,l))
      (#t
        (mtch equ
          (('F a0 b0) . ('F a1 b1))
            (append (unify-equation `(,a0 . ,a1)) (unify-equation `(,b0 . ,b1))))))))
(tracefun unify-equation)

(define (unify env)
  ;(map unify-equation env)
  env)

(define (main)
  (set! ty (make-type-symgen))
  (shew foo)
  (shew (tinf foo)))
