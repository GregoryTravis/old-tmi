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
  '((+ . (F Int (F Int Int)))))

;(define foo '(L (V f) (L (V x) (A (V +) (P (A (V f) (V x)) (V x))))))
; (Int -> Int) -> Int -> Int
; /. f /. x (f x) + x
(define foo '(L (V f) (L (V x) (A (A (V +) (A (V f) (V x))) (V x)))))

(define (tinf e)
  (tinf0 e initial-type-env '()))

; (exp, env, unis) -> (typed-exp, unis)
(define (tinf0 e env unis)
  (mtch e
    ('L (V var) body)
      (let ((var-t (ty)))
        (mtch (tinf0 body `((,var . ,var-t) . ,env) unis)
          ((T body body-t) unis)
            `((T (T ,body ,body-t) (F ,var-t ,body-t)) ,unis)))
    ('A fun arg)
      (let ((result-t (ty)))
        (mtch (tinf0 fun env unis)
          ((T fun fun-t) unis)
            (mtch (tinf0 arg env unis)
              ((T arg arg-t) unis)
                `((T (A (T ,fun ,fun-t) (T ,arg ,arg-t)) ,result-t) ((,fun-t (F ,arg-t ,result-t)) . ,unis)))))
    ('V x)
      `((T ,e ,(cdr (assoc x env))) ,unis)))
(tracefun tinf0)

(define (main)
  (set! ty (make-type-symgen))
  (shew foo)
  (shew (tinf foo)))
