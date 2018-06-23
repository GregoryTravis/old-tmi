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

(define defs
  '((def fold ((clause (pat (app ((var f) (cton ((ctor Cons) (var x) (var xs))) (var z))))
                       (body (app ((var f) (var x) (app ((var fold) (var xs) (var z)))))))
               (clause (pat (app ((var f) (cton ((ctor Nil))) (var z))))
                       (body (var z)))))))

(define (add-type-to-def def)
  (mtch def
    ('def name clauses)
      `(def ,name ,(map add-type-to-clause clauses))))
(define (add-type-to-clause clause)
  (mtch clause
    ('clause ('pat pat) ('body body))
      `(typed ,(ty) (clause (pat ,(add-type-to-exp pat)) (body ,(add-type-to-exp body))))))
; 'exp' means 'pat or body'
(define (add-type-to-exp exp)
  (mtch exp
    ('app exps)
      `(typed ,(ty) (app ,(map add-type-to-exp exps)))
    ('cton exps)
      `(typed ,(ty) (cton ,(map add-type-to-exp exps)))
    ('ctor c)
      `(typed ,(ty) (ctor ,c))
    ('var id)
      `(typed ,(ty) (var ,id))))

(define typed-defs (map add-type-to-def defs))
(shew 'outside-main)
(define (main)
  (set! ty (make-type-symgen))
  (shew typed-defs))
  ;(shew (ntimes-f 100 ty)))
