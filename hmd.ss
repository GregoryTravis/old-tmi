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

(define types
  '((type ((typector List) (var a))
          ((pat (cton ((ctor Cons) (var a) (type ((typector List) (var a))))))
           (pat (cton ((ctor Nil))))))))

(define defs
  '((def fold (clause (pat (app ((var f) (cton ((ctor Cons) (var x) (var xs))) (var z))))
                      (body (app ((var f) (var x) (app ((var fold) (var xs) (var z)))))))
              (clause (pat (app ((var f) (cton ((ctor Nil))) (var z))))
                      (body (var z))))))
