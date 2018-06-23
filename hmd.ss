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

(define defs
  '((def fold (/./. ((clause (pat (app ((var f) (cton ((ctor Cons) (var x) (var xs))) (var z))))
                             (body (app ((var f) (var x) (app ((var fold) (var xs) (var z)))))))
                     (clause (pat (app ((var f) (cton ((ctor Nil))) (var z))))
                             (body (var z))))))))

(define (add-type-to-def def)
  (mtch def
    ('def name ('/./. clauses))
      `(def ,name (typed ,(ty) ,(map add-type-to-clause clauses)))))
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

(define (gen-unis-for-def-scope def)
  (mtch def
    ('def name ('typed t clauses))
      (apply append (map gen-unis-for-clause-scope clauses))))
(define (gen-unis-for-clause-scope clause)
  (mtch clause
    ('typed t
    ('clause ('pat pat) ('body body)))
      (let ((bound (gather-exp-vars pat))
            (referenced (gather-exp-vars body)))
        (apply append
          (map (lambda (var)
                 (mtch var ('typed ref-t ('var ref-id))
                   (mtch (find-var-in-list ref-id bound)
                     ('typed bound-t ('var bound-id))
                       (begin
                         (assert (eq? ref-id bound-id))
                         (list (list bound-t ref-t)))
                     '()
                       '())))
               referenced)))))
(define (gather-exp-vars exp)
  (mtch exp
    ('typed t ('app exps))
      (apply append (map gather-exp-vars exps))
    ('typed t ('cton exps))
      (apply append (map gather-exp-vars exps))
    ('typed t ('ctor c))
      '()
    ('typed t ('var id))
      (list exp)))
(define (find-var-in-list id varlist)
  (mtch varlist
    (('typed t ('var vid)) . d)
      (if (eq? vid id)
        (car varlist)
        (find-var-in-list id d))
    '()
      '()))

(define typed-defs (map add-type-to-def defs))

(define cl '(clause (pat (cton ((ctor C) (var x) (var y)))) (body (app ((var f) (var y) (var x))))))
(define tcl (add-type-to-clause cl))

(define (main)
  (set! ty (make-type-symgen))
  ;(shew cl tcl)
  ;(shew (gen-unis-for-clause-scope tcl))

  (shew typed-defs)
  (shew (apply append (map gen-unis-for-def-scope typed-defs)))

  ;(shew typed-defs))
  ;(shew (ntimes-f 100 ty))
  )
