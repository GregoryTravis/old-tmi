(dload "lib.ss")
(dload "mtch.ss")

;; types: ec, ecs, eqn, eqns, e, te

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

#|
; /. f /. xs /. z if xs == [] z else (f (car xs) (fold f (cdr xs) z))
; fold f [] z = z
; fold f (x : xs) z = f x (fold f xs z)
; forall a b . (a -> b -> b) -> ((List a) -> (b -> b))
fix f = f (fix f)
fix f x = f (fix f) x
fix f x = f (/. x fix f x) x
f :: (a -> b) -> (a -> b)
fix f :: a -> b
fix :: ((a -> b) -> (a -> b)) -> (a -> b)
; Open-recursion fold
; Fix /. rec /. f /. xs /. z if (xs == []) z else (f (car xs) (rec f (cdr xs) z))
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
  '(
    ; int -> int -> int
    (+ . (PT Fun ((C Int) (PT Fun ((C Int) (C Int))))))
    ; a -> a -> Bool
    (== . (Forall ((TV a)) (PT Fun ((TV a) (PT Fun ((TV a) (C Bool)))))))
    ; a -> List a -> List a
    (Cons . (Forall ((TV a)) (PT Fun ((TV a) (PT Fun ((PT List ((TV a))) (PT List ((TV a)))))))))
    ; List a
    (Nil . (Forall ((TV a)) (PT List ((TV a)))))
    ; List a -> a
    (car . (Forall ((TV a)) (PT Fun ((PT List ((TV a))) (TV a)))))
    ; List a -> List a
    (cdr . (Forall ((TV a)) (PT Fun ((PT List ((TV a))) (PT List ((TV a)))))))
  ))

(define (get-constant-type k)
  (cond
    ((number? k) '(C Int))
    ((or (eq? k #t) (eq? k #f)) '(C Bool))
    (#t (err 'bad-constant k))))

; So this needs:
; + List T = Nil | Const T (List T)
; - multiple clauses
;   - this is probablly just: set them equal to each other
; + parameterized types
; + if/then/else
; - Fix

(define (gather-tvars e)
  (unique (gather-tvars-1 e)))
(define (gather-tvars-1 e)
  (mtch e
    ('T e t)
      (append (gather-tvars-1 e) (gather-tvars-1 t))
    ('C c)
      '()
    ('TV a)
      `(,e)
    ('PT c targs)
      (apply append (map gather-tvars-1 targs))
    ('L v body)
      (append (gather-tvars-1 v) (gather-tvars-1 body))
    ('A fun arg)
      (append (gather-tvars-1 fun) (gather-tvars-1 arg))
    ('Fix fun)
      (gather-tvars-1 fun)
    ('If b th el)
      (append (gather-tvars-1 b) (gather-tvars-1 th) (gather-tvars-1 el))
    ('V x)
      '()
    ('K k)
      '()))

(define (gen-inst-substitution tvars)
  (map (lambda (tv) `(,tv (TV ,(ty)))) tvars))

(define (instantiate-poly t)
  (mtch t
    ('Forall vs t)
      (apply-unifiers-to-type-term (gen-inst-substitution vs) t)
    t
      t))

;(tracefun gen-inst-substitution instantiate-poly)

(define (env-lookup-and-inst x env)
  (mtch (assoc x env)
    (x . t)
      (instantiate-poly t)))

(define (tinf e)
  (tinf0 e initial-type-env '()))

; (exp, env, unis) -> (typed-exp, unis)
(define (tinf0 e env unis)
  (mtch e
    ('L ('V var) body)
      (let ((var-t (ty)))
        (mtch (tinf0 body `((,var . (TV ,var-t)) . ,env) unis)
          (('T body body-t) unis)
            `((T (L (T (V ,var) (TV ,var-t)) (T ,body ,body-t)) (PT Fun ((TV ,var-t) ,body-t))) ,unis)))
    ('A fun arg)
      (let ((result-t (ty)))
        (mtch (tinf0 fun env unis)
          (('T fun fun-t) unis)
            (mtch (tinf0 arg env unis)
              (('T arg arg-t) unis)
                `((T (A (T ,fun ,fun-t) (T ,arg ,arg-t)) (TV ,result-t)) ((,fun-t (PT Fun (,arg-t (TV ,result-t)))) . ,unis)))))
    ('Fix fun)
      (mtch (tinf0 fun env unis)
        (('T fun ('PT 'Fun (a b))) unis)
          `((T (Fix (T ,fun (PT Fun (,a ,b)))) ,a) ;; Could also be b since a == b
            ((,a ,b)
            . ,unis)))
    ('If b th el)
      (let ((result-t (ty)))
        (mtch (tinf0 b env unis)
          (('T b b-t) unis)
            (mtch (tinf0 th env unis)
              (('T th th-t) unis)
                (mtch (tinf0 el env unis)
                  (('T el el-t) unis)
                    `((T (If (T ,b ,b-t) (T ,th ,th-t) (T ,el ,el-t)) (TV ,result-t))
                      (((TV ,result-t) ,th-t)
                       ((TV ,result-t) ,el-t)
                       (,b-t (C Bool))
                       . ,unis))))))
    ('V x)
      `((T ,e ,(env-lookup-and-inst x env)) ,unis)
    ('K k)
      `((T ,e ,(get-constant-type k)) ,unis)))
;(tracefun tinf0)

(define (ut a b)
  (if (not (equal? a b))
    (begin
      (shew a b)
      (err 'test-failure))
    '()))

(ut 1 1)
;(ut 1 2)

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
    ;('PT 'Fun (a b))
      ;`(PT Fun (,(apply-unifiers-to-type-term unifiers a) ,(apply-unifiers-to-type-term unifiers b)))
    ('PT tc targs)
      `(PT ,tc ,(map (lambda (t) (apply-unifiers-to-type-term unifiers t)) targs))
    ))
;(tracefun apply-unifiers apply-unifiers-to-type-term )
;(tracefun apply-unifiers-to-type-term )

(define (apply-unifiers-to-term unifiers e)
  (mtch e
    ('T e t)
      `(T ,(apply-unifiers-to-term unifiers e) ,(apply-unifiers-to-type-term unifiers t))
    ('L v body)
      `(L ,(apply-unifiers-to-term unifiers v) ,(apply-unifiers-to-term unifiers body))
    ('A fun arg)
      `(A ,(apply-unifiers-to-term unifiers fun) ,(apply-unifiers-to-term unifiers arg))
    ('Fix fun)
      `(Fix ,(apply-unifiers-to-term unifiers fun))
    ('If b th el)
      `(If ,(apply-unifiers-to-term unifiers b) ,(apply-unifiers-to-term unifiers th) ,(apply-unifiers-to-term unifiers el))
    ('V x)
      e
    ('K k)
      e))

;(tracefun apply-unifiers-to-term)

(define (ec-set-make) '())

(define (ec-set-check s)
  (assert (ec-set-ok s)))
(define (ec-set-ok s)
  (let ((all-elements (apply append s)))
    (equal? all-elements (unique all-elements))))

(assert (ec-set-ok '((a z) (b c) (d y))))
(assert (not (ec-set-ok '((a z) (b c a) (d y)))))

(define (ec-set-index-of s e) (ec-set-index-of-1 s e 0))
(define (ec-set-index-of-1 s e i)
  (mtch s
    (a . d)
      (if (member e a)
        i
        (ec-set-index-of-1 d e (+ i 1)))
    '()
      #f))

(ut 1
    (ec-set-index-of '((a) (b c) (d)) 'c))
(ut #f
    (ec-set-index-of '((a) (b c) (d)) 'e))

(define (ec-set-add-to-ec s i e)
  (if (eq? i 0)
    (cons (append (car s) (list e)) (cdr s))
    (cons (car s) (ec-set-add-to-ec (cdr s) (- i 1) e))))

(ut '((a) (b d) (c))
    (ec-set-add-to-ec '((a) (b) (c)) 1 'd))

(define (ec-set-add-element s e)
  (let ((index (ec-set-index-of s e)))
    (if (eq? index #f)
      (append s `((,e)))
      s)))
      ;(ec-set-add-to-ec s index e))))

(ut '((a) (b) (c))
    (ec-set-add-element '((a) (b) (c)) 'b))
(ut '((a) (b) (c) (d))
    (ec-set-add-element '((a) (b) (c)) 'd))

(define (ec-combine-ecs s ai bi)
  (assert (not (eq? ai bi)))
  ;(shew 'yeah ai bi (number-elements s))
  (mtch (divide-by-pred (lambda (p) (or (eq? (car p) ai) (eq? (car p) bi))) (number-elements s))
    (match . no-match)
      (begin
      ;(shew 'haha match no-match)
        (assert (eq? 2 (length match)))
        (let ((match (map (lambda (x) (mtch x (i . x) x)) match))
              (no-match (map (lambda (x) (mtch x (i . x) x)) no-match)))
          (append no-match (mtch match (a b) (list (append a b))))))))
;(tracefun ec-combine-ecs)

(ut '((a) (d) (b c))
    (ec-combine-ecs '((a) (b) (c) (d)) 1 2))

(define (ec-set-add s a b)
  (let ((s (ec-set-add-element (ec-set-add-element s a) b)))
    (let ((a-index (ec-set-index-of s a))
          (b-index (ec-set-index-of s b)))
      (if (not (eq? a-index b-index))
        (ec-combine-ecs s a-index b-index)
        s))))

(ut '((a b))
    (ec-set-add (ec-set-make) 'a 'b))
(ut '((b a))
    (ec-set-add (ec-set-add-element (ec-set-make) 'b) 'a 'b))
(ut '((a b))
    (ec-set-add (ec-set-add-element (ec-set-make) 'a) 'a 'b))
(ut '((c) (a b))
    (ec-set-add '((a) (c)) 'a 'b))
(ut '((a) (c b))
    (ec-set-add '((a) (c)) 'c 'b))

(define (lshew-type t)
  (mtch t
    ('C c)
      (->string c)
    ('TV v)
      (->string v)
    ('PT 'Fun (a b))
      (++ "(" (lshew-type a) " -> " (lshew-type b) ")")
    ('PT c targs)
      (++ "(" (join-things " " (cons c (map lshew-type targs))) ")")
    ('Forall vars t)
      (++ "Forall " (join-things " " (map lshew-type vars)) " . " (lshew-type t))
      ))
;(tracefun lshew-type)

(define (lshew-eqns eqns)
  (if (eq? eqns '())
    "(empty)"
    (join-things "\n" (map (lambda (eqn) (mtch eqn (a b) (++ (lshew-type a) " = " (lshew-type b)))) eqns))))
(define (shew-eqns eqns) (display (lshew-eqns eqns)) (display "\n"))

(define (lshew-ecs ecs)
  (if (eq? ecs '())
    "(empty)"
    (join-things "\n"
      (map (lambda (ec) (join-things " = " (map lshew-type ec))) ecs))))
(define (shew-ecs ecs) (display (lshew-ecs ecs)) (display "\n"))

;; eqns -> ecs (not unified)
(define (unify-create-initial-ec-set eqns) (unify-create-initial-ec-set-1 eqns (ec-set-make)))
(define (unify-create-initial-ec-set-1 eqns ecs)
  (mtch eqns
    ((a b) . d)
      (unify-create-initial-ec-set-1 d (ec-set-add ecs a b))
    '()
      ecs))
;(tracefun unify-create-initial-ec-set unify-create-initial-ec-set-1)

;; One set of all unordered pairs of equal items
;; ecs -> eqns
(define (unify-get-ec-set-all-pairs ecs)
  (apply append (map all-pairs ecs)))
  ;(map all-pairs ecs))

;; Get equal sub-parts from equal terms
(define (unify-get-sub-eqns eqn)
  (mtch eqn
    ;(('PT 'Fun (a b)) ('PT 'Fun (c d)))
      ;`((,a ,c) (,b ,d))
    (('PT ctor-a targs-a) ('PT ctor-b targs-b))
      (begin
        (assert (eq? ctor-a ctor-b) eqn)
        (assert (eq? (length targs-a) (length targs-b)))
        (zip targs-a targs-b))
    x
      '()))

;; Get all equal sub parts from all possible equal pairs
(define (unify-get-ec-all-sub-eqns ecs)
  (apply append (map unify-get-sub-eqns (unify-get-ec-set-all-pairs ecs))))

(define (unify-ec-set-add-eqns ecs eqns)
  (mtch eqns
    ((a b) . d)
      (unify-ec-set-add-eqns (ec-set-add ecs a b) d)
    '()
      ecs))

;; ecs -> ecs
(define (unify-dive-one-step ecs)
  ;(shew 'step 'before ecs)
  (let ((sub-eqns (unify-get-ec-all-sub-eqns ecs)))
    ;(shew 'sub-eqns sub-eqns)
    (let ((added (unify-ec-set-add-eqns ecs sub-eqns)))
      ;(shew 'added)
      ;(shew-ecs added)
      added)))

(define (apply-subs-to-subs-rhs subs)
  (map (lambda (sub) (mtch sub
    (a b)
      `(,a ,(apply-unifiers-to-type-term subs b))))
    subs))

; speed: this still generates all the subs for an ec, but we only need one
(define (infer-find-a-sub ecs)
  (mtch (find-first-maybe unify-ec-is-vars-n-1-thing ecs)
    (v1-ec)
      `(,(unify-v1-ec-to-subs v1-ec))
    #f
      (mtch (find-first-maybe unify-ec-is-vars-only ecs)
        (vo-ec)
          `(,(unify-vo-ec-to-subs vo-ec))
        #f
          #f)))
;(tracefun infer-find-a-sub)

(define (infer-subs eqns)
  (apply-subs-to-subs-rhs (infer-subs-1 (unify-create-initial-ec-set eqns))))

(define (infer-subs-1 ecs)
  (shew 'infer-subs-1)
  (shew-ecs ecs)
  (assert (ec-set-ok ecs))
  (assert (not (unify-ecs-is-type-error ecs)))
  (if (null? ecs)
    '()
    (let ((dove (apply-until-fixpoint unify-dive-one-step ecs)))
      (shew 'dove)
      (shew-ecs dove)
      (mtch (infer-find-a-sub dove)
        ((sub . rest))
          (begin
            (shew 'sub)
            (shew sub)
            (shew-eqns (list sub))
            (let ((applied (unify-apply-subs2 dove (list sub))))
              (shew 'applied)
              (shew-ecs applied)
              (let ((remove-singletons (grep (lambda (ec) (> (length ec) 1)) applied)))
                (cons sub (infer-subs-1 remove-singletons)))))))))

(define (infer-types e)
  (shew 'START)
  (set! ty (make-type-symgen))
  (shew e)
  (mtch (tinf e)
    (typed-exp eqns)
      (begin
        (shew typed-exp)
        (let ((all-subs (infer-subs eqns)))
          (shew 'all-subs)
          (shew-eqns all-subs)
          (let ((typed-term-subbed (apply-unifiers-to-term all-subs typed-exp)))
            (shew 'typed-term-subbed typed-term-subbed)
            (let ((quantified (quantify-type typed-term-subbed)))
              (shew 'quantified quantified)
                quantified))))))
          ;(unify-big-check unified)
          ;(assert (not (unify-ecs-is-type-error unified)))

;; Maybe doesn't need to be done via the pairs route.
(define (unify-apply-subs2 ecs subs)
  (let ((applied
    (unify-map-over-subs-types
      (lambda (e) (apply-unifiers-to-type-term subs e))
      (unify-get-ec-set-all-pairs ecs))))
    ;(shew 'before-thing)
    ;(shew-eqns applied)
    (unify-create-initial-ec-set applied)))

;(define (type-is-constant t) (mtch t ('C c) #t x #f))
(define (type-is-var t) (mtch t ('TV v) #t x #f))
;(define (type-is-fun t) (mtch t ('PT 'Fun (a b)) #t x #f))

(define (type-is-monotype t)
  (mtch t
    ('C c)
      #t
    ('TV v)
      #f
    ;('PT 'Fun (a b))
      ;(and (type-is-monotype a) (type-is-monotype b))
    ('PT ctor targs)
      (all? (map type-is-monotype targs))
      ))

(define (unify-ec-is-type-error ec)
  (> (length (grep type-is-monotype ec)) 1))
(define (unify-ecs-is-type-error ecs)
  (any? (map unify-ec-is-type-error ecs)))

; Just vars and one other thing not a var
(define (unify-ec-is-vars-n-1-thing ec)
  (and (> (length ec) 1)
    (eq? (- (length ec) 1)
         (length (grep type-is-var ec)))))
;(tracefun unify-ec-is-vars-n-1-thing)

(define (unify-v1-ec-to-subs ec)
  (mtch (divide-by-pred type-is-var ec)
    (vs . (t))
      (begin
        (map (lambda (v) `(,v ,t)) vs))))
;(tracefun unify-v1-ec-to-subs)

(define (unify-ec-is-vars-only ec)
  (all? (map type-is-var ec)))

(define (unify-vo-ec-to-subs ec)
  (map (lambda (v) `(,v ,(car ec))) (cdr ec)))

;(tracefun unify-ec-is-vars-n-mono unify-ec-is-vars-only)
;(tracefun unify-v1-ec-to-subs unify-vo-ec-to-subs)

(define (unify-map-over-subs-types f subs)
  (map (lambda (sub) (mtch sub (a b) `(,(f a) ,(f b)))) subs))

;; te -> ((TV a)...)
;; Since we aren't even thinking of higher-rank, this just returns all the TVs in the expression.
(define (free-type-vars te)
  (gather-tvars te))

;; te -> te
;; This just quantifies the type of the top-level expression, ignoring everything inside
(define (quantify-type te)
  (mtch te
    ('T e t)
      (let ((ftv (free-type-vars t)))
        (if (eq? ftv '())
          te
          `(T ,e (Forall ,(free-type-vars t) ,t))))))

(define (just-type e)
  (mtch (infer-types e) ('T e t) t))

(define unify-tests
  '(
    ; /. f /. x (f x) + x
    ; (Int -> Int) -> Int -> Int
    ((L (V f) (L (V x) (A (A (V +) (A (V f) (V x))) (V x))))
     (PT Fun ((PT Fun ((C Int) (C Int))) (PT Fun ((C Int) (C Int))))))

    ; (/. f /. x (f x) + x) (/. x + 1) 2
    ; Int
    ((A
       (A (L (V f) (L (V x) (A (A (V +) (A (V f) (V x))) (V x))))
          (L (V x) (A (A (V +) (V x)) (K 1))))
       (K 2))
     (C Int))

    ; /. x x
    ; a -> a
    ((L (V x) (V x))
     (Forall ((TV a)) (PT Fun ((TV a) (TV a)))))

    ; /. f /. x f (f x)
    ; (a -> a) -> a -> a
    ((L (V f) (L (V x) (A (V f) (A (V f) (V x)))))
     (Forall ((TV d)) (PT Fun ((PT Fun ((TV d) (TV d))) (PT Fun ((TV d) (TV d)))))))

    ; /. x /. y y
    ; a -> b -> b
    ((L (V x) (L (V y) (V y)))
     (Forall ((TV a) (TV b)) (PT Fun ((TV a) (PT Fun ((TV b) (TV b)))))))

    ; /. x /. y x
    ; a -> b -> a
    ((L (V x) (L (V y) (V x)))
     (Forall ((TV b) (TV a)) (PT Fun ((TV a) (PT Fun ((TV b) (TV a)))))))

    ((V +)
     (PT Fun ((C Int) (PT Fun ((C Int) (C Int))))))
    ((K 3)
     (C Int))
    ((K #t)
     (C Bool))

    ((A (A (V Cons) (K 1)) (V Nil))
     (PT List ((C Int))))
    ((A (V car) (A (A (V Cons) (K 1)) (V Nil)))
     (C Int))
    ((A (V cdr) (A (A (V Cons) (K 1)) (V Nil)))
     (PT List ((C Int))))

    ((If (K #t) (K 1) (K 2))
     (C Int))

    ; /. a /. b if a == b then a else b
    ; a -> a -> a
    ((L (V a) (L (V b) (If (A (A (V ==) (V a)) (V b)) (V a) (V b))))
     (Forall ((TV c)) (PT Fun ((TV c) (PT Fun ((TV c) (TV c)))))))

    ; Fix /. rec /. f /. xs /. z if (xs == []) z else (f (car xs) (rec f (cdr xs) z))
    ((Fix (L (V rec) (L (V f) (L (V xs) (L (V z)
            (If (A (A (V ==) (V xs)) (V Nil))
              (V z)
              (A (A (V f) (A (V car) (V xs))) (A (A (A (V rec) (V f)) (A (V cdr) (V xs))) (V z)))))))))
     (Forall ((TV m) (TV e))
         (PT Fun ((PT Fun ((TV m) (PT Fun ((TV e) (TV e)))))
                  (PT Fun ((PT List ((TV m))) (PT Fun ((TV e) (TV e)))))))))

    ; map = /. f /. x if (x == Nil) then Nil else Cons (f (car x)) (map f (cdr x))
    ; (a -> b) -> List a -> List b
    ; map = /. rec /. f /. x if (x == Nil) then Nil else Cons (f (car x)) (rec f (cdr x))
    ; ((a -> b) -> List a -> List b) -> (a -> b) -> List a -> List b
    ((Fix (L (V rec) (L (V f) (L (V x)
            (If (A (A (V ==) (V x)) (V Nil))
                (V Nil)
                (A (A (V Cons) (A (V f) (A (V car) (V x))))
                   (A (A (V rec) (V f)) (A (V cdr) (V x)))))))))
     (Forall
       ((TV o) (TV l))
         (PT
            Fun
               ((PT Fun ((TV o) (TV l)))
                   (PT Fun ((PT List ((TV o))) (PT List ((TV l)))))))))
   ))

(define (run-unify-tests)
  (map (lambda (test) (mtch test
    (src expected)
      (let ((actual (just-type src)))
        (shew 'test src expected actual (equal? expected actual))
        (assert (equal? expected (just-type src))))))
    unify-tests))

(define foo '(L (V f) (L (V x) (A (A (V +) (A (V f) (V x))) (V x)))))

(define (main)
  (run-unify-tests)
  (let ((t (just-type foo)))
    (shew t (lshew-type (just-type foo)))))
