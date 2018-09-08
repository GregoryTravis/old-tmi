(dload "lib.ss")
(dload "mtch.ss")

(reset-tracefun)

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

#|
; (type ((typector T) (var a) ...) (pat pat ...))
; (pat exp)    ;; Except pats don't have apps
; (body exp)
; (exp (app exps) | (cton exps) | (var id) | (ctor C))
(define types
  '((type ((typector List) (var a))
          ((pat (cton ((ctor Cons) (var a) (type ((typector List) (var a))))))
           (pat (cton ((ctor Nil))))))))
|#

(define initial-type-env
  '(
    ; int -> int -> int
    (+ . (PT Fun ((C Int) (PT Fun ((C Int) (C Int))))))
    ; int -> int -> int
    (* . (PT Fun ((C Int) (PT Fun ((C Int) (C Int))))))
    ; int -> int -> int
    (- . (PT Fun ((C Int) (PT Fun ((C Int) (C Int))))))
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
    ; Tuple2 a b = Tuple2 a b
    (Tuple2 . (Forall ((TV a) (TV b)) (PT Fun ((TV a) (PT Fun ((TV b) (PT Tuple2 ((TV a) (TV b)))))))))
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
    ('PL p body)
      (append (gather-tvars-1 p) (gather-tvars-1 body))
    ('ML ls)
      (apply append (map gather-tvars-1 ls))
    ('A fun arg)
      (append (gather-tvars-1 fun) (gather-tvars-1 arg))
    ('PA fun arg)
      (append (gather-tvars-1 fun) (gather-tvars-1 arg))
    ('Fix fun)
      (gather-tvars-1 fun)
    ('Fixn i ('PT 'FixList funs))
      (apply append (map gather-tvars-1 funs))
    ('If b th el)
      (append (gather-tvars-1 b) (gather-tvars-1 th) (gather-tvars-1 el))
    ('PV x)
      '()
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
;(tracefun env-lookup-and-inst)

(define (tinf e type-env)
  (tinf0 e type-env '()))

;; Infer types of a list of expressions, returning a list of types
;; and the final unis.  Thread the uni through each individual inference.
;; es -> env -> unis -> (tes, unis)
(define (tinf0* es env unis)
  (mtch es
    (e . es)
      (mtch (tinf0 e env unis)
        (te env unis)
          (mtch (tinf0* es env unis)
            (tes env unis)
              `((,te . ,tes) ,env ,unis)))
    '()
      `(() ,env ,unis)))

;; Generate necessary unifiers for a set of typed Fixn open-recursive forms.
;; Each of the a .. e are actually function types.
;; f :: a -> b -> ... -> e -> a
;; g :: a -> b -> ... -> e -> b
;; ...
;; j :: a -> b -> ... -> e -> e
;; Returns (generated-tvs unis)
(define (unifiers-for-fixn-open-recs tes)
  (let ((types (map (lambda (e) (mtch e ('T e t) t)) tes)))
    (let ((tvs (map (lambda (tv) `(TV ,tv)) (ntimes-f (length types) ty))))
      (let ((form-ts (map (lambda (tv) (generate-fun-type tvs tv)) tvs)))
        (shew 'types (map lshew-type types))
        (shew 'form-ts (map lshew-type form-ts))
        (shew 'eqs)
        (display (lshew-eqns (zip types form-ts)))
        (display "\n")
        `(,tvs ,(zip types form-ts))))))

(define (generate-fun-type arg-ts ret-t)
  (mtch arg-ts
    (a-t . d-t)
      `(PT Fun (,a-t ,(generate-fun-type d-t ret-t)))
    '()
      ret-t))

;(tracefun tinf0*)
;(tracefun generate-fun-type unifiers-for-fixn-open-recs)

;; Generate pairwise unifiers for a set of things that are all equal.
;; (a b c d) -> ((a b) (b c) (c d))
(define (multi-uni-gen ts)
  (mtch ts
    (a b . d)
      `((,a ,b) . ,(multi-uni-gen `(,b . ,d)))
    (a)
      '()))

; lambdas env unis -> (typed-lambdas env unis)
(define (tinf0-multilambda-lambas ls env unis)
  (mtch ls
    (lsa . lsd)
      (mtch (tinf0 lsa env unis)
        (('T lsa lsa-t) nenv unis)
          (mtch (tinf0-multilambda-lambas lsd env unis)
            (tls nenv unis)
              `(((T ,lsa ,lsa-t) . ,tls) ,env ,unis)))
    '()
      `(() ,env ,unis)))
;(tracefun tinf0-multilambda-lambas)

; (exp env unis) -> (typed-exp env unis)
(define (tinf0 e env unis)
  (mtch e
    ('L ('V var) body)
      (let ((var-t (ty)))
        (mtch (tinf0 body `((,var . (TV ,var-t)) . ,env) unis)
          (('T body body-t) nenv unis)
            `((T (L (T (V ,var) (TV ,var-t)) (T ,body ,body-t)) (PT Fun ((TV ,var-t) ,body-t)))
              ,env
              ,unis)))
    ('PL pat body)
      (mtch (tinf0 pat env unis)
        (('T pat pat-t) nenv unis)
          (mtch (tinf0 body nenv unis)
            (('T body body-t) nenv unis)
              `((T (PL (T ,pat ,pat-t) (T ,body ,body-t))
                   (PT Fun (,pat-t ,body-t)))
                ,env
                ,unis)))
    ('ML ls)
      (let ((ml-t (ty)))
        (mtch (tinf0-multilambda-lambas ls env unis)
          (tls nenv unis)
            (let ((ls-types (map (lambda (tl) (mtch tl ('T l l-t) l-t)) tls)))
              `((T (ML ,tls) (TV ,ml-t))
                ,env
                ,(append
                  (multi-uni-gen (cons `(TV ,ml-t) ls-types))
                  unis)))))
    ('A fun arg)
      (let ((result-t (ty)))
        (mtch (tinf0 fun env unis)
          (('T fun fun-t) nenv unis)
            (mtch (tinf0 arg env unis)
              (('T arg arg-t) nenv unis)
                `((T (A (T ,fun ,fun-t) (T ,arg ,arg-t)) (TV ,result-t))
                  ,env
                  ((,fun-t (PT Fun (,arg-t (TV ,result-t)))) . ,unis)))))
    ('PA fun arg)
      (let ((result-t (ty)))
        (mtch (tinf0 fun env unis)
          (('T fun fun-t) env unis)
            (mtch (tinf0 arg env unis)
              (('T arg arg-t) env unis)
                `((T (PA (T ,fun ,fun-t) (T ,arg ,arg-t)) (TV ,result-t))
                  ,env
                  ((,fun-t (PT Fun (,arg-t (TV ,result-t)))) . ,unis)))))
    ('Fix fun)
      (mtch (tinf0 fun env unis)
        (('T fun ('PT 'Fun (a b))) env unis)
          `((T (Fix (T ,fun (PT Fun (,a ,b)))) ,a) ;; Could also be b since a == b
            ,env
            ((,a ,b)
            . ,unis)))

    ;; f :: (a -> b) -> (c -> d) -> (a -> b)
    ;; g :: (a -> b) -> (c -> d) -> (c -> d)
    ;; Fixn 0 (f g) :: a -> b
    ;; Fixn 1 (f g) :: c -> d
    ('Fixn ('K i) (PT FixList funs))
      (mtch (tinf0* funs env unis)
        (tes env unis)
          (mtch (unifiers-for-fixn-open-recs tes)
            (result-ts new-unis)
              `((T (Fixn (K ,i) (PT FixList ,tes)) ,(nth i result-ts))
                ,env
                ,(append new-unis unis))))

    ('If b th el)
      (let ((result-t (ty)))
        (mtch (tinf0 b env unis)
          (('T b b-t) env unis)
            (mtch (tinf0 th env unis)
              (('T th th-t) env unis)
                (mtch (tinf0 el env unis)
                  (('T el el-t) env unis)
                    `((T (If (T ,b ,b-t) (T ,th ,th-t) (T ,el ,el-t)) (TV ,result-t))
                      ,env
                      (((TV ,result-t) ,th-t)
                       ((TV ,result-t) ,el-t)
                       (,b-t (C Bool))
                       . ,unis))))))
    ('V x)
      `((T ,e ,(env-lookup-and-inst x env)) ,env ,unis)
    ('PV x)
      (let ((x-t (ty)))
        `((T ,e (TV ,x-t)) ((,x . (TV ,x-t)) . ,env) ,unis))
    ('K k)
      `((T ,e ,(get-constant-type k)) ,env ,unis)))
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
    ('PL p body)
      `(PL ,(apply-unifiers-to-term unifiers p) ,(apply-unifiers-to-term unifiers body))
    ('ML ls)
      `(ML ,(map (lambda (t) (apply-unifiers-to-term unifiers t)) ls))
    ('A fun arg)
      `(A ,(apply-unifiers-to-term unifiers fun) ,(apply-unifiers-to-term unifiers arg))
    ('PA fun arg)
      `(PA ,(apply-unifiers-to-term unifiers fun) ,(apply-unifiers-to-term unifiers arg))
    ('Fix fun)
      `(Fix ,(apply-unifiers-to-term unifiers fun))
    ('Fixn i ('PT 'FixList funs))
      `(Fixn ,i (PT FixList ,(map (lambda (t) (apply-unifiers-to-term unifiers t)) funs)))
    ('If b th el)
      `(If ,(apply-unifiers-to-term unifiers b) ,(apply-unifiers-to-term unifiers th) ,(apply-unifiers-to-term unifiers el))
    ('V x)
      e
    ('PV x)
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

;; Add st as an equivalence class, merging with another if there
;; are any elements in common
(define (ec-set-add-set s st)
  (mtch st
    (a b . d)
      (ec-set-add-set (ec-set-add s a b) `(,b . ,d))
    (a)
      (ec-set-add-element s a)))
(define (ec-set-add-sets s sts)
  (mtch sts
    '()
      s
    (a . d)
      (ec-set-add-sets (ec-set-add-set s a) d)))

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

(define (infer-types e type-env)
  (shew 'START)
  (set! ty (make-type-symgen))
  (shew e)
  (mtch (tinf e type-env)
    (typed-exp env eqns)
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
  (and (all? (map type-is-var ec))
       (> (length ec) 1)))

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

(define (testpred-closure x) (mtch x ('Closure . _) #t))
(define (testpred-native x) (mtch x ('Native . _) #t))

(define test-program
  `(
    ;;#|
    ; /. f /. x (f x) + x
    ; (Int -> Int) -> Int -> Int
    (foo (L (V f) (L (V x) (A (A (V +) (A (V f) (V x))) (V x))))
      (PT Fun ((PT Fun ((C Int) (C Int))) (PT Fun ((C Int) (C Int)))))
      ,testpred-closure)

    ; (/. f /. x (f x) + x) (/. x + 1) 2
    ; Int
    (fooa (A (A (V foo) (L (V x) (A (A (V +) (V x)) (K 1)))) (K 2))
      (C Int)
      5)

    ; /. x x
    ; a -> a
    (id (L (V x) (V x))
     (Forall ((TV a)) (PT Fun ((TV a) (TV a)))) ,testpred-closure)

    ; (/. x x) 44
    ; int
    (ida (A (V id) (K 44))
     (C Int) 44)

    ; /. f /. x f (f x)
    ; (a -> a) -> a -> a
    (dapp (L (V f) (L (V x) (A (V f) (A (V f) (V x)))))
     (Forall ((TV d)) (PT Fun ((PT Fun ((TV d) (TV d))) (PT Fun ((TV d) (TV d)))))) ,testpred-closure)

    ; (/. f /. x f (f x)) (/. x x + x) 13
    ; 52
    (dappa (A (A (V dapp)
           (L (V x) (A (A (V +) (V x)) (V x))))
        (K 13))
     (C Int) 52)

    ; /. x /. y y
    ; a -> b -> b
    (xyy (L (V x) (L (V y) (V y)))
     (Forall ((TV a) (TV b)) (PT Fun ((TV a) (PT Fun ((TV b) (TV b)))))) ,testpred-closure)

    ; (/. x /. y y) 1 2
    ; 2
    (xxya (A (A (V xyy) (K 1)) (K 2))
     (C Int) 2)

    ; /. x /. y x
    ; a -> b -> a
    (xyx (L (V x) (L (V y) (V x)))
     (Forall ((TV b) (TV a)) (PT Fun ((TV a) (PT Fun ((TV b) (TV a)))))) ,testpred-closure)

    ; (/. x /. y x) 1 2
    ; 1
    (xyxa (A (A (V xyx) (K 1)) (K 2))
     (C Int) 1)

    (plus (V +)
     (PT Fun ((C Int) (PT Fun ((C Int) (C Int))))) ,testpred-native)
    (three (K 3)
     (C Int) 3)
    (troo (K #t)
     (C Bool) #t)
    (fals (K #f)
     (C Bool) #f)

    (cns (A (A (V Cons) (K 1)) (V Nil))
     (PT List ((C Int))) (Cons 1 Nil))
    (carcns (A (V car) (V cns))
     (C Int) 1)
    (cdrcns (A (V cdr) (V cns))
     (PT List ((C Int))) Nil)

    (ift (If (K #t) (K 1) (K 2))
     (C Int) 1)
    (iff (If (K #f) (K 1) (K 2))
     (C Int) 2)

    (eqt (A (A (V ==) (K 1)) (K 1))
     (C Bool) #t)
    (eqf (A (A (V ==) (K 1)) (K 2))
     (C Bool) #f)

    ; /. a /. b if a == b then a else b
    ; a -> a -> a
    (ifab (L (V a) (L (V b) (If (A (A (V ==) (V a)) (V b)) (V a) (V b))))
     (Forall ((TV c)) (PT Fun ((TV c) (PT Fun ((TV c) (TV c)))))) ,testpred-closure)

    ; (/. a /. b if a == b then a else b) 1 2
    ; 2
    (ifaban (A (A (V ifab) (K 1)) (K 2))
     (C Int) 2)

    ; (/. a /. b if a == b then a else b) 1 1
    ; 1
    (ifabae (A (A (V ifab) (K 1)) (K 1))
     (C Int) 1)

    ; Fix /. rec /. f /. xs /. z if (xs == []) z else (f (car xs) (rec f (cdr xs) z))
    (fold (Fix (L (V rec) (L (V f) (L (V xs) (L (V z)
            (If (A (A (V ==) (V xs)) (V Nil))
              (V z)
              (A (A (V f) (A (V car) (V xs))) (A (A (A (V rec) (V f)) (A (V cdr) (V xs))) (V z)))))))))
     (Forall ((TV m) (TV e))
         (PT Fun ((PT Fun ((TV m) (PT Fun ((TV e) (TV e)))))
                  (PT Fun ((PT List ((TV m))) (PT Fun ((TV e) (TV e)))))))) ,testpred-closure)

    (d1234 (A (A (V Cons) (K 1)) (A (A (V Cons) (K 2)) (A (A (V Cons) (K 3)) (A (A (V Cons) (K 4)) (V Nil)))))
      (PT List ((C Int)))
      (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))

    ; fold = fix + ns 0
    (folda (A (A (A (V fold)
              (V +))
           (V d1234))
        (K 0))
      (C Int) 10)

    (ml-fold (Fix (L (V rec) (L (V f) (ML ((PL (PA (PA (V Cons) (PV a)) (PV d))
                                              (L (V z) (A (A (V f) (V a)) (A (A (A (V rec) (V f)) (V d)) (V z)))))
                                          (PL (PV Nil)
                                              (L (V z) (V z))))))))
      (Forall ((TV f) (TV i))
          (PT Fun ((PT Fun ((TV f) (PT Fun ((TV i) (TV i)))))
                   (PT Fun ((PT List ((TV f))) (PT Fun ((TV i) (TV i)))))))) ,testpred-closure)

    ; fold = fix + ns 0
    (ml-folda (A (A (A (V ml-fold)
              (V +))
           (V d1234))
        (K 0))
      (C Int) 10)

    ; map = /. f /. x if (x == Nil) then Nil else Cons (f (car x)) (map f (cdr x))
    ; (a -> b) -> List a -> List b
    ; map = /. rec /. f /. x if (x == Nil) then Nil else Cons (f (car x)) (rec f (cdr x))
    ; ((a -> b) -> List a -> List b) -> (a -> b) -> List a -> List b
    (map (Fix (L (V rec) (L (V f) (L (V x)
            (If (A (A (V ==) (V x)) (V Nil))
                (V Nil)
                (A (A (V Cons) (A (V f) (A (V car) (V x))))
                   (A (A (V rec) (V f)) (A (V cdr) (V x)))))))))
     (Forall
       ((TV o) (TV l))
         (PT
            Fun
               ((PT Fun ((TV o) (TV l)))
                   (PT Fun ((PT List ((TV o))) (PT List ((TV l)))))))) ,testpred-closure)

    ; map (/. x x + x) ns
    (mapa (A (A (V map)
           (L (V x) (A (A (V +) (V x)) (V x))))
        (V d1234))
     (PT List ((C Int))) (Cons 2 (Cons 4 (Cons 6 (Cons 8 Nil))))) 

    (ml-map (Fix (L (V rec) (L (V f)
        (ML ((PL (PA (PA (V Cons) (PV a)) (PV d))
                 (A (A (V Cons) (A (V f) (V a))) (A (A (V rec) (V f)) (V d))))
             (PL (V Nil) (V Nil)))))))
      (Forall
        ((TV f) (TV k))
          (PT
             Fun
                ((PT Fun ((TV f) (TV k)))
                    (PT Fun ((PT List ((TV f))) (PT List ((TV k)))))))) ,testpred-closure)

    ; map (/. x x + x) ns
    (ml-mapa (A (A (V ml-map)
           (L (V x) (A (A (V +) (V x)) (V x))))
        (V d1234))
      (PT List ((C Int))) (Cons 2 (Cons 4 (Cons 6 (Cons 8 Nil))))) 

    ; fact
    (fact (Fix (L (V rec)
       (L (V n) (If (A (A (V ==) (V n)) (K 0))
                    (K 1)
                    (A (A (V *) (V n)) (A (V rec) (A (A (V -) (V n)) (K 1))))))))
     (PT Fun ((C Int) (C Int))) ,testpred-closure)

    ; fact 10 baby
    (fact10 (A (V fact) (K 10))
     (C Int) 3628800)

    (ml-fact (Fix (L (V rec)
      (ML ((PL (K 0) (K 1))
           (PL (PV n) (A (A (V *) (V n)) (A (V rec) (A (A (V -) (V n)) (K 1)))))))))
      (PT Fun ((C Int) (C Int))) ,testpred-closure)

    ; fact 10 baby
    (ml-fact10 (A (V ml-fact) (K 10))
     (C Int) 3628800)

    (even-oprec
      (L (V even-rec)
        (L (V odd-rec)
          (L (V x) (If (A (A (V ==) (V x)) (K 0)) (K 1) (A (V odd-rec) (A (A (V -) (V x)) (K 1)))))))
      (Forall ((TV a))
        (PT Fun
          ((TV a)
           (PT Fun ((PT Fun ((C Int) (C Int))) (PT Fun ((C Int) (C Int)))))))) ,testpred-closure)
    (odd-oprec
      (L (V even-rec)
        (L (V odd-rec)
          (L (V x) (If (A (A (V ==) (V x)) (K 0)) (K 0) (A (V even-rec) (A (A (V -) (V x)) (K 1)))))))
      (Forall ((TV b))
        (PT Fun ((PT Fun ((C Int) (C Int)))
                 (PT Fun ((TV b) (PT Fun ((C Int) (C Int)))))))) ,testpred-closure)
    (even (Fixn (K 0) (PT FixList ((V even-oprec) (V odd-oprec))))
      (PT Fun ((C Int) (C Int))) ,testpred-closure)
    (odd (Fixn (K 1) (PT FixList ((V even-oprec) (V odd-oprec))))
      (PT Fun ((C Int) (C Int))) ,testpred-closure)

    (e4 (A (V even) (K 4))
      (C Int) 1)
    (e5 (A (V even) (K 5))
      (C Int) 0)
    (o4 (A (V odd) (K 4))
      (C Int) 0)
    (o5 (A (V odd) (K 5))
      (C Int) 1)

    ;; 3-way mutual recursion
    (mod3-rec
      (L (V mod3-rec)
        (L (V mod3-1-rec)
          (L (V mod3-2-rec)
            (L (V x)
              (If (A (A (V ==) (V x)) (K 0))
                (K 0)
                (A (V mod3-1-rec) (A (A (V -) (V x)) (K 1))))))))
      (Forall ((TV a) (TV c))
        (PT Fun ((TV a)
                 (PT Fun ((PT Fun ((C Int) (C Int)))
                          (PT Fun ((TV c)
                                   (PT Fun ((C Int) (C Int)))))))))) ,testpred-closure)
    (mod3-1-rec
      (L (V mod3-rec)
        (L (V mod3-1-rec)
          (L (V mod3-2-rec)
            (L (V x)
              (If (A (A (V ==) (V x)) (K 0))
                (K 1)
                (A (V mod3-2-rec) (A (A (V -) (V x)) (K 1))))))))
      (Forall ((TV a) (TV b))
        (PT Fun ((TV a)
                 (PT Fun ((TV b)
                          (PT Fun ((PT Fun ((C Int) (C Int)))
                                   (PT Fun ((C Int) (C Int)))))))))) ,testpred-closure)
    (mod3-2-rec
      (L (V mod3-rec)
        (L (V mod3-1-rec)
          (L (V mod3-2-rec)
            (L (V x)
              (If (A (A (V ==) (V x)) (K 0))
                (K 2)
                (A (V mod3-rec) (A (A (V -) (V x)) (K 1))))))))
      (Forall ((TV b) (TV c))
        (PT Fun ((PT Fun ((C Int) (C Int)))
                 (PT Fun ((TV b)
                          (PT Fun ((TV c)
                                   (PT Fun ((C Int) (C Int)))))))))) ,testpred-closure)
    (mod3 (Fixn (K 0) (PT FixList ((V mod3-rec) (V mod3-1-rec) (V mod3-2-rec))))
      (PT Fun ((C Int) (C Int))) ,testpred-closure)
    (mod3-1 (Fixn (K 1) (PT FixList ((V mod3-rec) (V mod3-1-rec) (V mod3-2-rec))))
      (PT Fun ((C Int) (C Int))) ,testpred-closure)
    (mod3-2 (Fixn (K 2) (PT FixList ((V mod3-rec) (V mod3-1-rec) (V mod3-2-rec))))
      (PT Fun ((C Int) (C Int))) ,testpred-closure)

    (mod3-5 (A (V mod3) (K 5))
      (C Int) 2)
    (mod3-6 (A (V mod3) (K 6))
      (C Int) 0)
    (mod3-7 (A (V mod3) (K 7))
      (C Int) 1)

    (plx (PL (PV x) (V x))
      (Forall ((TV a)) (PT Fun ((TV a) (TV a)))) ,testpred-closure)
    (plxa (A (PL (PV x) (V x)) (K 12))
      (C Int) 12)
    (coulder (PL (PA (PA (V Cons) (PV aa)) (PV dd)) (V dd))
      (Forall ((TV c)) (PT Fun ((PT List ((TV c))) (PT List ((TV c)))))) ,testpred-closure)
    (coulder-a (A (V coulder)
                  (A (A (V Cons) (K 1)) (A (A (V Cons) (K 2)) (V Nil))))
      (PT List ((C Int))) (Cons 2 Nil))
    ;(coulder-a-fail (A (V coulder) (V Nil))
      ;(PT List ((C Int))) (Cons 2 Nil))

    (ml0 (ML ((PL (PA (PA (V Cons) (PV aa)) (PV dd)) (K 13))
              (PL (PV Nil) (K 12))))
      (Forall ((TV d)) (PT Fun ((PT List ((TV d))) (C Int)))) ,testpred-closure)
    (ml0-a-p (A (V ml0) (A (A (V Cons) (K 1)) (V Nil)))
      (C Int) 13)
    (ml0-a-n (A (V ml0) (V Nil))
      (C Int) 12)

    (rebuild (Fix (L (V rec)
                    (ML ((PL (PA (PA (V Cons) (PV a)) (PV d)) (A (A (V Cons) (V a)) (A (V rec) (V d))))
                         (PL (PV Nil) (V Nil))))))
      (Forall ((TV j)) (PT Fun ((PT List ((TV j))) (PT List ((TV j)))))) ,testpred-closure)
    (rebuild-a (A (V rebuild) (V d1234))
      (PT List ((C Int))) (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))

    (scope0 (A (A (PL (PV a) (PL (PV b) (V a))) (K 10)) (K 11))
      (C Int) 10)
    (scope1 (A (A (PL (PV a) (PL (PV b) (V b))) (K 10)) (K 11))
      (C Int) 11)

    (ml-const (ML ((PL (K 1) (K 10))
                   (PL (K 2) (K 20))))
      (PT Fun ((C Int) (C Int))) ,testpred-closure)
    (ml-const-a-1 (A (V ml-const) (K 1))
      (C Int) 10)
    (ml-const-a-2 (A (V ml-const) (K 2))
      (C Int) 20)
    ;;|#

    ;; ml2a (a . d) () = 1
    ;; ml2a () (a . d) = 2
    (ml2a (ML ((PL (PA (PA (V Tuple2) (PA (PA (V Cons) (PV a)) (PV d))) (V Nil)) (K 1))
               (PL (PA (PA (V Tuple2) (V Nil)) (PA (PA (V Cons) (PV a)) (PV d))) (K 2))))
       (Forall ((TV h) (TV s))
         (PT Fun ((PT Tuple2 ((PT List ((TV h))) (PT List ((TV s))))) (C Int)))) ,testpred-closure)
       ;(Forall ((TV a)) (PT Fun ((PT List ((TV a))) (PT List ((TV a)))))) ,testpred-closure)
    (ml2a-s (L (V x) (L (V y) (A (V ml2a) (A (A (V Tuple2) (V x)) (V y)))))
      (Forall ((TV d) (TV e)) (PT Fun ((PT List ((TV d))) (PT Fun ((PT List ((TV e))) (C Int)))))) ,testpred-closure)
    (ml2a-sa (A (A (V ml2a-s) (A (A (V Cons) (K 1)) (V Nil))) (V Nil))
      (C Int) 1)
   ))

(define test-program-no-fix
  `(
    ;;#|
    ; /. f /. x (f x) + x
    ; (Int -> Int) -> Int -> Int
    (foo (L (V f) (L (V x) (A (A (V +) (A (V f) (V x))) (V x))))
      (PT Fun ((PT Fun ((C Int) (C Int))) (PT Fun ((C Int) (C Int)))))
      ,testpred-closure)

    ; (/. f /. x (f x) + x) (/. x + 1) 2
    ; Int
    (fooa (A (A (V foo) (L (V x) (A (A (V +) (V x)) (K 1)))) (K 2))
      (C Int)
      5)

    ; /. x x
    ; a -> a
    (id (L (V x) (V x))
     (Forall ((TV a)) (PT Fun ((TV a) (TV a)))) ,testpred-closure)

    ; (/. x x) 44
    ; int
    (ida (A (V id) (K 44))
     (C Int) 44)

    ; /. f /. x f (f x)
    ; (a -> a) -> a -> a
    (dapp (L (V f) (L (V x) (A (V f) (A (V f) (V x)))))
     (Forall ((TV d)) (PT Fun ((PT Fun ((TV d) (TV d))) (PT Fun ((TV d) (TV d)))))) ,testpred-closure)

    ; (/. f /. x f (f x)) (/. x x + x) 13
    ; 52
    (dappa (A (A (V dapp)
           (L (V x) (A (A (V +) (V x)) (V x))))
        (K 13))
     (C Int) 52)

    ; /. x /. y y
    ; a -> b -> b
    (xyy (L (V x) (L (V y) (V y)))
     (Forall ((TV a) (TV b)) (PT Fun ((TV a) (PT Fun ((TV b) (TV b)))))) ,testpred-closure)

    ; (/. x /. y y) 1 2
    ; 2
    (xxya (A (A (V xyy) (K 1)) (K 2))
     (C Int) 2)

    ; /. x /. y x
    ; a -> b -> a
    (xyx (L (V x) (L (V y) (V x)))
     (Forall ((TV b) (TV a)) (PT Fun ((TV a) (PT Fun ((TV b) (TV a)))))) ,testpred-closure)

    ; (/. x /. y x) 1 2
    ; 1
    (xyxa (A (A (V xyx) (K 1)) (K 2))
     (C Int) 1)

    (plus (V +)
     (PT Fun ((C Int) (PT Fun ((C Int) (C Int))))) ,testpred-native)
    (three (K 3)
     (C Int) 3)
    (troo (K #t)
     (C Bool) #t)
    (fals (K #f)
     (C Bool) #f)

    (cns (A (A (V Cons) (K 1)) (V Nil))
     (PT List ((C Int))) (Cons 1 Nil))
    (carcns (A (V car) (V cns))
     (C Int) 1)
    (cdrcns (A (V cdr) (V cns))
     (PT List ((C Int))) Nil)

    (ift (If (K #t) (K 1) (K 2))
     (C Int) 1)
    (iff (If (K #f) (K 1) (K 2))
     (C Int) 2)

    (eqt (A (A (V ==) (K 1)) (K 1))
     (C Bool) #t)
    (eqf (A (A (V ==) (K 1)) (K 2))
     (C Bool) #f)

    ; /. a /. b if a == b then a else b
    ; a -> a -> a
    (ifab (L (V a) (L (V b) (If (A (A (V ==) (V a)) (V b)) (V a) (V b))))
     (Forall ((TV c)) (PT Fun ((TV c) (PT Fun ((TV c) (TV c)))))) ,testpred-closure)

    ; (/. a /. b if a == b then a else b) 1 2
    ; 2
    (ifaban (A (A (V ifab) (K 1)) (K 2))
     (C Int) 2)

    ; (/. a /. b if a == b then a else b) 1 1
    ; 1
    (ifabae (A (A (V ifab) (K 1)) (K 1))
     (C Int) 1)

    ; Fix /. rec /. f /. xs /. z if (xs == []) z else (f (car xs) (rec f (cdr xs) z))
    (fold (L (V f) (L (V xs) (L (V z)
            (If (A (A (V ==) (V xs)) (V Nil))
              (V z)
              (A (A (V f) (A (V car) (V xs))) (A (A (A (V fold) (V f)) (A (V cdr) (V xs))) (V z)))))))
     (Forall ((TV m) (TV e))
         (PT Fun ((PT Fun ((TV m) (PT Fun ((TV e) (TV e)))))
                  (PT Fun ((PT List ((TV m))) (PT Fun ((TV e) (TV e)))))))) ,testpred-closure)

    (d1234 (A (A (V Cons) (K 1)) (A (A (V Cons) (K 2)) (A (A (V Cons) (K 3)) (A (A (V Cons) (K 4)) (V Nil)))))
      (PT List ((C Int)))
      (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))

    ; fold = fix + ns 0
    (folda (A (A (A (V fold)
              (V +))
           (V d1234))
        (K 0))
      (C Int) 10)

    (ml-fold (L (V f) (ML ((PL (PA (PA (V Cons) (PV a)) (PV d))
                               (L (V z) (A (A (V f) (V a)) (A (A (A (V ml-fold) (V f)) (V d)) (V z)))))
                           (PL (PV Nil)
                               (L (V z) (V z))))))
      (Forall ((TV f) (TV i))
          (PT Fun ((PT Fun ((TV f) (PT Fun ((TV i) (TV i)))))
                   (PT Fun ((PT List ((TV f))) (PT Fun ((TV i) (TV i)))))))) ,testpred-closure)

    ; fold = fix + ns 0
    (ml-folda (A (A (A (V ml-fold)
              (V +))
           (V d1234))
        (K 0))
      (C Int) 10)

    ; map = /. f /. x if (x == Nil) then Nil else Cons (f (car x)) (map f (cdr x))
    ; (a -> b) -> List a -> List b
    ; map = /. rec /. f /. x if (x == Nil) then Nil else Cons (f (car x)) (rec f (cdr x))
    ; ((a -> b) -> List a -> List b) -> (a -> b) -> List a -> List b
    (map (L (V f) (L (V x)
            (If (A (A (V ==) (V x)) (V Nil))
                (V Nil)
                (A (A (V Cons) (A (V f) (A (V car) (V x))))
                   (A (A (V map) (V f)) (A (V cdr) (V x)))))))
     (Forall
       ((TV o) (TV l))
         (PT
            Fun
               ((PT Fun ((TV o) (TV l)))
                   (PT Fun ((PT List ((TV o))) (PT List ((TV l)))))))) ,testpred-closure)

    ; map (/. x x + x) ns
    (mapa (A (A (V map)
           (L (V x) (A (A (V +) (V x)) (V x))))
        (V d1234))
     (PT List ((C Int))) (Cons 2 (Cons 4 (Cons 6 (Cons 8 Nil))))) 

    (ml-map (L (V f)
        (ML ((PL (PA (PA (V Cons) (PV a)) (PV d))
                 (A (A (V Cons) (A (V f) (V a))) (A (A (V ml-map) (V f)) (V d))))
             (PL (V Nil) (V Nil)))))
      (Forall
        ((TV f) (TV k))
          (PT
             Fun
                ((PT Fun ((TV f) (TV k)))
                    (PT Fun ((PT List ((TV f))) (PT List ((TV k)))))))) ,testpred-closure)

    ; map (/. x x + x) ns
    (ml-mapa (A (A (V ml-map)
           (L (V x) (A (A (V +) (V x)) (V x))))
        (V d1234))
      (PT List ((C Int))) (Cons 2 (Cons 4 (Cons 6 (Cons 8 Nil))))) 

    ; fact
    (fact (L (V n) (If (A (A (V ==) (V n)) (K 0))
                       (K 1)
                       (A (A (V *) (V n)) (A (V fact) (A (A (V -) (V n)) (K 1))))))
     (PT Fun ((C Int) (C Int))) ,testpred-closure)

    ; fact 10 baby
    (fact10 (A (V fact) (K 10))
     (C Int) 3628800)

    (ml-fact
      (ML ((PL (K 0) (K 1))
           (PL (PV n) (A (A (V *) (V n)) (A (V ml-fact) (A (A (V -) (V n)) (K 1)))))))
      (PT Fun ((C Int) (C Int))) ,testpred-closure)

    ; fact 10 baby
    (ml-fact10 (A (V ml-fact) (K 10))
     (C Int) 3628800)

    (even
      (L (V x) (If (A (A (V ==) (V x)) (K 0)) (K 1) (A (V odd) (A (A (V -) (V x)) (K 1)))))
      (Forall ((TV a))
        (PT Fun
          ((TV a)
           (PT Fun ((PT Fun ((C Int) (C Int))) (PT Fun ((C Int) (C Int)))))))) ,testpred-closure)
    (odd
      (L (V x) (If (A (A (V ==) (V x)) (K 0)) (K 0) (A (V even) (A (A (V -) (V x)) (K 1)))))
      (Forall ((TV b))
        (PT Fun ((PT Fun ((C Int) (C Int)))
                 (PT Fun ((TV b) (PT Fun ((C Int) (C Int)))))))) ,testpred-closure)
    ;(even (Fixn (K 0) (PT FixList ((V even-oprec) (V odd-oprec))))
      ;(PT Fun ((C Int) (C Int))) ,testpred-closure)
    ;(odd (Fixn (K 1) (PT FixList ((V even-oprec) (V odd-oprec))))
      ;(PT Fun ((C Int) (C Int))) ,testpred-closure)

    (e4 (A (V even) (K 4))
      (C Int) 1)
    (e5 (A (V even) (K 5))
      (C Int) 0)
    (o4 (A (V odd) (K 4))
      (C Int) 0)
    (o5 (A (V odd) (K 5))
      (C Int) 1)

    ;; 3-way mutual recursion
    (mod3
      (L (V x)
        (If (A (A (V ==) (V x)) (K 0))
          (K 0)
          (A (V mod3-1) (A (A (V -) (V x)) (K 1)))))
      (Forall ((TV a) (TV c))
        (PT Fun ((TV a)
                 (PT Fun ((PT Fun ((C Int) (C Int)))
                          (PT Fun ((TV c)
                                   (PT Fun ((C Int) (C Int)))))))))) ,testpred-closure)
    (mod3-1
      (L (V x)
        (If (A (A (V ==) (V x)) (K 0))
          (K 1)
          (A (V mod3-2) (A (A (V -) (V x)) (K 1)))))
      (Forall ((TV a) (TV b))
        (PT Fun ((TV a)
                 (PT Fun ((TV b)
                          (PT Fun ((PT Fun ((C Int) (C Int)))
                                   (PT Fun ((C Int) (C Int)))))))))) ,testpred-closure)
    (mod3-2
      (L (V x)
        (If (A (A (V ==) (V x)) (K 0))
          (K 2)
          (A (V mod3) (A (A (V -) (V x)) (K 1)))))
      (Forall ((TV b) (TV c))
        (PT Fun ((PT Fun ((C Int) (C Int)))
                 (PT Fun ((TV b)
                          (PT Fun ((TV c)
                                   (PT Fun ((C Int) (C Int)))))))))) ,testpred-closure)
    ;(mod3 (Fixn (K 0) (PT FixList ((V mod3-rec) (V mod3-1-rec) (V mod3-2-rec))))
      ;(PT Fun ((C Int) (C Int))) ,testpred-closure)
    ;(mod3-1 (Fixn (K 1) (PT FixList ((V mod3-rec) (V mod3-1-rec) (V mod3-2-rec))))
      ;(PT Fun ((C Int) (C Int))) ,testpred-closure)
    ;(mod3-2 (Fixn (K 2) (PT FixList ((V mod3-rec) (V mod3-1-rec) (V mod3-2-rec))))
      ;(PT Fun ((C Int) (C Int))) ,testpred-closure)

    (mod3-5 (A (V mod3) (K 5))
      (C Int) 2)
    (mod3-6 (A (V mod3) (K 6))
      (C Int) 0)
    (mod3-7 (A (V mod3) (K 7))
      (C Int) 1)

    (plx (PL (PV x) (V x))
      (Forall ((TV a)) (PT Fun ((TV a) (TV a)))) ,testpred-closure)
    (plxa (A (PL (PV x) (V x)) (K 12))
      (C Int) 12)
    (coulder (PL (PA (PA (V Cons) (PV aa)) (PV dd)) (V dd))
      (Forall ((TV c)) (PT Fun ((PT List ((TV c))) (PT List ((TV c)))))) ,testpred-closure)
    (coulder-a (A (V coulder)
                  (A (A (V Cons) (K 1)) (A (A (V Cons) (K 2)) (V Nil))))
      (PT List ((C Int))) (Cons 2 Nil))
    ;(coulder-a-fail (A (V coulder) (V Nil))
      ;(PT List ((C Int))) (Cons 2 Nil))

    (ml0 (ML ((PL (PA (PA (V Cons) (PV aa)) (PV dd)) (K 13))
              (PL (PV Nil) (K 12))))
      (Forall ((TV d)) (PT Fun ((PT List ((TV d))) (C Int)))) ,testpred-closure)
    (ml0-a-p (A (V ml0) (A (A (V Cons) (K 1)) (V Nil)))
      (C Int) 13)
    (ml0-a-n (A (V ml0) (V Nil))
      (C Int) 12)

    (rebuild (ML ((PL (PA (PA (V Cons) (PV a)) (PV d)) (A (A (V Cons) (V a)) (A (V rebuild) (V d))))
                  (PL (PV Nil) (V Nil))))
      (Forall ((TV j)) (PT Fun ((PT List ((TV j))) (PT List ((TV j)))))) ,testpred-closure)
    (rebuild-a (A (V rebuild) (V d1234))
      (PT List ((C Int))) (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))

    (scope0 (A (A (PL (PV a) (PL (PV b) (V a))) (K 10)) (K 11))
      (C Int) 10)
    (scope1 (A (A (PL (PV a) (PL (PV b) (V b))) (K 10)) (K 11))
      (C Int) 11)

    (ml-const (ML ((PL (K 1) (K 10))
                   (PL (K 2) (K 20))))
      (PT Fun ((C Int) (C Int))) ,testpred-closure)
    (ml-const-a-1 (A (V ml-const) (K 1))
      (C Int) 10)
    (ml-const-a-2 (A (V ml-const) (K 2))
      (C Int) 20)
    ;;|#

    ;; ml2a (a . d) () = 1
    ;; ml2a () (a . d) = 2
    (ml2a (ML ((PL (PA (PA (V Tuple2) (PA (PA (V Cons) (PV a)) (PV d))) (V Nil)) (K 1))
               (PL (PA (PA (V Tuple2) (V Nil)) (PA (PA (V Cons) (PV a)) (PV d))) (K 2))))
       (Forall ((TV h) (TV s))
         (PT Fun ((PT Tuple2 ((PT List ((TV h))) (PT List ((TV s))))) (C Int)))) ,testpred-closure)
       ;(Forall ((TV a)) (PT Fun ((PT List ((TV a))) (PT List ((TV a)))))) ,testpred-closure)
    (ml2a-s (L (V x) (L (V y) (A (V ml2a) (A (A (V Tuple2) (V x)) (V y)))))
      (Forall ((TV d) (TV e)) (PT Fun ((PT List ((TV d))) (PT Fun ((PT List ((TV e))) (C Int)))))) ,testpred-closure)
    (ml2a-sa (A (A (V ml2a-s) (A (A (V Cons) (K 1)) (V Nil))) (V Nil))
      (C Int) 1)
   ))

(define (native-curry-2 f) `(Native ,(lambda (x) `(Native ,(lambda (y) (f x y))))))
(define global-env `(
  (+ . (Native ,(lambda (x) `(Native ,(lambda (y) (+ x y))))))
  (- . ,(native-curry-2 -))
  (* . ,(native-curry-2 *))
  (Cons . (Native ,(lambda (a) `(Native ,(lambda (d) `(Cons ,a ,d))))))
  (Nil . Nil)
  (car . (Native ,(lambda (x) (mtch x ('Cons a d) a))))
  (cdr . (Native ,(lambda (x) (mtch x ('Cons a d) d))))
  (== . ,(native-curry-2 eq?))
  (Tuple2 . ,(native-curry-2 (lambda (x y) `(Tuple2 ,x ,y))))
))

(define (lookup x ass)
  (mtch (assoc x ass)
    (x . v)
      v
    #f
      (err 'lookup x ass)))

(define (leval-check-type v t)
  (mtch (list v t)
    (('Closure lam env) t)
      (assert (mtch t ('PT 'Fun (a b)) #t ('Forall _ ('PT 'Fun (a b))) #t))
    (('Native f) t)
      (assert (mtch t ('PT 'Fun (a b)) #t ('Forall _ ('PT 'Fun (a b))) #t))
    (i ('C 'Int))
      (assert (number? i))
    (b ('C 'Bool))
      (assert (boolean? b) b)
    x
      x
    )
  v)
;(tracefun leval-check-type)

(define (generate-nested-application f args)
  (mtch args
    (a)
      `(A ,f ,a)
    (a . d)
      (generate-nested-application `(A ,f ,a) d)))

;; This doesn't do anything with the env other than cons onto it
;; so maybe it should just return a list which is then appended.
(define (pattern-match-and-extend-env p x env)
  (mtch `(,p ,x)
    (('T p pt) x)
      (pattern-match-and-extend-env p x env)
    (('PV v) x)
      `(((,v . ,x) . ,env))
    (('C ca) ('C cb))
      (if (eq? ca cb)
        `(,env)
        '())
    (('PA ('T ('V ctor) ctt) arg) (ctor2 arg2))
      (if (eq? ctor ctor2)
        (pattern-match-and-extend-env arg arg2 env)
        '())
    (('PA fun arg) (ctor2 . args2))
      (mtch (pattern-match-and-extend-env fun `(,ctor2 . ,(rdc args2)) env)
        (env)
          (pattern-match-and-extend-env arg (rac args2) env)
        '()
          '())
    ;; This is e.g. (cdr nil), arg isn't a cton
    (('PA fun arg) x)
      '()
    ((KK k) k2)
      (if (eq? k k2)
        `(,env)
        '())
        ))
#;(tracefun-with
  (lambda (app runner) (mtch app (f p x env)
    (plain-ol-tracer `(,f ,p ,x) runner)))
  pattern-match-and-extend-env)

;; -> maybe value
(define (leval-ml-ls ls x env)
  (mtch ls
    (('T ('PL tp b) t) . lsd)
      (mtch (pattern-match-and-extend-env tp x env)
        (extended-env)
          `(,(leval b extended-env))
        '()
          (leval-ml-ls lsd x env))
    '()
      '()))

(define (leval e env)
  (mtch e
    ('T e t)
      (leval-check-type (leval e env) t)
    ('L v b)
      `(Closure ,e ,env)
    ('PL v b)
      `(Closure ,e ,env)
    ('ML ls)
      `(Closure ,e ,env)
    ('A f x)
      (let ((f (leval f env))
            (x (leval x env)))
        (mtch f
          ('Closure ('L ('V v) b) c-env)
            (leval b `((,v . ,x) . ,c-env))
          ('Closure ('L ('T ('V v) _) b) c-env)
            (leval b `((,v . ,x) . ,c-env))
          ('Closure ('PL tp b) c-env)
            (mtch (pattern-match-and-extend-env tp x c-env)
              (extended-env)
                (leval b extended-env)
              '()
                (err 'pattern-match-failure tp x))
          ('Closure ('ML ls) c-env)
            (mtch (leval-ml-ls ls x c-env)
              (v)
                v
              '()
                (err 'pattern-match-failure tp x))
          (Native f)
            (f x)))
    ('If b t e)
      (mtch (leval b env)
        #t
          (leval t env)
        #f
          (leval e env))
    ('Fix f)
      (leval `(A ,f (L (V x) (A (Fix ,f) (V x)))) env)
    ('Fixn (K i) (PT FixList funs))
      (leval
        (generate-nested-application
          (nth i funs)
          (map
            (lambda (i)
              `(L (V x) (A (Fixn (K ,i) (PT FixList ,funs)) (V x))))
            (gen-integer-sequence 0 (length funs))))
        env)
      #|
      (leval `(A (A ,(nth i funs)
                    (L (V x) (A (Fixn 0 ,funs) (V x))))
                 (L (V x) (A (Fixn 1 ,funs) (V x))))
             env)
             |#
    ('V v)
      (lookup v env)
    ('K k)
      k))
;(tracefun leval)

(define (shew-program-types typed-program)
  (display
    (++
      (join-things "\n"
        (map (lambda (binding) (mtch binding (name . ('T e t))
               (++ name " :: " (lshew-type t))))
          typed-program))
      "\n")))

(define (infer-program program) (infer-program-1 program initial-type-env global-env))
(define (infer-program-1 program type-env g-env)
  (mtch program
    ((name . body) . rest)
      (mtch (infer-types body type-env)
        ('T e t)
          `((,name . (T ,e ,t))
            . ,(infer-program-1 rest (append type-env `((,name . ,t))) g-env)))
    '()
      '()))

(define (eval-program typed-program env)
  (mtch typed-program
    ;; ONO This does not build the global env as we go which is fine if there are no CAFs but main
    ((name . ('T e t)) . rest)
      (let ((evaled-e (leval e env)))
        `((,name . ,evaled-e)
          . ,(eval-program rest `((,name . ,evaled-e) . ,env))))
    '()
      '()))

(define (verify-results typed-program evaled-program)
  (map (lambda (per) (mtch per (name code expected-type expected-result)
    (let ((actual-type (mtch (lookup name typed-program) ('T e t) t))
          (actual-result (lookup name evaled-program)))
      (assert (equal? expected-type actual-type) expected-type actual-type)
      ;(shew 'hey expected-result)
      (if (procedure? expected-result)
        (assert (expected-result actual-result) actual-result)
        (assert (equal? expected-result actual-result) expected-result actual-result))
      ;(shew `(test ,name))
      )))
    test-program)
  (shew 'test-ok))

(define (shew-elide-closures program)
  (shew
    (grep
      (lambda (b) (mtch b (name . ('Closure . _)) #f (name . value) #t))
      program)))

(define (_main)
  (let ((program (map (lambda (x) (mtch x (n c t v) `(,n . ,c))) test-program)))
    (let ((typed-program (infer-program program)))
      (shew-program-types typed-program)
      (let ((evaled-program (eval-program typed-program global-env)))
        ;(shew 'evaled evaled-program)
        ;(begin (shew 'evaled) (shew-elide-closures evaled-program))
        (verify-results typed-program evaled-program)))))

;; Cycle finding takes a digraph in this form:
;;   ((src0 sink0 sink1 ...) ...)

;; Returns a list of cycles
(define (find-cycles digraph path n)
  (if (member? n path)
    (begin
      ;(shew `(loop ,(prefix-until n path)))
      `(,(prefix-until n path)))
    (let ((ts (lookup n digraph)))
      ;(shew `(,n --> ,@ts))
      (apply append
        (map (lambda (t) (find-cycles digraph `(,n . ,path) t)) ts)))))
;(tracefun find-cycles)

(define (find-cycles-all digraph)
  (let ((nodes (map car digraph)))
    (let ((cycles (apply append (map (lambda (n) (find-cycles digraph '() n)) nodes))))
      ;(shew 'cycles cycles)
      (let ((ecs (ec-set-add-sets (ec-set-make) cycles)))
        ;(shew 'ecs ecs)
        ecs))))

(define proo '(
  (a b) (b a h)
  (c d e h) (e c)
  (d e)
  (f g)
  (g h i)
  (h)
  (i)
  ))

(define (find-pattern-binders pat)
  (mtch pat
    ('PA fun arg)
      (append (find-pattern-binders fun) (find-pattern-binders arg))
    ('V x)
      '()
    ('PV x)
      `(,x)
    ('K k)
      '()
      ))

(define (find-unbound-refs e bound-ids)
  (mtch e
    ('L ('V x) body)
      (find-unbound-refs body `(,x . ,bound-ids))
    ('A fun arg)
      (append (find-unbound-refs fun bound-ids) (find-unbound-refs arg bound-ids))
    ('V x)
      (if (member? x bound-ids) '() `(,x))
    ('K k)
      '()
    ('If b t e)
      (append (find-unbound-refs b bound-ids) (find-unbound-refs t bound-ids) (find-unbound-refs e bound-ids))
    ('ML lams)
      (apply append (map (lambda (e) (find-unbound-refs e bound-ids)) lams))
    ('PL pat body)
      (append
        (find-unbound-refs pat bound-ids)
        (find-unbound-refs body (append (find-pattern-binders pat) bound-ids)))
    ('PA fun arg)
      (append (find-unbound-refs fun bound-ids) (find-unbound-refs arg bound-ids))
    ('PV v)
      '()
      ))

(define (find-global-refs program)
  (map (lambda (nv) (mtch nv
    (name . value)
      `(,name . ,(find-unbound-refs value '()))))
    program))

    #|
    (even-oprec
      (L (V even-rec)
        (L (V odd-rec)
          (L (V x) (If (A (A (V ==) (V x)) (K 0)) (K 1) (A (V odd-rec) (A (A (V -) (V x)) (K 1)))))))
    (even (Fixn (K 0) (PT FixList ((V even-oprec) (V odd-oprec))))
      (PT Fun ((C Int) (C Int))) ,testpred-closure)
      |#

(define (->openrec-name s)
  (->symbol (++ (->string s) "-openrec")))

(define (generate-fixns-1 program cycle)
  (apply append
    (map-with-index
      (lambda (i id)
        (let ((openrec-form
                (foldr
                  (lambda (fun e) `(L (V ,fun) ,e))
                  (lookup id program)
                  cycle))
              (tied
                `(Fixn (K ,i) (PT FixList ,(map (lambda (fun) `(V ,(->openrec-name fun))) cycle)))))
          `((,(->openrec-name id) . ,openrec-form)
            (,id . ,tied))))
      cycle)))

(define (generate-fixns program cycles)
  ;(shew 'program program)
  (apply append (map (lambda (c) (generate-fixns-1 program c)) cycles)))

(define (main)
  (let ((program (map (lambda (rec) (mtch rec (name value type evaled) `(,name . ,value))) test-program-no-fix)))
    (let ((refs (find-global-refs program)))
      ;(shew 'refs refs)
      (let ((refs (append refs (map (lambda (gb) (mtch gb (id . val) `(,id))) global-env))))
        ;(shew 'refs refs)
        (let ((cycles (find-cycles-all refs)))
          (shew 'cycles cycles)
          (shew (generate-fixns program cycles)))))))
