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

; fold = /. f /. xs /. z if xs == [] z else (cons (f (car xs)) (fold f (cdr xs) z))
; or
; fold f [] z = z
; fold f (x : xs) z = (f x) : (fold f xs z)
; So this needs:
; - List T = Nil | Const T (List T)
; - multiple clauses
;   - this is probablly just: set them equal to each other
; - parameterized types
; - if/then/else
; - Fix

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
;(tracefun tinf0)

(define (ut a b)
  (if (not (equal? a b))
    (begin
      (shew a b)
      (err 'test-failure))
    '()))

(ut 1 1)
;(ut 1 2)

#|
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
|#

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
;(tracefun apply-unifiers-to-type-term )

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
    ('F a b)
      (++ "(" (lshew-type a) " -> " (lshew-type b) ")")))
;(tracefun lshew-type)

(define (lshew-eqns eqns)
  (join-things "\n" (map (lambda (eqn) (mtch eqn (a b) (++ (lshew-type a) " = " (lshew-type b)))) eqns)))
(define (shew-eqns eqns) (display (lshew-eqns eqns)) (display "\n"))

(define (lshew-ecs ecs)
  (join-things "\n"
    (map (lambda (ec) (join-things " = " (map lshew-type ec))) ecs)))
(define (shew-ecs ecs) (display (lshew-ecs ecs)) (display "\n"))

;; eqns -> ecs (not unified)
(define (unify-create-initial-ec-set eqns) (unify-create-initial-ec-set-1 eqns (ec-set-make)))
(define (unify-create-initial-ec-set-1 eqns ecs)
  (mtch eqns
    ((a b) . d)
      (unify-create-initial-ec-set-1 d (ec-set-add ecs a b))
    '()
      ecs))

;; ecs -> eqns
(define (unify-get-ec-set-all-pairs ecs)
  (apply append (map all-pairs ecs)))
  ;(map all-pairs ecs))

(define (unify-get-sub-eqns eqn)
  (mtch eqn
    (('F a b) ('F c d))
      `((,a ,c) (,b ,d))
    x
      '()))

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
      ;(shew 'added added)
      added)))

;; eqns -> ecs (unified)
(define (unify eqns)
  (apply-until-fixpoint unify-dive-one-step (unify-create-initial-ec-set eqns)))

(define (unify-big-check s)
  (and
    (ec-set-ok s)
    (unify-check-no-op s)))
(define (unify-check-no-op s)
  ;; Verifying this is a no-op
  (and
    (equal? s (unify (unify-get-ec-set-all-pairs s)))
    (equal? s (unify-ec-set-add-eqns (ec-set-make) (unify-get-ec-set-all-pairs s)))))

;(define (type-is-constant t) (mtch t ('C c) #t x #f))
(define (type-is-var t) (mtch t ('TV v) #t x #f))
(define (type-is-fun t) (mtch t ('F a b) #t x #f))

(define (type-is-monotype t)
  (mtch t
    ('C c)
      #t
    ('TV v)
      #f
    ('F a b)
      (and (type-is-monotype a) (type-is-monotype b))))

(define (unify-ec-is-type-error ec)
  (> (length (grep type-is-monotype ec)) 1))
(define (unify-ecs-is-type-error ecs)
  (any? (map unify-ec-is-type-error ecs)))

(define (unify-ec-is-vars-n-mono ec)
  (and
    (all? (map (for type-is-monotype type-is-var) ec))
    (> (length (grep type-is-monotype ec)) 0)))

; ONO maybe check explicitly for ones with no vars at all?
(define (unify-vm-ec-to-subs ec)
  (mtch (divide-by-pred type-is-monotype ec)
    ((t) . vs)
      (begin
        ;(assert (> (length vs) 0))
        (map (lambda (v) `(,v ,t)) vs))))

(define (unify-ec-is-vars-only ec)
  (all? (map type-is-var ec)))

(define (unify-vo-ec-to-subs ec)
  (map (lambda (v) `(,v ,(car ec))) (cdr ec)))

;(tracefun unify-ec-is-vars-n-mono unify-ec-is-vars-only)
;(tracefun unify-vm-ec-to-subs unify-vo-ec-to-subs)

(define (unify-get-subs ecs)
  (append
    (apply append
      (map unify-vm-ec-to-subs (grep unify-ec-is-vars-n-mono ecs)))
    (apply append
      (map unify-vo-ec-to-subs (grep unify-ec-is-vars-only ecs)))))

(define (unify-map-over-subs-types f subs)
  (map (lambda (sub) (mtch sub (a b) `(,(f a) ,(f b)))) subs))

(define (unify-apply-subs ecs subs)
  (unify-map-over-subs-types
    (lambda (e) (apply-unifiers-to-type-term subs e))
    (unify-get-ec-set-all-pairs ecs)))

;; Really not sure if this is right, just a guess.
;; After all other subs are done, the remainder ecs
;; Should be just a set of (var, something) pairs,
;; so verify and just return them as subs
;; ecs -> subs
(define (unify-extract-final-subs ecs)
  (map (lambda (ec) (mtch ec (('TV v) x) ec)) ecs))

;; ecs -> (subs ecs)
(define (unify-sub-one-step ecs)
  (let ((subs (unify-get-subs ecs)))
    (shew 'subs subs)
    (let ((applied (unify-apply-subs ecs subs)))
      (shew 'applied applied)
      (shew-ecs applied)
      (let ((unified (unify applied)))
        (shew 'unified unified)
        (shew-ecs unified)
        `(,subs ,unified)
        )
      )))
;(tracefun unify-sub-one-step)
;; ecs -> subs
(define (unify-sub ecs) (unify-sub-1 '() ecs))
(define (unify-sub-1 subs ecs)
  (mtch (unify-sub-one-step ecs)
    (new-subs new-ecs)
      (if (equal? new-ecs ecs)
        (begin
          (assert (equal? new-subs '()))
          (append subs (unify-extract-final-subs new-ecs)))
        (unify-sub-1 (append subs new-subs) new-ecs))))

(define (unify-really e)
  (set! ty (make-type-symgen))
  (shew e)
  (mtch (tinf e)
    (typed-exp eqns)
      (begin
        (shew typed-exp)
        (shew 'eqns eqns)
        (shew 'eqns)
        (shew-eqns eqns)
        (let ((unified (unify eqns)))
          (unify-big-check unified)
          (shew 'unified)
          (shew unified)
          (shew-ecs unified)
          (assert (not (unify-ecs-is-type-error unified)))
          (let ((all-subs (unify-sub unified)))
            (shew 'so all-subs)
            (let ((typed-term-2 (apply-unifiers-to-term all-subs typed-exp)))
              (shew 'and-so typed-term-2)
              typed-term-2))))))

(define (just-type e)
  (mtch (unify-really e) ('T e t) t))

(define unify-tests
  '(
    ; /. f /. x (f x) + x
    ; (Int -> Int) -> Int -> Int
    ((L (V f) (L (V x) (A (A (V +) (A (V f) (V x))) (V x))))
     (F (F (C Int) (C Int)) (F (C Int) (C Int))))

    ; /. x x
    ; a -> a
    ((L (V x) (V x))
     (F (TV a) (TV a)))

    ; /. f /. x f (f x)
    ; (a -> a) -> a -> a
    ((L (V f) (L (V x) (A (V f) (A (V f) (V x)))))
     (F (F (TV d) (TV d)) (F (TV d) (TV d))))
   ))

(define (run-unify-tests)
  (map (lambda (test) (mtch test
    (src expected)
      (let ((actual (just-type src)))
        (shew 'test src actual expected)
        (assert (equal? expected (just-type src))))))
    unify-tests))

(define (main)
  (run-unify-tests)
  (let ((t (just-type foo)))
    (shew t (lshew-type (just-type foo)))))
