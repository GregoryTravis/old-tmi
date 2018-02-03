(load "lib.ss")
(load "mtch.ss")
(load "precedence.ss")
(load "preprocess.ss")
(load "tokenize.ss")
(require profile)

(define gram #hash(
  (top . ((decls)))
  ;(app . ((exp exp) (app exp)))
  (let . ((let_keyword lcb decls rcb in_keyword exp)))
  (where . ((exp where_keyword lcb decls rcb)))
  (definition . ((exp equals exp)))
  (decls . ((definition semicolon decls) (definition)))
  (parenexp . ((lparen exp rparen)))
  (listexp . ((lsb comma-separated-exp-sequence rsb)))
  (comma-separated-exp-sequence . ((exp) (comma-separated-exp-sequence comma exp)))
  (base-exp . ((constructor) (identifier) (integer) (operator) (parenexp) (listexp)))
  (base-exp-seq . ((base-exp) (base-exp-seq base-exp)))
  (exp . ((if) (let) (where) (case) (base-exp-seq)))
  (case . ((case_keyword exp of_keyword lcb case_clauses rcb)))
  (case_clauses . ((case_clauses semicolon case_clause) (case_clause)))
  (case_clause . ((exp rdbl_arrow exp)))
  (if . ((if_keyword exp then_keyword exp else_keyword exp)))
))

(define binarize-production-symgen (tagged-symbol-generator-generator "parsebin-"))
(define (binarize-production production)
  (mtch production
    (nt (a b c . rest))
      (let ((new-nt (binarize-production-symgen)))
        `((,nt (,a ,new-nt)) . ,(binarize-production `(,new-nt (,b ,c . ,rest)))))
    (nt rhses) `((,nt ,rhses))))

(define (regroup-binarized-grammar gram)
  (make-hash (map (lambda (group) (cons (caar group) (map cadr group))) (group-by car gram))))

(define (binarize grammar)
  (regroup-binarized-grammar
    (flatten1
      (map binarize-production
        (flatten1
          (map (lambda (ntdef)
                 (mtch ntdef
                      (nt . rhses) (map (lambda (rhs) `(,nt ,rhs)) rhses)))
               (hash->list grammar)))))))

; Returns (value) or #f
; TODO not using memo yet
(define (parse gram nt os s e memo)
  (if (eq? s e)
    ; TODO implement epsilon?
    #f
    (let ((memo-val (hash-ref memo (list nt s e) '())))
      (if (not (null? memo-val))
          ; This includes the recursion-prevention #f value
          memo-val
          ;; These two lines probably not needed, redudant to previous if
          (if (eq? memo-val #f)
            #f  
            ; memo placeholder for recursion prevention
            (begin
              (hash-set! memo (list nt s e) #f)
              (mtch
                (if (and (eq? (+ s 1) e) (eq? (car (vector-ref os s)) nt))
                  ; TODO maybe check before the memo check?
                  `(,(vector-ref os s))
                  (find-first-maybe
                    (lambda (production) (try-prodution gram nt os s e memo production))
                    (hash-ref gram nt (lambda () '()))))
                (value)
                  (begin
                    (hash-set! memo (list nt s e) `(,value))
                    `(,value))
                #f
                  (begin
                    ;(hash-remove! memo (list nt s e))
                    ;; Probably redundant?
                    (hash-set! memo (list nt s e) #f)
                    #f))))))))

(define (try-prodution gram nt os s e memo production)
  (mtch production
    (a b)
      (mtch (find-first-maybe (lambda (m) (try-two gram a b os s m e memo)) (gen-integer-sequence s e))
        ((resulta resultb)) `((,nt ,resulta ,resultb))
        #f #f)
    (a)
      (mtch (parse gram a os s e memo)
        (result) `((,nt ,result))
        #f #f)))

#|
(tracefun-with
  (lambda (app runner)
    (mtch app (try-prodution gram nt os s e memo production)
      (plain-ol-tracer (list 'try-prodution nt os s e production) runner)))
  try-prodution)
|#

(define (try-two gram a b os s m e memo)
  (mtch (parse gram a os s m memo)
    (resulta)
      (mtch (parse gram b os m e memo)
        (resultb) `((,resulta ,resultb))
        #f #f)
    #f #f))

#|
(tracefun-with
  (lambda (app runner)
    (mtch app (try-two gram a b os s m e memo)
      (plain-ol-tracer (list 'try-two a b os s m e) runner)))
  try-two)
|#

(define (grammar-unbinarize e)
  (mtch e
    (a (b . c))
      (if (and (symbol? b) (starts-with (symbol->string b) "parsebin-"))
          `(,(grammar-unbinarize a) . ,(grammar-unbinarize c))
          `(,(grammar-unbinarize a) (,(grammar-unbinarize b) . ,(grammar-unbinarize c))))
    (a . d)
      `(,(grammar-unbinarize a) . ,(grammar-unbinarize d))
    aa aa))
;(tracefun unbinarize)

(define (case-clause-unbinarize-1 e)
  (mtch e
    ('case_clauses ('case_clause . rest)) (list rest)
    ('case_clauses as semicolon ('case_clause . rest)) (append (case-clause-unbinarize-1 as) (list rest))
    x x))
(define (case-clause-unbinarize e) (apply-and-descend case-clause-unbinarize-1 e))

  ;(definition . ((exp equals exp)))
  ;(decls . ((definition semicolon decls) (definition)))
(define (decls-unbinarize-1 e)
  (mtch e
    ('decls ('definition . d)) (list d)
    ('decls ('definition . d) semicolon ('decls . rest)) (cons d (decls-unbinarize-1 `(decls . ,rest)))
    x x))
(define (decls-unbinarize e) (apply-and-descend decls-unbinarize-1 e))

(define (un-definition-1 e)
  (mtch e
    ('definition . d) d
    x x))
(define (un-definition e) (apply-and-descend un-definition-1 e))

(define (flatten-base-exp-seq e)
  (mtch e
    ('base-exp-seq rdc ('base-exp e))
      (append (flatten-base-exp-seq rdc) (list e))
    ('base-exp-seq ('base-exp e))
      (list e)))
(define (base-exp-seq-unbinarize-1 e)
  (mtch e
    ('base-exp-seq . d) `(app ,(flatten-base-exp-seq e))
    x x))
(define (base-exp-seq-unbinarize e) (apply-and-descend base-exp-seq-unbinarize-1 e))

(define (apply-and-descend f e)
  (let ((e (apply-until-fixpoint f e)))
    (if (pair? e)
      (cons (apply-and-descend f (car e)) (apply-and-descend f (cdr e)))
      e)))

(define (un-cses-1 cses)
  (mtch cses
    ('comma-separated-exp-sequence ('exp a))
      `(,a)
    ('comma-separated-exp-sequence rdc ('comma . _) ('exp rac))
      (append (un-cses-1 rdc) (list rac))))
(define (un-cses e)
  (foldr
    (lambda (a d)
      `(app ((constructor "Cons") ,a ,d)))
    `(app ((constructor "Nil")))
    (un-cses-1 e)))
;(tracefun un-cses)

(define (p2s-1 e)
  (mtch e
    ('let ('let_keyword . x) ('lcb . x) decls ('rcb . x) ('in_keyword . x) exp)
      `(let ,decls ,exp)
    ('where exp ('where_keyword . x) ('lcb . x) decls ('rcb . x))
      `(where ,decls ,exp)
    ('if ('if_keyword . x) b ('then_keyword . x) t ('else_keyword . x) e)
      `(if ,b ,t ,e)
    ('exp x) x
    ('case case_keyword exp of_keyword lcb case_clauses rcb) `(case ,exp ,case_clauses)
    ('listexp ('lsb . _) cses ('rsb . _)) (un-cses cses)
    x x))
(define (p2s e) (apply-and-descend p2s-1 e))
;(tracefun p2s-1)

; Look for app trees and pass them to unapp-1
(define (unapp e)
  (mtch e
    ('app . x) `(app ,(unapp-1 e))
    (a . d) `(,(unapp a) . ,(unapp d))
    x x))
(define (unapp-1 e)
  (mtch e
    ('app x ('app y . z)) `(,(unapp x) . ,(unapp-1 `(app ,y . ,z)))
    ('app x y) `(,(unapp x) ,(unapp y))))
;(tracefun unapp)(tracefun unapp-1)

(define (unparenexp-1 e)
  (mtch e
    ('parenexp lparen app rparen) app
    x x))
(define (unparenexp e) (apply-and-descend unparenexp-1 e))
;(define unparenexp id)

; At this point all expressions are (app (a b c)) where b is an operator, including $$.
; There may also be (app (a)).
; Convert $$-chains to regular multi-arg app nodes, and the rest to binops.
(define (separate-app-op sem)
  (mtch sem
    ('let bindings body)
      `(let ,(map separate-app-op bindings) ,(separate-app-op body))
    ('if b t e)
      `(if ,(separate-app-op b) ,(separate-app-op t) ,(separate-app-op e))
    ('definition a ('equals . d) b)
      `(,(separate-app-op a) (equals . ,d) ,(separate-app-op b))
    (a ('equals . d) b)
      `(,(separate-app-op a) (equals . ,d) ,(separate-app-op b))
    ('app (a ('operator "$$" . d) b))
      `(app ,(map separate-app-op (unfold-real-app sem)))
    ('app (a ('operator . d) b))
      `(binop ,(separate-app-op a) (operator . ,d) ,(separate-app-op b))
    ('app (a))
      `(app (,(separate-app-op a)))
    x x))

(define (unfold-real-app sem)
  (mtch sem
    ('app (a ('operator "$$" . d) b))
      (append (unfold-real-app a) `(,b))
    x `(,x)))
    ;('app (a op b))
      ;`(,sem)))

(define (postprocess e)
  ; Unparenexp must be after unapp
  ;(separate-app-op (precedence (unparenexp (unapp (p2s (decls-unbinarize (case-clause-unbinarize (grammar-unbinarize e)))))))))
  (un-definition (separate-app-op (precedence (unparenexp (p2s (base-exp-seq-unbinarize (decls-unbinarize (case-clause-unbinarize (grammar-unbinarize e))))))))))

(define (top-parse gram nt os)
  ;(shew 'parse os)
  (let ((gram (binarize gram)))
    ;(shew gram)
    (mtch (parse gram nt (list->vector os) 0 (length os) (make-hash))
      (value) `(,value)
      #f #f)))

#|
(tracefun-with
  (lambda (app runner)
    (mtch app ('parse gram nt os s e memo) (plain-ol-tracer (list 'parse nt s e) runner)))
  parse)
|#

#|
(shew (top-parse gram 'rcb '(rcb)))
(shew (top-parse gram 'exp '(identifier)))
(shew (top-parse gram 'app '(identifier identifier)))
(shew (top-parse gram 'exp '(identifier identifier)))
(shew (top-parse gram 'exp '(identifier identifier identifier)))
(shew (top-parse gram 'exp '(lparen identifier rparen)))
(shew (top-parse gram 'exp '(lparen identifier rparen)))
(shew (top-parse gram 'exp '(lparen lparen identifier rparen rparen)))
(shew (top-parse gram 'decls '(identifier equals identifier identifier semicolon identifier equals identifier)))
|#

(define (wrap-file tokens)
  (mtch (last tokens)
    (a as (row col))
      `((let_keyword "let" (-1 -1))
        ,@tokens
        (in_keyword "in" (,(+ row 1) -1))
        (identifier "main" (,(+ row 1) 2)))))

; Categorical!
(define (maybe-list ms)
  (if (any? (map (lambda (m) (eq? m #f)) ms))
    #f
    (list (map car ms))))

; Split into tlfs and parse separately; won't work on already-preprocessed code
; (if it lacks proper layout) (define (parse-file filename).
;
; TODO: if a tlf fails to parse, don't keep parsing the rest of the lines.
(define (parse-file filename)
  (let ((tokens (tokenize-top (read-file-as-string filename))))
    (mtch (maybe-list (map (lambda (tlf) (top-parse gram 'definition (preprocess-top tlf)))
                        (split-into-tlfs tokens)))
      (value) `((let ,(map postprocess value) (app ((identifier "main")))))
      #f #f)))

(define (split-into-tlfs tokens)
  (group-by-starts (lambda (token) (mtch token (_ _ (line column)) (eq? column 0))) tokens))

(hook-with timing-hook parse-file tokenize-top preprocess-top top-parse binarize postprocess)
