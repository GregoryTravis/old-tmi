(load "lib.ss")
(load "mtch.ss")
(load "precedence.ss")
(load "preprocess.ss")
(load "tokenize.ss")
(require profile)

(define gram #hash(
  (top . ((decls)))
  ;(app . ((exp exp) (app exp)))
  (plet . ((let_keyword lcb decls rcb in_keyword exp)))
  (pwhere . ((exp where_keyword lcb decls rcb)))
  (definition . ((exp equals exp)))
  (decls . ((definition semicolon decls) (definition)))
  (parenexp . ((lparen exp rparen)))
  (listexp . ((lsb comma-separated-exp-sequence rsb)))
  (comma-separated-exp-sequence . ((exp) (comma-separated-exp-sequence comma exp)))
  (lambda-exp . ((lambda parenexp exp)))
  (base-exp . ((constructor) (identifier) (integer) (operator) (parenexp) (listexp) (lambda-exp)))
  (base-exp-seq . ((base-exp) (base-exp-seq base-exp)))
  (exp . ((pif) (plet) (pwhere) (case) (base-exp-seq)))
  (case . ((case_keyword exp of_keyword lcb case_clauses rcb)))
  (case_clauses . ((case_clauses semicolon case_clause) (case_clause)))
  (case_clause . ((exp rdbl_arrow exp)))
  (pif . ((if_keyword exp then_keyword exp else_keyword exp)))
))

(define (general-recurser before-fun after-fun e)
  (let ((e (before-fun e))
        (rec (lambda (e) (general-recurser before-fun after-fun e))))
    (after-fun
      (mtch e
        ('top decls)
          `(top ,(rec decls))
        ('plet letk lcb decls rcb ink exp)
          `(plet ,letk ,lcb ,(rec decls) ,rcb ,ink ,(rec exp))
        ('let decls exp)
          `(let ,(map rec decls) ,(rec exp))
        ('pwhere exp wherek lcb decls rcb)
          `(pwhere ,(rec exp) ,wherek ,lcb ,(rec decls) ,rcb)
        ('where decls body)
          `(where ,(map rec decls) ,(rec body))
        ('definition lexp equalsk rexp)
          `(definition ,(rec lexp) ,equalsk ,(rec rexp))
        ('decls def semi decls)
          `(decls ,(rec def) ,semi ,(rec decls))
        ('decls def)
          `(decls ,(rec def))
        ('decls-list ds)
          `(decls-list ,(map rec ds))
        ('parenexp lp e rp)
          `(parenexp ,lp ,(rec e) ,rp)
        ('listexp lsb cses rsb)
          `(listexp ,lsb ,(rec cses) ,rsb)
        ('comma-separated-exp-sequence exp)
          `(comma-separated-exp-sequence ,(rec exp))
        ('comma-separated-exp-sequence cses comma exp)
          `(comma-separated-exp-sequence ,(rec cses) ,comma ,(rec exp))
        ('lambda-exp lambda args exp)
          `(lambda-exp ,lambda ,(rec args) ,(rec exp))
        ('base-exp e)
          `(base-exp ,(rec e))
        ('base-exp-seq e)
          `(base-exp-seq ,(rec e))
        ('base-exp-seq es e)
          `(base-exp-seq ,(rec es) ,(rec e))
        ('exp e)
          `(exp ,(rec e))
        ('binop a op b)
          `(binop ,(rec a) ,(rec op) ,(rec b))
        ('app es)
          `(app ,(map rec es))
        ('case casek e ofk lcb ccs rcb)
          `(case ,casek ,(rec e) ,ofk ,lcb ,(rec ccs) ,rcb)
        ('case e ccs)
          `(case ,(rec e) ,(map rec ccs))
        ('case_clauses ccs semi cc)
          `(case_clauses ,(rec ccs) ,semi ,(rec cc))
        ('case_clauses cc)
          `(case_clauses ,(rec cc))
        ('case_clause pat arr exp)
          `(case_clause ,(rec pat) ,arr ,(rec exp))
        ('case_clause pat exp)
          `(case_clause ,(rec pat) ,(rec exp))
        ('case-clauses-list cc)
          `(case-clauses-list ,(map rec cc))
        ('pif ifk b thenk t elsek e)
          `(pif ,ifk ,(rec b) ,thenk ,(rec t) ,elsek ,(rec e))
        ('if b t e)
          `(if ,(rec b) ,(rec t) ,(rec e))
        ;; Tokens.  TODO put 'token at the front of these
        ('whitespace s . _) e
        ('let_keyword s . _) e
        ('in_keyword s . _) e
        ('case_keyword s . _) e
        ('of_keyword s . _) e
        ('rdbl_arrow s . _) e
        ('where_keyword s . _) e
        ('if_keyword s . _) e
        ('then_keyword s . _) e
        ('else_keyword s . _) e
        ('integer s . _) e
        ('constructor s . _) e
        ('identifier s . _) e
        ('comma s . _) e
        ('comment s . _) e
        ('semicolon s . _) e
        ('equals s . _) e
        ('lambda s . _) e
        ('operator s . _) e
        ('lparen s . _) e
        ('rparen s . _) e
        ('lsb s . _) e
        ('rsb s . _) e
        ('lcb s . _) e
        ('rcb s . _) e
        ))))
;(tracefun general-recurser)

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

#|
; Returns (value) or #f
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
|#

(define (case-clause-unbinarize-2 e)
  (mtch e
    ('case_clauses ('case_clause . rest)) `((case_clause . ,rest))
    ('case_clauses ('case_clause . rest) semicolon as) (cons `(case_clause . ,rest) (case-clause-unbinarize-2 as)) ))
(define (case-clause-unbinarize-1 e)
  (mtch e
    ('case_clauses . _) `(case-clauses-list ,(case-clause-unbinarize-2 e))
    x x))
(define (case-clause-unbinarize e) (general-recurser case-clause-unbinarize-1 id e))

  ;(definition . ((exp equals exp)))
  ;(decls . ((definition semicolon decls) (definition)))
(define (decls-unbinarize-2 e)
  (mtch e
    ('decls d) (list d)
    ('decls d semicolon ('decls . rest)) (cons d (decls-unbinarize-2 `(decls . ,rest)))))
(define (decls-unbinarize-1 e)
  (mtch e
    ('decls . _) `(decls-list ,(decls-unbinarize-2 e))
    x x))
(define (decls-unbinarize e) (general-recurser decls-unbinarize-1 id e))
;(tracefun decls-unbinarize decls-unbinarize-1)

(define (flatten-base-exp-seq e)
  (mtch e
    ('base-exp-seq ('base-exp e) d)
      (cons e (flatten-base-exp-seq d))
    ('base-exp-seq ('base-exp e))
      (list e)))
(define (base-exp-seq-unbinarize-1 e)
  (mtch e
    ('base-exp-seq . d) `(app ,(flatten-base-exp-seq e))
    x x))
(define (base-exp-seq-unbinarize e) (general-recurser base-exp-seq-unbinarize-1 id e))

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

(define (p2s e)
  (mtch e
    ('plet ('let_keyword . x) ('lcb . x) decls ('rcb . x) ('in_keyword . x) exp)
      `(let ,(p2s decls) ,(p2s exp))
    ('pwhere exp ('where_keyword . x) ('lcb . x) decls ('rcb . x))
      `(where ,(p2s decls) ,(p2s exp))
    ('pif ('if_keyword . x) b ('then_keyword . x) t ('else_keyword . x) e)
      `(if ,(p2s b) ,(p2s t) ,(p2s e))
    ('exp x)
      (p2s x)
    ('case case_keyword exp of_keyword lcb ('case-clauses-list case_clauses) rcb)
      `(case ,(p2s exp) ,(map p2s case_clauses))
    ('case_clause pat _ exp)
      `(case_clause ,(p2s pat) ,(p2s exp))
    ('listexp ('lsb . _) cses ('rsb . _))
      (p2s (un-cses cses))
    ('definition pat e body)
      `(definition ,(p2s pat) ,e ,(p2s body))
    ('decls-list defs)
      ;`(decls-list (map p2s defs))
      (map p2s defs)
    ('app os)
      `(app ,(map p2s os))
    ('identifier . _)
      e
    ('integer . _)
      e
    ('operator . _)
      e
    ('parenexp l e r)
      `(parenexp ,l ,(p2s e) ,r)
    ('constructor . _)
      e
    ('lambda-exp sym pat body)
      `(lambda-exp ,sym ,(p2s pat) ,(p2s body))
      ))
;(tracefun p2s)

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
(define (unparenexp e) (general-recurser unparenexp-1 id e))
;(define unparenexp id)

; At this point all expressions are (app (a b c)) where b is an operator, including $$.
; There may also be (app (a)).
; Convert $$-chains to regular multi-arg app nodes, and the rest to binops.
(define (separate-app-op sem)
  (mtch sem
    ('let bindings body)
      `(let ,(map separate-app-op bindings) ,(separate-app-op body))
    ('where bindings body)
      `(where ,(map separate-app-op bindings) ,(separate-app-op body))
    ('case exp ccs)
      `(case ,(separate-app-op exp) ,(map separate-app-op ccs))
    ('case_clause pat exp)
      `(case_clause ,(separate-app-op pat) ,(separate-app-op exp))
    ('if b t e)
      `(if ,(separate-app-op b) ,(separate-app-op t) ,(separate-app-op e))
    ('definition a ('equals . d) b)
      `(definition ,(separate-app-op a) (equals . ,d) ,(separate-app-op b))
    (a ('equals . d) b)
      `(,(separate-app-op a) (equals . ,d) ,(separate-app-op b))
    ('app (a ('operator "$$" . d) b))
      `(app ,(map separate-app-op (unfold-real-app sem)))
    ('app (a ('operator . d) b))
      `(binop ,(separate-app-op a) (operator . ,d) ,(separate-app-op b))
    ('parenexp l e r)
      `(parenexp ,l ,(separate-app-op e) ,r)
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

(define lambda-symgen (tagged-symbol-generator-generator 'lambda))

(define (lambda->let-1 e)
  (mtch e
    ;('lambda-exp _ h _) (err h)
    ('lambda-exp _ ('parenexp _ ('app pat) _) body)
      (let ((name (lambda-symgen)))
        `(let ((definition (app ,(cons `(identifier ,(symbol->string name)) pat))
                (equals "=")
                ,body))
              ;(equals "=")
              (app ((identifier ,(symbol->string name))))))
     x x))
(define (lambda->let e) (general-recurser lambda->let-1 id e))

(define (postprocess e)
  (let ((ee (general-recurser (lambda (x) x) (lambda (x) x) e)))
    (if (not (equal? e ee)) (err 'yeah e ee) '()))
  (unparenexp (separate-app-op (precedence (lambda->let (p2s (base-exp-seq-unbinarize (decls-unbinarize (case-clause-unbinarize e)))))))))

#|
(define (top-parse gram nt os)
  ;(shew 'parse os)
  (let ((gram (binarize gram)))
    ;(shew gram)
    (mtch (parse gram nt (list->vector os) 0 (length os) (make-hash))
      (value) `(,value)
      #f #f)))
|#

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

#|
(define no-overture #f)
(define (add-overture s)
  (if no-overture s
    (string-append (read-file-as-string "overture.tmi") "\n" s)))
|#

; Split into tlfs and parse separately; won't work on already-preprocessed code
; (if it lacks proper layout) (define (parse-file filename).
;
; TODO: if a tlf fails to parse, don't keep parsing the rest of the lines.
#|
(define (parse-file filename)
  (let ((tokens (wrap-file (tokenize-top (add-overture (read-file-as-string filename))))))
    (mtch (maybe-list (map (lambda (tlf) (top-parse gram 'definition (preprocess-top tlf)))
                        (split-into-tlfs tokens)))
      (value) `((let ,(map postprocess value) (app ((identifier "main")))))
      #f #f)))
|#

; add overture, tokenize, split tlfs, preprocess, parse, postprocess

(define (split-into-tlfs tokens)
  (group-by-starts (lambda (token) (mtch token (_ _ (line column)) (eq? column 0))) tokens))

;(hook-with timing-hook parse-file tokenize-top preprocess-top top-parse binarize postprocess)
;(hook-with timing-hook parse-file)
