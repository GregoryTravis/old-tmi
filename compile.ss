(define start-time (current-milliseconds))
;(require errortrace)
(dload "lib.ss")
(dload "mtch.ss")
(dload "preprocess.ss")
(dload "tokenize.ss")
(dload "packrat.ss")
(dload "parse.ss")
(dload "precedence.ss")
(dload "pretty-print.ss")
(dload "native-rel.ss")

;; This is meant to be included in the compiled program, but since we're just
;; running in here in the compiler process, we just load it.
(dload "native-preamble.ss")

(dload "native-web.ss")

;; Disables all includes and dumps things
(define debug-compile #f)

;; Interpreted is faster
(define use-interpreted-pattern-matching #t)

; Returns map from function name to list of alternate funs
(define generate-tlf-lookup #f)
(define (compile-let sem)
  (mtch sem
    ('let bindings exp)
      (let ((multilambdas (group-as-multilambdas bindings)))
        `(letrec
          ,(append
             (if generate-tlf-lookup
               `((tmi-lookup-tlf-by-name ,(build-eval multilambdas)))
               '())
             (map compile-multilambda-group multilambdas))
          ,(compile-exp exp)))))

; Generates a lookup function that will return foo given "foo", and excludes "main" because
; it runs during definition of main so main is not defined yet
(define (build-eval multilambdas)
  (let ((names (map
    (lambda (ml)
      (mtch ml
        (('fun name) . defs)
          name))
    (grep (lambda (ml) (mtch ml (('fun . _) . _) #t x #f)) multilambdas))))
    `(lambda (name-string)
       (let ((thunk
         (cdr (assoc name-string
            (list . ,(map (lambda (name) `(cons ,name (lambda () ,(string->symbol name)))) (rember "main" names)))))))
         (thunk)))))

(define case-symgen (tagged-symbol-generator-generator 'case))

(define (group-as-multilambdas bindings)
  (group-byy
    (lambda (binding)
      (mtch binding
        ('definition ('identifier name . d) ('equals . _) body)
          `(fun ,name)
        ('definition ('app (('identifier name . d) . d)) ('equals . _) body)
          `(fun ,name)
        ('definition ('binop lpat ('operator op . _) rpat) ('equals . _) body)
          `(binop ,op)))
    bindings))

; Takes ("name" def def def), returns scheme letrec (name val) form
(define (compile-multilambda-group ml)
  (mtch ml
    (('fun name) . defs)
      `(,(string->symbol name) ,(compile-multilambda name defs))
    (('binop name) . defs)
      `(,(string->symbol (++ "op" name)) ,(compile-multilambda name defs))))

(define pm-symgen (tagged-symbol-generator-generator 'pm))

(define trace-def-enabled #f)
(define (trace-def name src)
  (if trace-def-enabled
    `(procedure-rename (tracefun-wrap ',(string->symbol name) ,src) ',(string->symbol name))
    src))

(define (compile-multilambda name ml)
  ;; TODO check for duplicate patterns here
  (mtch ml
    (('definition ('identifier . d)
      (equals . d)
      body))
     (compile-exp body)
    _
      (let ((args (pm-symgen))
            (src-extent (get-src-extent ml)))
        (let ((failer
            `(begin
               (display "WTF: ")
               (display (cons ',(string->symbol name) ,args))
               (display "\n")
               (show-tmi-src ',src-extent combined-src-gooogoo)
               (error "pattern match failure" gooogoo ',src-extent (cons ',(string->symbol name) ,args)))))
          (trace-def name `(lambda ,args ,(compile-multilambda-1 `(,name ,args) args ml failer)))))))

(define (cm-args-pat e)
  (mtch e
    ('app (('identifier . d) . pat))
      pat
    ('identifier . d)
      '()
    ('binop lpat ('operator . _) rpat)
      `(,lpat ,rpat)))
(define (compile-multilambda-1 name args ml failer)
  (mtch ml
    (('definition
      dpat
      (equals . d)
      body)
     . the-rest)
     (let ((vresult (pm-symgen)))
       `(let ((,vresult
           ,(if use-interpreted-pattern-matching
              (compile-app-pattern args (cm-args-pat dpat) `(list ,(compile-exp body) 'hhh))
              (compile-int-app-pattern args (cm-args-pat dpat) body))))
          (if (eq? ,vresult #f)
              ,(compile-multilambda-1 name args the-rest failer)
              (car ,vresult))))
    ; End of the list; nothing has matched, so crash
    '() failer))

(define (compile-int-app-pattern args1 dpat1 body1)
  (let ((bvar (pm-symgen)))
    `(let ((,bvar (tmi-match-app-pattern ,args1 ',dpat1)))
       (if (eq? ,bvar #f)
         #f
         (list (apply ,(compile-body-with-lambda dpat1 body1) ,bvar))))))
;(tracefun compile-body-with-lambda compile-int-app-pattern)

(define (compile-body-with-lambda pat body)
  `(lambda ,(tmi-gather-vars-app-pattern pat) ,(compile-exp body)))

(define (tmi-gather-vars-app-pattern pat1)
  (mtch pat1
    (a . d)
      (let ((a-bindings (tmi-gather-vars-pattern a)))
        (if (eq? a-bindings #f)
          #f
          (let ((d-bindings (tmi-gather-vars-app-pattern d)))
            (if (eq? d-bindings #f)
              #f
              (append a-bindings d-bindings)))))
    '()
      '()))

(define (tmi-gather-vars-pattern pat1)
  (mtch pat1
    ('identifier name . _)
      `(,(string->symbol name))
    ('integer value . _)
       '()
    ('string value . _)
      '()
    ('constructor value . _)
       '()
    ('app (('constructor . cd) . ad))
      (tmi-gather-vars-app-pattern `((constructor . ,cd) . ,ad))
    ('app (('identifier s . cd) ('identifier vars . vs)))
      `(,(string->symbol vars))))
      ;(tmi-gather-vars-app-pattern `((identifier . ,cd) ,var))))
;(tracefun tmi-gather-vars-app-pattern tmi-gather-vars-pattern compile-body-with-lambda)

(define (tmi-match-app-pattern args1 pat1)
  (mtch pat1
    (a . d)
      (if (pair? args1)
        (let ((a-bindings (tmi-match-pattern (car args1) a)))
          (if (eq? a-bindings #f)
            #f
            (let ((d-bindings (tmi-match-app-pattern (cdr args1) d)))
              (if (eq? d-bindings #f)
                #f
                (append a-bindings d-bindings)))))
        #f)
    '()
      (if (null? args1)
        '()
        #f)))

(define (tmi-match-pattern target1 pat1)
  (mtch pat1
    ('identifier name . _)
      `(,target1)
    ('integer value . _)
      (if (equal? target1 (string->number value))
         '()
         #f)
    ('string value . _)
      (if (equal? target1 (strip-quotes value))
        '()
        #f)
    ('constructor value . _)
      (if (equal? target1 (string->symbol value))
         '()
         #f)
    ('app (('constructor . cd) . ad))
      (tmi-match-app-pattern target1 `((constructor . ,cd) . ,ad))
    ('app (('identifier s . cd) var))
      (let ((predicate (eval (string->symbol (++ "t-" (string->symbol s) "?")))))
        (if (predicate target1)
          `(,target1)
          #f))))
      ;(tmi-match-app-pattern target1 `((identifier . ,cd) ,var))))
;(tracefun tmi-match-pattern tmi-match-app-pattern)

; omg I hate myself, these 1s are there because my mtch macro
; is not hygienic
(define (compile-app-pattern target1 pat1 body1)
  (mtch pat1
    (a . d)
      (let ((va (pm-symgen))
            (vd (pm-symgen)))
      `(if (pair? ,target1)
         (let ((,va (car ,target1))
               (,vd (cdr ,target1)))
           ,(compile-pattern va a
              (compile-app-pattern vd d body1)))
         #f))
    '()
      `(if (eq? '() ,target1)
         ;(list ,(compile-exp body1) 'fff)
         ;,(compile-exp body1)
         ,body1
         #f)))

(define (compile-type-pattern target1 e body1)
  (mtch e
    (('identifier type . _) ('identifier var . _))
      `(if (,(string->symbol (string-append "t-" type "?")) ,target1)
          (let ((,(string->symbol var) ,target1))
            ,body1)
          #f)))

(define (compile-pattern target1 pat1 body1)
  (mtch pat1
    ('identifier name . _)
      `(let ((,(string->symbol name) ,target1))
         ,body1)
    ('integer value . _)
      `(if (equal? ,target1 ,(string->number value))
         ,body1
         #f)
    ('string value . _)
      `(begin
      (if (equal? ,target1 ,(strip-quotes value))
         ,body1
         #f)
         )
    ('constructor value . _)
      `(if (equal? ,target1 (quote ,(string->symbol value)))
         ,body1
         #f)
    ('app (('constructor . cd) . ad))
      (compile-app-pattern target1 `((constructor . ,cd) . ,ad) body1)
    ('app (('identifier . cd) var))
      (compile-type-pattern target1 `((identifier . ,cd) ,var) body1)))
;(tracefun compile-pattern-and-body)

(define (compile-hash-literal entries)
  `(coll-make-hash
     (list . ,(map (lambda (e) (mtch e ('hash-entry ('identifier i . _) e)
                                       `(cons ',(string->symbol i) ,(compile-exp e))))
                 entries))))

(define (get-src-extent src)
  (let ((tokens (find-tokens src)))
    (if (> (length tokens) 0)
      (mtch (list (car tokens) (last tokens))
        ((typea sa (rowa cola)) (typeb sb (rowb colb)))
          `((,rowa ,cola) (,rowb ,colb))
        x '())
      '())))

(define (find-tokens src)
  (mtch src
    ((type s (row col)) . d)
      (if (and (integer? row) (integer? col))
        (cons (car src) (find-tokens (cdr src)))
        (append (find-tokens (car src)) (find-tokens (cdr src))))
    (a . d)
      (append (find-tokens a) (find-tokens d))
    x '()))
;(tracefun get-src-extent find-tokens)

(define stack-trace-push-pop-enabled #t)
(define (stack-trace-push-pop src compiled)
  (if stack-trace-push-pop-enabled
      (mtch (get-src-extent src)
        (start end)
        ;(let ((src-text (tokens->src (find-tokens src))))
          (let ((result-v (pm-symgen)))
            `(begin
               ;(shew (list 'push '(,start ,end) ',src))
               (set! tmi-stack (cons '(,start ,end) tmi-stack))
               (let ((,result-v ,compiled))
                 ;(shew (list 'pop '(,start ,end) ,result-v))
                 (set! tmi-stack (cdr tmi-stack))
                 ,result-v)));)
        x compiled)
    compiled))

(define (compile-exp e)
  (stack-trace-push-pop e
    (mtch e
      ('identifier name . d)
        (string->symbol name)
      ('integer s . d)
        (string->number s)
      ('string s . d)
        (strip-quotes s)
      ('app (('constructor name . d)))
        `',(string->symbol name)
      ('app (('constructor name . d) . cton-args))
        `(cons (quote ,(string->symbol name)) (list . ,(map compile-exp cton-args)))
      ('binop ('constructor name . d) (operator "$" . _) e)
        (let ((args (pm-symgen)))
          `((lambda ,args (cons (quote ,(string->symbol name)) ,args)) ,(compile-exp e)))
      ;; (('constructor name . d) . cton-args)
      ;;  (let ((args (pm-symgen)))
      ;;    `((lambda ,args (cons (quote ,(string->symbol name)) ,args)) . ,(map compile-exp cton-args)))
      ;; This only applies to naked constructors not in head position
      ('constructor name . d)
        `',(string->symbol name)
      ;; ('app (('constructor ctor . d)))
      ;;  `(quote ,(string->symbol ctor))
      ('app (e))
        (compile-exp e)
      ('app es)
        (no-bad-funs (map compile-exp es))
      ('binop a ('operator op . _) b)
        (if (is-short-circuit op)
          `(,(string->symbol (string-append "op" op)) ,(compile-thunk (compile-exp a)) ,(compile-thunk (compile-exp b)))
          `(,(string->symbol (string-append "op" op)) ,(compile-exp a) ,(compile-exp b)))
      ('unop ('unary-operator op . _) e)
        `(,(string->symbol (string-append "op" op)) ,(compile-exp e))
      ('if b t e)
        `(if (tmi-if ,(compile-exp b)) ,(compile-exp t) ,(compile-exp e))
      ('let . _)
        (compile-let e)
      ('where . e)
        (compile-let `(let . ,e))
      ('hash entries)
        (compile-hash-literal entries)
      )))
;(tracefun compile-exp)

(define (compile-thunk ce)
  `(lambda () ,ce))

(define (is-short-circuit op)
  (or (equal? op "&&") (equal? op "||")))

;; All racket functions are available to be called directly from TMI; it's only
;; care and discipline that makes this work, thus it is wrong.  Most of the time
;; this isn't too bad, but for and/or it's terrible, since all TMI booleans are
;; true in Racket.  So just don't allow these.
(define (no-bad-funs app)
  (assert (not (or (eq? (car app) 'or) (eq? (car app) 'and))))
  app)

(define (strip-quotes s)
  (assert (eq? (string-ref s 0) #\"))
  (assert (eq? (string-ref s (- (string-length s) 1)) #\"))
  (substring s 1 (- (string-length s) 1)))

(define (case-clause->definition casefun-name cc)
  (mtch cc
    ('case_clause pat exp)
      `(definition (app ((identifier ,(symbol->string casefun-name)) ,pat)) (equals "=") ,exp)
    x x))

(define (compile-simplify-1 e)
  (mtch e
    ('case exp clauses)
      (let ((casefun-name (case-symgen)))
        `(let
          ,(map (lambda (cc) (case-clause->definition casefun-name cc)) clauses)
          (app ((identifier ,(symbol->string casefun-name)) ,exp))))
    ('pdo '() exp)
      exp
    ('pdo (('do_assignment pat body) . assignments) exp)
      (begin
        `(app ((constructor "Seq") ,body (lambda-exp (app (,pat)) (pdo ,assignments ,exp)))))
    ('lambda-exp ('app pat) body)
      (let ((name (lambda-symgen)))
        `(let ((definition (app ,(cons `(identifier ,(symbol->string name)) pat))
                (equals "=")
                ,body))
              ;(equals "=")
              (app ((identifier ,(symbol->string name))))))
    ('left-section op e)
      (let ((param `(identifier ,(symbol->string (pm-symgen)))))
        (compile-simplify
          `(lambda-exp (app (,param)) (binop ,param ,op ,e))))
    ('right-section e op)
      (let ((param `(identifier ,(symbol->string (pm-symgen)))))
        (compile-simplify
          `(lambda-exp (app (,param)) (binop ,e ,op ,param))))
    ('both-section op)
      (let ((lparam `(identifier ,(symbol->string (pm-symgen))))
            (rparam `(identifier ,(symbol->string (pm-symgen)))))
        (compile-simplify
          `(lambda-exp (app (,lparam ,rparam)) (binop ,lparam ,op ,rparam))))
    x x))
(define (compile-simplify e) (general-recurser-s compile-simplify-1 id e))

(define (show-tmi-src extent combined-src)
  (let ((src-lines (string-split combined-src "\n")))
    (mtch extent
      ((srow scol) (erow ecol))
        (begin
          (display (++ "** " (->string erow) ":\n"))
          (map (lambda (line) (display (++ line "\n")))
            (nth-range srow (1+ erow) src-lines))))))
(define (show-tmi-stack-trace stack combined-src)
  (let ((src-lines (string-split combined-src "\n")))
    (map (lambda (frame)
      (mtch frame
        ((srow scol) (erow ecol))
          (begin
            (display (++ "** " (->string erow) ":\n"))
            (map (lambda (line) (display (++ line "\n")))
              (nth-range srow (1+ erow) src-lines)))))
      stack)))
(define (wrap-main main combined-src)
  `(let ((tmi-stack '())
         (gooogoo '(2 3 4))
         (combined-src-gooogoo ,combined-src))
     (with-handlers ((exn?
         (lambda (e) (show-tmi-stack-trace tmi-stack ,combined-src) (raise e))))
       (driver-main ,main))))

(define (compile sem combined-src)
  (if debug-compile (shew 'sem sem) '())
  (let ((simple (compile-simplify sem)))
    (if debug-compile (shew 'simple simple) '())
    (let ((compiled (compile-let simple)))
      (if debug-compile (shew 'compiled compiled) '())
      (wrap-main compiled combined-src))))

(define (compile-file filename)
  ; Not sure where this extra list comes from
  (mtch (parse-file filename)
    (sem combined-src) (compile sem combined-src)))

(define (compile-scheme-compiled c)
  (eval `(lambda () ,c)))
(define (execute-compiled-scheme o)
  (o))
(define (run-compiled c)
  ;(shew c)
  (execute-compiled-scheme (compile-scheme-compiled c)))

;(hook-with timing-hook parse-file compile run-compiled timing-hook execute-compiled-scheme compile-scheme-compiled)

;(assert (eq? (vector-length (current-command-line-arguments)) 1))
(define (main filename)
  (with-handlers ((exn? (lambda (e) (shew 'gosh e))))
    (begin
      (display (tmi-pretty-print (run-compiled (compile-file filename))))
      (display "\n"))))
;(hook-with timing-hook main)
;(main)
