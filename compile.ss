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
;; This is meant to be included in the compiled program, but since we're just
;; running in here in the compiler process, we just load it.
(dload "native-preamble.ss")

; Returns map from function name to list of alternate funs
(define (compile-let sem)
  (mtch sem
    ('let bindings exp)
      `(letrec
        ,(map compile-multilambda-group (group-as-multilambdas bindings))
        ,(compile-exp exp))))

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

(define (compile-multilambda name ml)
  ;; TODO check for duplicate patterns here
  (mtch ml
    (('definition ('identifier . d)
      (equals . d)
      body))
     (compile-exp body)
    _
      (let ((args (pm-symgen)))
        `(lambda ,args ,(compile-multilambda-1 `(,name ,args) args ml)))))

(define (cm-args-pat e)
  (mtch e
    ('app (('identifier . d) . pat))
      pat
    ('identifier . d)
      '()
    ('binop lpat ('operator . _) rpat)
      `(,lpat ,rpat)))
(define (compile-multilambda-1 name args ml)
  (mtch ml
    (('definition
      dpat
      (equals . d)
      body)
     . the-rest)
     (let ((vresult (pm-symgen)))
       `(let ((,vresult ,(compile-app-pattern args (cm-args-pat dpat) `(list ,(compile-exp body) 'hhh))))
          (if (eq? ,vresult #f)
              ,(compile-multilambda-1 name args the-rest)
              (car ,vresult))))
   ; End of the list; nothing has matched, so crash
   '() `(raise `(pattern-match-failure ,,(cons 'cons name)))))

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

(define (compile-exp e)
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
      (let ((args (pm-symgen)))
        `((lambda ,args (cons (quote ,(string->symbol name)) ,args)) . ,(map compile-exp cton-args)))
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
      (map compile-exp es)
    ('binop a ('operator op . _) b)
      `(,(string->symbol (string-append "op" op)) ,(compile-exp a) ,(compile-exp b))
    ('if b t e)
      `(if ,(compile-exp b) ,(compile-exp t) ,(compile-exp e))
    ('let . _)
      (compile-let e)
    ('where . e)
      (compile-let `(let . ,e))
    ('hash entries)
      (compile-hash-literal entries)
    ))
;(tracefun compile-exp)

(define (strip-quotes s)
  (assert (eq? (string-ref s 0) #\"))
  (assert (eq? (string-ref s (- (string-length s) 1)) #\"))
  (substring s 1 (- (string-length s) 1)))

(define (case-clause->definition casefun-name cc)
  (mtch cc
    ('case_clause pat exp)
      `(definition (app ((identifier ,(symbol->string casefun-name)) ,pat)) (equals "=") ,exp)
    x x))

;; Handle both 1-arg and 2+-arg lambda arg patterns
(define (lambda-pat-args e)
  (mtch e
    ('app pat) pat
    pat `(,pat)))

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
        (mtch pat ('identifier . _) #t) ;; Assertion
        `(app ((constructor "Seq") ,body (lambda-exp ,pat (pdo ,assignments ,exp)))))
    ('lambda-exp pat body)
      (let ((name (lambda-symgen)))
        `(let ((definition (app ,(cons `(identifier ,(symbol->string name)) (lambda-pat-args pat)))
                (equals "=")
                ,body))
              ;(equals "=")
              (app ((identifier ,(symbol->string name))))))
    x x))
(define (compile-simplify e) (general-recurser-s compile-simplify-1 id e))

(define (wrap-main main)
  `(driver-main ,main))

(define (compile sem)
  ;(shew 'sem sem)
  (let ((simple (compile-simplify sem)))
    ;(shew 'simple simple)
    (let ((compiled (compile-let simple)))
      ;(shew compiled)
      (wrap-main compiled))))

(define (compile-file filename)
  ; Not sure where this extra list comes from
  (mtch (parse-file filename)
    (sem) (compile sem)))

(define (run-compiled c)
  ;(shew c)
  (eval c))

(define unconsify-magic 'ZC45$2E)

; Unconsify Cons lists throughout e.
(define (unconsify e)
  (mtch e
    ('Cons a d) (unconsify-cons e)
    (a . d) (map unconsify e)
    x x))

; Convert (Cons 1 (Cons 2 ... Nil)) format to (<magic> 1 2 ... <magic>) format.
; The magic will be replaced with square brackets later.
(define (unconsify-cons e)
  (cons unconsify-magic (append (unconsify-cons-1 e) (list unconsify-magic))))
(define (unconsify-cons-1 e)
  (mtch e
    ('Cons a d) (cons (unconsify a) (unconsify-cons-1 d))
    'Nil '()))

; Convert parens with magic tokens to [].
(define pretty-shew-postprocess-rewrites
  `((,(string-append "(" (symbol->string unconsify-magic) " ") "[")
    (,(string-append "(" (symbol->string unconsify-magic) "\n") "[\n")
    (,(string-append " " (symbol->string unconsify-magic) ")") "]")))
(define (pretty-shew-postprocess s)
  (apply-string-rewrites s pretty-shew-postprocess-rewrites))

; pretty-print, but with [] lists
(define (pretty-shew o)
  (if (not (void? o))
    (let ((op (open-output-string)))
      (pretty-print (unconsify o) op)
      (display (pretty-shew-postprocess (get-output-string op))))
    '()))

;(hook-with timing-hook parse-file compile run-compiled)

;(assert (eq? (vector-length (current-command-line-arguments)) 1))
(define (main filename)
  (display (tmi-pretty-print (run-compiled (compile-file filename))))
  (display "\n"))
;(hook-with timing-hook main)
;(main)
