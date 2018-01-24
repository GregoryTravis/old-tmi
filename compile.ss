(load "Lib.ss")
(load "mtch.ss")
(load "native-preamble.ss")
(load "parse.ss")

; Returns map from function name to list of alternate funs
(define (compile-let sem)
  (mtch sem
    ('let bindings exp)
      `(letrec
        ,(map compile-multilambda-group (group-as-multilambdas bindings))
        ,(compile-exp exp))))

(define (group-as-multilambdas bindings)
  (group-byy
    (lambda (binding)
      (mtch binding
        (('app (('identifier name . d) . d)) ('equals . _) body)
          name
        #|
        ;; No-args case
        ;(('identifier name . d) ('equals . _) body)
          ;name))
          |#
          ))
    bindings))

; Takes ("name" def def def), returns scheme letrec (name val) form
(define (compile-multilambda-group ml)
  (mtch ml
    (name . defs)
    `(,(string->symbol name) ,(compile-multilambda defs))))

(define pm-symgen (tagged-symbol-generator-generator 'pm))

(define (compile-multilambda ml)
  ;; TODO check for duplicate patterns here
  (mtch ml
    ((('app (('identifier . d)))
      (equals . d)
      body))
     (compile-exp body)
    _
      (let ((args (pm-symgen)))
        `(lambda ,args ,(compile-multilambda-1 args ml)))))

(define (compile-multilambda-1 args ml)
  (mtch ml
    ((('app (('identifier . d) . pat))
      (equals . d)
      body)
     . the-rest)
     (let ((vresult (pm-symgen)))
       `(let ((,vresult ,(compile-app-pattern args pat `(list ,(compile-exp body) 'hhh))))
          (if (eq? ,vresult #f)
              ,(compile-multilambda-1 args the-rest)
              (car ,vresult))))
   ; End of the list; nothing has matched, so crash
   '() `(failllalalala)))

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
    ('app es)
      (compile-app-pattern target1 es body1)))
;(tracefun compile-pattern-and-body)

(define (compile-exp e)
  (mtch e
    ('identifier name . d)
      (string->symbol name)
    ('integer name . d)
      (string->number name)
    ('constructor name . d)
      `(lambda args (cons (quote ,(string->symbol name)) args))
    ('app (('constructor ctor . d)))
      `(quote ,(string->symbol ctor))
    ('app (e))
      (compile-exp e)
    ('app es)
      (map compile-exp es)
    ('binop a ('operator op . _) b)
      `(,(string->symbol op) ,(compile-exp a) ,(compile-exp b))
    ('if b t e)
      `(if ,(compile-exp b) ,(compile-exp t) ,(compile-exp e))
    x x
    ))
;(tracefun compile-exp)

(define (compile sem)
  (shew 'sem sem)
  (let ((compiled (compile-let sem)))
    (shew compiled)
    (let ((all `(begin ,native-preamble ,compiled)))
      ;(shew 'all all)
      (eval all))))

(define (compile-file filename)
  ; Not sure where this extra list comes from
  (mtch (parse-file filename)
    (sem) (compile sem)))

(assert (eq? (vector-length (current-command-line-arguments)) 1))
(shew (compile-file (vector-ref (current-command-line-arguments) 0)))
