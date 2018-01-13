(load "Lib.ss")
(load "mtch.ss")
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
        ; No-args case
        (('identifier name . d) ('equals . _) body)
          name))
    bindings))

; Takes ("name" def def def), returns scheme letrec (name val) form
(define (compile-multilambda-group ml)
  (mtch ml
    (name . defs)
    `(,(string->symbol name) ,(compile-multilambda defs))))

(define pm-symgen (tagged-symbol-generator-generator 'pm))

(define (compile-multilambda ml)
  (let ((args (pm-symgen)))
    `(lambda ,args ,(compile-multilambda-1 args ml))))

(define (compile-multilambda-1 args ml)
  (mtch ml
    (((app ((identifier . d) . pat))
      (equals . d)
      body)
     . the-rest)
     (let ((vresult (pm-symgen)))
       `(let ((,vresult ,(compile-pattern-and-body args pat body)))
          (if (eq? ,vresult #f)
              ,(compile-multilambda-1 args the-rest)
              (car ,vresult))))
    ; No-args case
    (((identifier . d) (equals . dd) body) . the-rest)
      (compile-multilambda-1 args
        `(((app ((identifier . ,d)))
           (equals . ,dd)
           ,body)
          . ,the-rest))

   ; End of the list; nothing has matched, so crash
   '() `(failllalalala)))

; omg I hate myself, these 1s are there because my mtch macro
; is not hygienic
(define (compile-pattern-and-body target1 pat1 body1)
  (mtch pat1
    ((identifier name . _) . d)
      (let ((vd (pm-symgen)))
        `(if (pair? ,(compile-exp target1))
           (let ((,(string->symbol name) (car ,target1))
                 (,vd (cdr ,target1)))
             ,(compile-pattern-and-body vd d body1))
           #f))
    ; No-args case
    (identifier name . _)
      `(if (eq? '() ,target1)
         (list ,(compile-exp body1))
         #f)
    '()
      `(if (eq? '() ,target1)
         (list ,(compile-exp body1))
         #f)))
;(tracefun compile-pattern-and-body)

(define (compile-exp e)
  (mtch e
    ('identifier name . d)
      (string->symbol name)
    ('integer name . d)
      (string->number name)
    ('app es)
      (map compile-exp es)
    x x))
(tracefun compile-exp)

(define (compile sem)
  (shew sem)
  (let ((compiled (compile-let sem)))
    (shew compiled)
    (shew ((eval compiled)))
    compiled))

(define (compile-file filename)
  ; Not sure where this extra list comes from
  (mtch (parse-file filename)
    (sem) (compile sem)))

(assert (eq? (vector-length (current-command-line-arguments)) 1))
(shew (compile-file (vector-ref (current-command-line-arguments) 0)))
