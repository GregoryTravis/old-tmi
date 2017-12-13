(require compatibility/defmacro)
(define-for-syntax mtch-show-expansion #f)

(define-for-syntax (shew . args) (map pretty-print args))

(define-for-syntax (mtch-literal? o)
  (or ;(is-quote? o)
      (string? o)
      (number? o)
      (null? o)
      (eq? #t o)
      (eq? #f o)))

(define-for-syntax (mtch-is-quote? o)
  (and (pair? o)
       (eq? (car o) 'quote)
       (pair? (cdr o))
       (null? (cddr o))))

(define-for-syntax mtch-vargen
  (let ((serial 0))
    (lambda ()
      (let ((s serial))
        (set! serial (+ serial 1))
        (string->symbol (string-append "_m" (number->string s)))))))

(define-for-syntax (mtch-clause target pat body)
  (let ((carvar (mtch-vargen))
        (cdrvar (mtch-vargen)))
    (cond
     ((mtch-is-quote? pat)
      `(if (equal? ',(cadr pat) ,target)
           ,body
           (fail)))     ((pair? pat)
      `(if (pair? ,target)
           (let ((,carvar (car ,target))
                 (,cdrvar (cdr ,target)))
             ,(mtch-clause carvar
                          (car pat)
                          (mtch-clause cdrvar
                                      (cdr pat)
                                      body)))
           (fail)))
     ((eq? '_ pat) body)
     ((symbol? pat)
      `(let ((,pat ,target)) ,body))
     ((mtch-literal? pat)
      `(if (equal? ',pat ,target)
           ,body
           (fail)))
     (#t (err 'mtch-clause target pat body)))))

(define-for-syntax (mtch-clauses target clauses all-clauses)
  (if (null? clauses)
      `(err 'match-failure target ',all-clauses)
      (let ((pat (car clauses))
            (body (cadr clauses))
            (rest (cddr clauses)))
        (let ((next (mtch-clauses target rest all-clauses)))
          `(let ((fail (lambda () ,next)))
             ,(mtch-clause target pat body))))))

(define-for-syntax (mtch-render target clauses)
  (let ((code
         `(let ((target ,target))
            ,(mtch-clauses 'target clauses clauses))))
    (if mtch-show-expansion
        (begin
          (pretty-print 'mtch)
          (pretty-print target)
          (pretty-print clauses)
          (pretty-print code))
        '())
    code))

(define-macro (mtch target . clauses)
  (mtch-render target clauses))
