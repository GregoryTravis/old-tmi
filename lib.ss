(require mzlib/compat)
;(require scheme/system)
;(require (lib "ports-6.ss" "rnrs/io"))
;;(require (lib "simple-6.ss" "rnrs/io")) ; bad one
;(require (lib "32.ss" "srfi"))
;(require (lib "13.ss" "srfi"))
;(require (lib "35.ss" "srfi"))
;(require (lib "defmacro.ss"))
;(require (lib "pregexp.ss"))
;(require (lib "process.ss"))
;(require (lib "compat.ss"))
;(require (for-syntax (lib "pretty.ss")))
;;(require (lib "../errortrace/errortrace.ss"))
;;(require-for-syntax (lib "list.ss"))
(load "mtch.ss")

(define impl-tracefun-indentation 0)
(define trace-stump-level 1000)
(define (trace-indent) (set! impl-tracefun-indentation (+ 1 impl-tracefun-indentation)))
(define (trace-unindent) (set! impl-tracefun-indentation (+ -1 impl-tracefun-indentation)))

(define (tracing-push-print exp)
  (display "        ")
  (display (make-string-string impl-tracefun-indentation "| "))
  (display "+ ")
  (lshew exp)
  (display "\n")
  (flush-output)
  (set! impl-tracefun-indentation (+ impl-tracefun-indentation 1)))

(define (tracing-pop-print exp)
  (set! impl-tracefun-indentation (- impl-tracefun-indentation 1))
  (display "        ")
  (display (make-string-string impl-tracefun-indentation "| "))
  (display "-> ")
  (lshew exp)
  (display "\n")
  (flush-output))

(define (tracefun-hookist name f)
  (lambda args
    (tracing-push-print (cons name args))
    (let ((result (apply f args)))
      (tracing-pop-print result)
      result)))

(define (plain-ol-tracer app runner)
  (display (make-string-string impl-tracefun-indentation "| "))
  (display "+  ")
  (lshew (stump app trace-stump-level))
  (display "\n")
  (trace-indent)
  (flush-output)
  (let ((r (runner)))
    (trace-unindent)
    (display (make-string-string impl-tracefun-indentation "| "))
    (display "-> ")
    (lshew (stump r trace-stump-level))
    (display "\n")
    (flush-output)
    r))

;; untested
;; (define (tracer-filter f tracer)
;;   (lambda (app runner)
;;     (if (f app)
;;         (tracer app runner)
;;         (runner))))

(define (trace-wrap tracer name f)
  (lambda args
    (let ((r '()))
      (tracer (cons name args)
              (lambda ()
                (set! r (apply f args))
                r))
      r)))

(define-macro (tracefun . funs)
  `(tracefun-with plain-ol-tracer ,@funs))

(define-macro (tracefun-with tracer . funs)
  (cons 'begin
        (map (lambda (f)
               `(set! ,f (trace-wrap ,tracer ',f ,f)))
             funs)))

(define-macro (hook-with hookist . funs)
  (cons 'begin
        (map (lambda (f)
               `(set! ,f (,hookist ',f ,f)))
             funs)))

(define-macro (hook-with hookist . funs)
  (cons 'begin
        (map (lambda (f)
               `(set! ,f (,hookist ',f ,f)))
             funs)))

(define (args-and-result-hook cb)
  (lambda (name f)
    (lambda args
      (let ((result (apply f args)))
        (apply cb (append args (list result)))
        result))))

(define (before-and-after-hook b a)
  (lambda (name f)
    (lambda args
      (apply b args)
      (let ((result (apply f args)))
        (apply a (append args (list result)))
        result))))

(define (nth n list)
  (if (= 0 n)
      (car list)
      (nth (- n 1) (cdr list))))

;; Mzscheme already has this, and the args are swapped.
;; (define (take n lyst)
;;   (if (> n 0)
;;       (if (null? lyst)
;;           (err)
;;           (cons (car lyst) (take (1- n) (cdr lyst))))
;;       '()))

(define (nth-tail n lyst)
  (if (= 0 n)
      lyst
      (nth-tail (1- n) (cdr lyst))))

(define (scoop-by n lyst)
  (if (null? lyst)
      '()
      (cons (take lyst n)
            (scoop-by n (nth-tail n lyst)))))

(define concat string-append)

(define (applyer f)
  (lambda (args) (apply f args)))

(define (call-with-output-string pf)
  (let ((p (open-output-string)))
    (pf p)
    (get-output-string p)))

(define (read-objects filename)
  (call-with-input-file filename read-objects-port))

(define (write-string-to-file filename s)
  (call-with-output-file filename
    (lambda (port) (display s port))))

(define (read-objects-port p)
  (letrec ((objects '())
           (read-em
            (lambda ()
              (let ((object (read p)))
                (if (eof-object? object)
                    objects
                    (begin
                      (set! objects (cons object objects))
                      (read-em)))))))
    (reverse (read-em))))

(define (pp . rest)
  (map pretty-print rest))

(define (spp . rest)
  (call-with-output-string
   (lambda (port)
     (map (lambda (o) (pretty-print o port)) rest))))

(define (sdisplay . rest)
  (call-with-output-string
   (lambda (port)
     (map (lambda (o) (display o port)) rest))))

(define (swrite . rest)
  (call-with-output-string
   (lambda (port)
     (map (lambda (o) (write o port)) rest))))

(define (shew . args)
  (map pretty-print args)
  (flush-output))

(define (say . args)
  (display (apply string-append (map ->string args)))
  (display "\n"))

(define (lshew . args)
  (display (apply lsshew args)))

(define (lsshew . args)
  (string-collapse-spaces
   (string-one-line
    (call-with-output-string
     (lambda (port)
       (map (lambda (j) (display j port)) args))))))

(define (proper-list? l)
  (or (eq? l '()) (and (pair? l) (proper-list? (cdr l)))))

;(define (proper-cons? c)
;  (and (pair? c) (or (pair? (cdr c)) (null? (cdr c)))))

(define (proper-tree? t)
  (if (pair? t)
      (and (proper-list? t)
           (all? (map proper-tree? t)))
      #t))

(define (make-dict . args)
  (cond
   ((null? args) (make-dict equal?))
   ((eq? (car args) eq?) (make-dict-1 hashq-ref hashq-set!))
   ((eq? (car args) eqv?) (make-dict-1 hashv-ref hashv-set!))
   ((eq? (car args) equal?) (make-dict-1 hash-ref hash-set!))
   (#t (err))))

(define (make-dict-1 reffer setter)
  (let ((hashtable (make-hash-table))
        (missing (cons '() '())))
    (lambda args
      (cond
       ((and (eq? (car args) 'exists)
             (null? (cddr args)))
        (let ((key (cadr args)))
          (not (eq? (reffer hashtable key missing) missing))))
       ((and (eq? (car args) 'get)
             (null? (cddr args)))
        (let ((key (cadr args)))
          (let ((val (reffer hashtable key missing)))
            (if (eq? val missing)
                (err 'no-such-dict-entry)
                val))))
       ((eq? (car args) 'map)
        (hash-map (cadr args) hashtable))
       ((eq? (car args) 'keys)
        (hash-keys hashtable))
       ((eq? (car args) 'values)
        (hash-values hashtable))
       ((eq? (car args) 'kv-pairs)
                                        ;(shew 'urg hashtable)
        (hash-kv-pairs hashtable))
       ((and (eq? (car args) 'put)
             (null? (cdddr args)))
        (let ((key (cadr args))
              (value (caddr args)))
          (setter hashtable key value)))
       ((and (eq? (car args) 'shew))
        (shew hashtable))
       (#t (err "bad dict method" args))))))

(define (make-autoadd-dict generator)
  (make-autoadd-fun-dict (lambda (dummy) (generator))))

(define (make-autoadd-fun-dict f)
  (let ((dict (make-dict)))
    (lambda args
      (cond
       ((and (eq? (car args) 'get)
             (null? (cddr args)))
        (let ((key (cadr args)))
          (if (not (dict 'exists key))
              (dict 'put key (f key))
              '())
          (dict 'get key)))
       (#t (apply dict args))))))

(define (make-dict-list)
  (let ((dict (make-dict)))
    (lambda args
      (cond
       ((and (eq? (car args) 'get)
             (null? (cddr args)))
        (let ((key (cadr args)))
          (if (dict 'exists key)
              (dict 'get key)
              '())))
       ((and (eq? (car args) 'add)
             (null? (cdddr args)))
        (let ((key (cadr args)) (value (caddr args)))
          (dict 'put key (snoc
                          (if (dict 'exists key)
                              (dict 'get key)
                              '())
                          value))))
       (#t (apply dict args))))))

;; Map each distinct element of input list to an object
;; generated by generator, and then apply this mapping to
;; the whole list.
(define (generator-dict-map lyst generator)
  (let ((dict (make-autoadd-dict generator)))
    (map (lambda (x) (dict 'get x)) lyst)))

(define (fun-dict-map lyst f)
  (let ((dict (make-autoadd-fun-dict f)))
    (map (lambda (x) (dict 'get x)) lyst)))

(define (unique lyst)
  (cond
   ((null? lyst) lyst)
   ((member (car lyst) (cdr lyst)) (unique (cdr lyst)))
   (#t (cons (car lyst) (unique (cdr lyst))))))

(define (has-duplicates? lyst)
  (not (eq? (length lyst) (length (unique lyst)))))

(define (any? lyst)
  (if (null? lyst)
      #f
      (or (car lyst)
          (any? (cdr lyst)))))

(define (all? lyst)
  (cond
   ((null? lyst) #t)
   ((eq? #t (car lyst)) (all? (cdr lyst)))
   ((eq? #f (car lyst)) #f)
   (#t (err 'all? lyst))))

(define (none lyst)
  (not (any? lyst)))

(define (same lyst)
  (if (or (null? lyst) (null? (cdr lyst)))
      #t
      (if (equal? (car lyst) (cadr lyst))
          (same (cdr lyst))
          #f)))

(define-macro (assert exp . stuff)
  `(if ,exp
       '()
       (err "Assertion failure" ',exp ,@stuff)))

(define (id x) x)

(define (err . args)
  (display "Error!\n")
  (shew (map show-shorten args))
(car '()))
;  (exit))

(define show-shorten-length 50)
(define (show-shorten-list lyst) (show-shorten-list1 lyst 0))
(define (show-shorten-list1 lyst acc)
  (cond
   ((not (pair? lyst)) lyst)
   ((>= acc show-shorten-length) '(...))
   (#t (cons (car lyst) (show-shorten-list1 (cdr lyst) (+ acc 1))))))

(define (show-shorten o)
  (if (pair? o)
      (map-improper show-shorten-list (show-shorten-list o))
      o))

(define (map-improper f s)
  (if (pair? s)
      (cons (f (car s))
            (map-improper f (cdr s)))
      (f s)))

(define (tmap f s)
  (if (pair? s)
      (cons (tmap f (car s))
            (tmap f (cdr s)))
      (f s)))

(define (sr . es)
  (apply shew es)
  (last es))

(define (last l)
  (if (pair? l)
      (if (null? (cdr l))
          (car l)
          (last (cdr l)))
      (err)))

(define (make-string-string n s)
  (if (<= n 0)
      ""
      (concat s (make-string-string (1- n) s))))

(define (string-one-line s)
  (string-map
   (lambda (c) (if (eq? c #\newline) #\space c))
   s))

(define (string-collapse-spaces s)
  (list->string
   (foldr
    (lambda (a rest)
      (if (and (eq? a #\space)
               (pair? rest)
               (eq? (car rest) #\space))
          rest
          (cons a rest)))
    '()
    (string->list s))))

(define (find-first pred lyst)
  (cond
   ((null? lyst) (err pred))
   ((pred (car lyst)) (car lyst))
   (#t (find-first pred (cdr lyst)))))

(define (cton? o)
  (and (pair? o) (ctor? (car o))))

(define (ctor? o)
  (if (symbol? o)
      (let ((c (string-ref (symbol->string o) 0)))
        (and (char>=? c #\A)
             (char<=? c #\Z)))
      #f))

(define (non-ctor-symbol? o)
  (and (symbol? o) (not (ctor? o))))

(define (member-deep a lyst)
  (cond
   ((pair? lyst)
    (or (member-deep a (car lyst))
        (member-deep a (cdr lyst))))
   ((eq? a lyst) #t)
   (#t #f)))

;; lens is a -> (extracted . re-inserter)
(define (compose-lens-and-fun lens f)
  (lambda (x)
    (let* ((blah (lens x))
           (extracted (car blah))
           (re-inserter (cdr blah)))
      (re-inserter (f extracted)))))

(define (apply-through-lens lens f o)
  ((compose-lens-and-fun lens f) o))

(define (compose-lenses a b)
  (lambda (x)
    (let* ((blah-b (b x))
           (extracted-b (car blah-b))
           (re-inserter-b (cdr blah-b))
           (blah-a (a extracted-b))
           (extracted-a (car blah-a))
           (re-inserter-a (cdr blah-a)))
      (cons extracted-a (lambda (x) (re-inserter-b (re-inserter-a x)))))))

(define (car-lens x)
  (cons (car x) (lambda (a) (cons a (cdr x)))))

(define (cdr-lens x)
  (cons (cdr x) (lambda (d) (cons (car x) d))))

(define cadr-lens (compose-lenses car-lens cdr-lens))

(define (lensmap lens f l)
  (map (lambda (e) (apply-through-lens lens f e)) l))

(define (fnot f) (lambda (x) (not (f x))))
(define (for a b) (lambda (x) (or (a x) (b x))))
(define (feq? a) (lambda (b) (eq? a b)))

(define (map-append f . lysts)
  (apply append (apply map (cons f lysts))))

(define (map-improper-append f . lysts)
  (apply append (apply map-improper (cons f lysts))))

(define (grep pred lyst)
  (map-append
   (lambda (o)
     (if (pred o)
         (list o)
            '()))
   lyst))

(define (divide-by-pred p lyst)
  (cons (grep p lyst) (grep (fnot p) lyst)))

(define (group-byy f lyst)
  (if (null? lyst)
      '()
      (let* ((group-of-car (f (car lyst)))
             (divided (divide-by-pred
                       (lambda (x) (equal? group-of-car (f x)))
                       lyst))
             (in-group (car divided))
             (not-in-group (cdr divided)))
        (cons (cons group-of-car in-group)
              (group-byy f not-in-group)))))

;; Elements may match more than one predicate; they are grouped with
;; the first one they match.  Every element must match at least one
;; predicate.
(define (group-by-preds preds lyst)
  (if (null? preds)
      (if (not (null? lyst))
          (err "Some elements match no preds: " lyst)
          '())
      (cons (grep (car preds) lyst)
            (group-by-preds (cdr preds) (grep (fnot (car preds)) lyst)))))

;; ;; maybe stuff.

;; f should return either (value) or #f; find-first-maybe will return
;; the first one it finds that's not null, in the same format: (value)
;; or #f.
(define (find-first-maybe f lyst)
  (if (null? lyst)
      fail
      (let ((r (f (car lyst))))
        (if (fail? r)
            (find-first-maybe f (cdr lyst))
            r))))

;; Retrieve x for each (just x) in list, and throw away failures.
;; Good advice for the youth of today!
(define (maybe-successes lyst)
  (cond
   ((null? lyst) '())
   ((pair? lyst)
    (if (fail? (car lyst))
        (maybe-successes (cdr lyst))
        (cons (just-value (car lyst))
              (maybe-successes (cdr lyst)))))
   (#t (err 'maybe-successes lyst))))

;; ;; If any sub-call returns #f, return #f.  Otherwise, extract the
;; ;; value from each singlet and return the list of them.  If you get an
;; ;; #f, don't bother doing the rest.
;; (define (maybe-map f lyst)
;;   (if (null? lyst)
;;       '()
;;       (let ((v (f (car lyst))))
;;         (if (eq? #f v)
;;             #f
;;             (let ((rest (maybe-map f (cdr lyst))))
;;               (if (eq? #f rest)
;;                   #f
;;                   (cons v rest)))))))

;; (define (maybe-map-append f lyst)
;;   (let ((r (maybe-map f lyst)))
;;     (if (eq? #f r)
;;         #f
;;         (apply append r))))

(define (invert-listy-matrix lists)
  (let ((ns (map null? lists)))
    (if (any? ns)
        (if (not (all? ns))
            (err 'invert-listy-matrix 'uneven lists)
            '())
        (cons (map car lists) (invert-listy-matrix (map cdr lists))))))

(define (zip f . lysts)
  ;(shew 'um-zip lysts)
  (if (any? (map null? lysts))
      (if (not (all? (map null? lysts)))
          (err 'uneven-zip-params lysts (map null? lysts))
          '())
      (cons (apply f (map car lysts))
            (apply zip (cons f (map cdr lysts))))))

;; maybe stuff, for real

(define (just a) (list 'just a))
(define fail 'fail)

(define (fail? a) (eq? a fail))
(define (just? a) (and (pair? a) (eq? (car a) 'just) (null? (cddr a))))
(define (just-value o)
  (assert (just? o) o)
  (cadr o))

(define (maybe-combine combiner args)
  (if (any? (map fail? args))
      fail
      (begin
        ;(shew 'combine combiner args (map just-value args) (apply combiner (map just-value args)))
        (just (apply combiner (map just-value args))))))

(define (maybe-append . things)
  (maybe-combine append things))

(define (maybe-list . things)
  (maybe-combine list things))

(define (maybe-compose . fs)
  (assert (not (null? fs)))
  (if (null? (cdr fs))
      (lambda args
        (let ((r (apply (car fs) args)))
          (assert (or (just? r) (fail? r)) r)
          r))
      (let ((rest (apply maybe-compose (cdr fs))))
        (lambda args
          (let ((r (apply (car fs) args)))
            (assert (or (just? r) (fail? r)) r)
            (if (fail? r)
                fail
                (rest (just-value r))))))))

(define (maybe-try . fs)
  (if (null? fs)
      fail
      (let ((rest (apply maybe-try (cdr fs))))
        (lambda args
          (let ((r (apply (car fs) args)))
            (assert (or (just? r) (fail? r)) r)
            (if (fail? r)
                (apply rest args)
                (just-value r)))))))

(define (maybe-apply f args)
  (if (fail? args)
      fail
      (just (apply f (just-value args)))))

(define (maybe-map f as)
  (maybe-apply reverse (maybe-list (maybe-map-1 f as '()))))

(define (maybe-map-1 f as accum)
  (if (null? as)
      (just accum)
      (let ((v (f (car as))))
        (if (fail? v)
            fail
            (maybe-map-1 f (cdr as) (cons (just-value v) accum))))))

;; (define (cons-onto-each a dses)
;;   (if (null? dses)
;;       '()
;;       (cons (cons a (car dses))
;;             (cons-onto-each a (cdr dses)))))

(define (maybe-zip f . args)
  (if (not (same (map length args)))
      fail
      (maybe-map (applyer f) (invert-listy-matrix args))))

(define (mabify f)
  (lambda args
    (if (any? (map (lambda (p) (eq? 'fail p)) args))
        'fail
        (apply f args))))

(define (map-until-not-fail f lyst)
  (if (null? lyst)
      'fail
      (let ((v (f (car lyst))))
        (if (eq? v 'fail)
            (map-until-not-fail f (cdr lyst))
            v))))

;; (define (apply-until f pred o)
;;   (let ((r (f o)))
;;     (if (pred r)
;;         r
;;         (apply-until f pred r))))

(define (++ . stuff)
  (apply concat (map (lambda (o) (->string o)) stuff)))

(define (->int o)
  (cond
   ((number? o) (inexact->exact (floor o)))
   ((string? o) (->int (string->number o)))
   (#f (err))))

(define (->string o)
  (cond
   ((symbol? o) (symbol->string o))
   ((string? o) o)
   ((number? o) (number->string o))
   ((char? o) (make-string 1 o))
   (#t (sdisplay o))))

(define (member? a as)
  (not (not (member a as))))

(define (atom? o)
  (or
   (null? o)
   (symbol? o)
   (number? o)
   (eq? o #t)
   (eq? o #f)
   (string? o)))

(define (is-quote? o)
  (and (pair? o)
       (eq? (car o) 'quote)
       (pair? (cdr o))
       (null? (cddr o))))

(define (quoted-symbol? e)
  (and (is-quote? e) (symbol? (cadr e))))

(define (quote-quoted o)
  (cadr o))

(define (literal? o)
  (or ;(is-quote? o)
      (string? o)
      (number? o)
      (null? o)
      (eq? #t o)
      (eq? #f o)))

(define (rdc riap) (reverse (cdr (reverse riap))))
(define (snoc d a) (reverse (cons a (reverse d))))

(define (rac lyst)
  (cond
   ((null? lyst) (err))
   ((null? (cdr lyst)) (car lyst))
   (#t (rac (cdr lyst)))))

(define (ends-with string suffix)
  (let ((suflen (string-length suffix))
        (slen (string-length string)))
    (and (>= slen suflen)
         (string= suffix (substring string (- (string-length string)
                                              (string-length suffix)))))))

(define (starts-with string prefix)
  (let ((prelen (string-length prefix))
        (slen (string-length string)))
    (and (>= slen prelen)
         (string= prefix (substring string 0 prelen)))))

(define (->symbol o)
  (cond
   ((string? o) (string->symbol o))
   ((symbol? o) o)
   (#t (err))))

(define (foldr f e lyst)
  (if (null? lyst)
      e
      (f (car lyst) (foldr f e (cdr lyst)))))

(define (tagged-symbol-generator-generator)
  (let ((serial 0))
    (lambda (tag)
      (let ((s serial))
        (set! serial (1+ serial))
        (->symbol (concat (->string tag) (number->string s)))))))

(define (symbol-generator-generator)
  (let ((tsg (tagged-symbol-generator-generator)))
    (lambda () (tsg "a"))))

(define (lambda? e)
  (and (proper-list? e)
       (= 3 (length e))
       (eq? '/. (car e))))

(define (scheme-lambda? e)
  (and (proper-list? e)
       (= 3 (length e))
       (eq? 'lambda (car e))))

(define (classic-lambda? e)
  (and (lambda? e)
       (symbol? (cadr e))))

(define (closure? e)
  (and (proper-list? e)
       (= 3 (length e))
       (eq? 'closure (car e))
       (lambda? (cadr e))))

(define (app? e)
  (and (pair? e)
       (not (or (is-quote? e)
                (var? e)))))

(define (1-arg-app? e)
  (and (app? e) (= 2 (length e))))

(define (compose . funs)
  (cond
   ((null? funs) id)
   ((null? (cdr funs)) (car funs))
   (#t (let ((first (car funs))
             (rest (apply compose (cdr funs))))
         (lambda (x) (first (rest x)))))))

(define (conditional? e)
  (and (pair? e)
       (eq? 'if (car e))
       (let ((l (length e)))
         (or (eq? l 3) (eq? l 4)))))

(define (boolean? x)
  (or (eq? x 'True)
      (eq? x 'False)))

(define (pair?-exp? p)
  (and (pair? p)
       (eq? 'pair? (car p))
       (eq? 2 (length p))))

(define (if-pair?-exp? p)
  (and (eq? 3 (length p))
       (eq? 'if-pair? (car p))))

(define (guard? e)
  (and (pair? e)
       (eq? '? (car e))
       (proper-list? (cdr e))))

(define (fun-without-guard-syntax? e)
  (and (pair? e)
       (eq? 'fun (car e))
       (not (guard? (caddr e)))
       (>= (length e) 3)))

(define (fun-with-guard-syntax? e)
  (and (pair? e)
       (eq? 'fun (car e))
       (guard? (caddr e))
       (>= (length e) 4)))

(define (fun-src-syntax? e)
;(shew 'fun-src-syntax? e (fun-with-guard-syntax? e) (guard? (caddr e)) (length e)(or (fun-with-guard-syntax? e)
;       (fun-without-guard-syntax? e)))
   (or (fun-with-guard-syntax? e)
       (fun-without-guard-syntax? e)))

;(define (fun-with-guard? e)
(define (fun? e)
  (and (pair? e)
       (eq? 'fun (car e))
;       (guard? (caddr e))
       (= 4 (length e))))

;; (define (fun? e) (or (fun-without-guard? e)
;;                      (fun-with-guard? e)))

;; (define (guard-exp e)
;;   (assert (guard? e))
;;   (cadr e))

(define (var? e)
  (and (pair? e)
       (eq? 'unquote (car e))
       (pair? (cdr e))
       (symbol? (cadr e))
       (null? (cddr e))))

(define (var-name e)
  (assert (var? e))
  (cadr e))

(define (make-var name)
  (list 'unquote name))

(define (global-var? e)
  (and (pair? e)
       (eq? 'var (car e))
       (= 3 (length e))))

(define (global-var->binding e)
  (assert (global-var? e))
  (cons (cadr e) (caddr e)))

(define (multi-lambda? e)
  (and (proper-list? e)
       (eq? '/./. (car e))
       (all? (map proper-list? (cdr e)))))

(define (tree-traverse t atom-f pair-f)
  (cond
   ((pair? t)
    (let ((np (pair-f t)))
      (cons (tree-traverse (car np) atom-f pair-f)
            (tree-traverse (cdr np) atom-f pair-f))))
   ((atom? t) (atom-f t))
   (#t (err 'tree-traverse t))))

(define (atom-traverse f t)
  (tree-traverse t f id))

(define (treewalk-pairs f t)
  (if (pair? t)
      (let ((nt (f 'pair t)))
        (assert (pair? nt))
        (cons (treewalk f (car nt)) (treewalk-pairs f (cdr nt))))
      t))

(define (treewalk f t)
  (cond
   ((pair? t)
    (let ((nt (f 'list t)))
      (assert (pair? nt))
      (treewalk-pairs f nt)))
   ((atom? t) (f 'atom t))
   (#t (err treewalk f t))))

(define (list-treewalker f)
  (lambda (how t)
    (if (eq? 'list how)
        (f t)
        t)))

(define (se-treewalker f)
  (lambda (how t)
    (f how t)
    t))

(define (term? e)
  (and (proper-list? e)
       (= 2 (length e))
       (eq? 'term (car e))))

(define (begin? e)
  (and (proper-list? e)
       (not (null? e))
       (eq? 'begin (car e))))

(define (is-this-labeled-doublet? s e)
  (and (list? e)
       (= 2 (length e))
       (equal? s (car e))))

(define (primitive? e)
  (is-this-labeled-doublet? 'Primitive e))

(define (primitive2? e)
  (is-this-labeled-doublet? ''Primitive e))

(define (extract-primitive-maybe e)
  (if (primitive2? e)
      (cadr e)
      e))

(define (is-this-primitive? s e)
  (and (is-this-labeled-doublet? s e)
       (primitive? (cadr e))))

(define (is-some-primitive? e)
  (and (list? e)
       (= 2 (length e))
       (primitive? (cadr e))))

(define (is-cons? c)
  (and (list? c)
       (= 3 (length c))
       (equal? 'Cons (car c))))

(define (is-quoted-cons? c)
  (and (list? c)
       (= 3 (length c))
       (equal? ''Cons (car c))))

(define (is-consy-list? c)
  (or (and (is-cons? c) (is-consy-list? (caddr c)))
      (eq? c '())))

(define (conditional? e)
  (and (list? e)
       (= 4 (length e))
       (equal? 'if (car e))))

(define (member-improper? a lyst)
  (or
   (and (pair? lyst)
        (or (eq? a (car lyst))
            (eq? a (cdr lyst))
            (member-improper? a (cdr lyst))))))

(define (read-all-lines port)
  (let ((line (read-line port)))
    (if (eof-object? line)
        '()
        (cons (++ line "\n") (read-all-lines port)))))

(define (typeof o)
  (cond
   ((cton? o) 'cton)
   ((ctor? o) 'ctor)
   ((string? o) 'string)
   ((number? o) 'number)
   ((symbol? o) 'symbol)
   (#t (err 'typeof o))))

(define (lookup k env)
  (let ((v (assoc k env)))
    (if (eq? #f v)
        (err 'lookup k env)
        (cdr v))))
(define (lookup-exists? e env)
  (not (eq? #f (assoc e env))))

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

;; This doesn't really ever change.
(define (consify l)
  (cond
   ((pair? l) `(Cons ,(consify (car l)) ,(consify (cdr l))))
   ((null? l) 'Nil)
   (#t l)))

(define (consify-top-layer l)
  (cond
   ((pair? l) `(Cons ,(car l) ,(consify-top-layer (cdr l))))
   ((null? l) 'Nil)
   (#t l)))

(define (smart== a b)
  (or (and (number? a) (= a b))
      (and (string? a) (string= a b))
      (equal? a b)))

(define (til-same f arg)
  (let ((result (f arg)))
    (if (smart== arg result)
        result
        (til-same f result))))

(define (map-when f lyst pred)
  (if (null? lyst)
      '()
      (let ((a (car lyst)))
        (cons (if (pred a) (f a) a)
              (map-when f (cdr lyst) pred)))))

(define (stump e n)
  (cond
   ((= n 0) '...)
   ((pair? e) (cons (stump (car e) (- n 1))
                    (stump (cdr e) (- n 1))))
   (#t e)))

(define (tree-size a)
  (if (pair? a)
      (+ (tree-size (car a)) (tree-size (cdr a)))
      1))

(define (quoted-symbol? o)
  (and (pair? o) (eq? (car o) 'quote) (symbol? (cadr o)) (null? (cddr o))))

(define (join-things-list glue things)
  (cond
   ((null? things) things)
   ((null? (cdr things)) things)
   (#t (cons (car things) (cons glue (join-things-list glue (cdr things)))))))

(define (join-things glue things)
  (apply ++ (join-things-list glue things)))

(define (cmd . stuff)
  ;(display "+ ") (display (join-things " " stuff)) (display "\n")
  (apply system stuff))

(define (check . stuff)
  (let ((bools (rdc stuff))
        (value (rac stuff)))
    (if (all? bools)
        value
        (err 'check value))))
