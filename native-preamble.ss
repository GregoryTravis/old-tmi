(require json)

;; Must be True or False; not using mtch since it's used so much (probably).
(define (tmi-if b)
  (assert (or (eq? b 'True) (eq? b 'False)) 'not-boolean b)
  (eq? b 'True))

; Stops at the first non-Cons construction.
(define (native-unconsify e)
  (mtch e
    ('Cons a d) (cons (native-unconsify a) (native-unconsify d))
    'Nil '()
    x x))

(define (consify l)
  (mtch l
    (a . d) `(Cons ,a ,(consify d))
    '() 'Nil))

(define (traceo f . args)
  (shew 'traceo f args)
  (let ((result (apply f args)))
    (shew 'traceo-ret result)
    result))

(define (ffi-convert-retval o)
  (cond
    ((eq? o #t) 'True)
    ((eq? o #f) 'False)
    (#t o)))

(define (ffi-apply f args)
  (ffi-convert-retval (apply f (native-unconsify args))))

(define (driver-main io)
  (mtch io
    ;; TODO instead of renaming 'args' why not use hygienic macros?
    ('Command ('Cons proc-name args2))
      (let ((proc (eval (string->symbol proc-name))))
        (ffi-apply proc args2))
    ('Return x)
      x
    ('Seq io kio)
      (driver-main (kio (driver-main io)))))
;(tracefun driver-main)

(define t-int? integer?)
(define t-string? string?)

(define (tmi-if-ify op)
  (lambda (a b) (if (op a b) 'True 'False)))

;; Only for native scheme operators used without overloading; eventually most of these will disappear
(define op* *)
(define op/ /)
(define op+ +)
(define op- -)
(define op< (tmi-if-ify <))
(define op> (tmi-if-ify >))
(define op== (tmi-if-ify equal?))
(define op!= (tmi-if-ify (lambda (a b) (not (equal? a b)))))
; This isn't used yet except in precedence
; (define op$$ $$)

(define native-+ +)

(define (coll-make-hash pairs)
  (let ((hash (make-hash)))
    (map
      (lambda (p)
        (mtch p (k . v)
          (begin
            (assert (symbol? k))
            (assert (not (hash-has-key? hash k)))
            (hash-set! hash k v))))
      pairs)
    hash))

;; Convert any hash to an equal? hash.  Recursive.
(define (->hash-equal o)
  (if (hash? o)
    (make-hash (map (lambda (e) (mtch e (k . v) `(,k . ,(->hash-equal v)))) (hash->list o)))
    (mtch o
      (a . d) (map ->hash-equal o)
      x x)))

;; Only handles a list of records
(define (json->tmi json)
  (consify (map ->hash-equal json)))

(define (native-read-data filename)
  (call-with-input-file* filename
    (lambda (in) (json->tmi (read-json in)))))

(define (native-write-data filename data)
  (call-with-output-file* filename #:exists 'replace
    (lambda (out) (write-json data out))))
