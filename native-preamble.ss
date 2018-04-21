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

#|
(define (consify l)
  (mtch l
    (a . d) `(Cons ,a ,(consify d))
    '() 'Nil))
|#

(define (traceo f . args)
  (shew 'traceo f args)
  (let ((result (apply f args)))
    (shew 'traceo-ret result)
    result))

(define (ffi-convert-retval o)
  (cond
    ((eq? o #t) 'True)
    ((eq? o #f) 'False)
    ((eq? o (void)) 'Nil)
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
  (lambda args (if (apply op args) 'True 'False)))

;; Only for native scheme operators used without overloading; eventually most of these will disappear
(define native+ +)
(define native- -)
(define native* *)
(define native/ /)
(define native< (tmi-if-ify <))
(define native> (tmi-if-ify >))
(define native<= (tmi-if-ify <=))
(define native>= (tmi-if-ify >=))
(define native== (tmi-if-ify equal?))
(define native!= (tmi-if-ify (lambda (a b) (not (equal? a b)))))
(define native^^ (tmi-if-ify (lambda (a b) (xor (tmi-if a) (tmi-if b)))))
; This isn't used yet except in precedence
; (define native$$ $$)
(define (op$ f a) (f a))
(define op! (tmi-if-ify (lambda (b) (not (tmi-if b)))))

; Short-circuiting operators
(define op&&
  (tmi-if-ify (lambda (at bt) (and (tmi-if (at)) (tmi-if (bt))))))
(define op\|\|
  (tmi-if-ify (lambda (at bt) (or (tmi-if (at)) (tmi-if (bt))))))

(define (coll-make-hash pairs)
  (assert (unique? (map car pairs)))
  (make-immutable-hash
    (map
      (lambda (p) (mtch p (k . v) `(,(symbol->string k) . ,v)))
      pairs)))

;; Convert any hash to an immutable equal? hash.  Recursive.
(define (->hash-equal o)
  (if (hash? o)
    (make-immutable-hash (map (lambda (e) (mtch e (k . v) `(,k . ,(->hash-equal v)))) (hash->list o)))
    (mtch o
      (a . d) (map ->hash-equal o)
      '() 'Nil
      x x)))

;; Change symbol keys to strings
(define (string-hash-keys h)
  (if (hash? h)
    (make-immutable-hash 
      (map (lambda (k) (cons (symbol->string k) (string-hash-keys (hash-ref h k)))) (hash-keys h)))
    (mtch h
      (a . d) (map string-hash-keys h)
      '() 'Nil
      x x)))

;; Change string keys to symbols
(define (symbol-hash-keys h)
  (if (hash? h)
    (make-immutable-hash 
      (map (lambda (k) (cons (string->symbol k) (symbol-hash-keys (hash-ref h k)))) (hash-keys h)))
    (mtch h
      (a . d) (map symbol-hash-keys h)
      x x)))

;; Only handles a list of records
(define (json->tmi json)
  (string-hash-keys (->hash-equal json)))
;; Only handles a list of records
(define (tmi->json o)
  (map symbol-hash-keys o))

(define (scheme->tmi o)
  (if (hash? o)
    (make-immutable-hash
      (map (lambda (k) (cons (->string k) (scheme->tmi (hash-ref o k)))) (hash-keys o)))
    (mtch o
      (a . d)
        `(Cons ,(scheme->tmi a) ,(scheme->tmi d))
      '()
        'Nil
      x x)))
(define (tmi->scheme o)
  (if (hash? o)
    (make-immutable-hash
      (map (lambda (k) (cons k (tmi->scheme (hash-ref o k)))) (hash-keys o)))
    (mtch o
      ('Cons a d)
        `(,(tmi->scheme a) . ,(tmi->scheme d))
      'Nil
        '()
      x x)))
(tracefun tmi->scheme scheme->tmi)

(define (native-read-data filename)
  (call-with-input-file* filename
    (lambda (in) (scheme->tmi (read-json in)))))
(tracefun scheme->tmi json->tmi)

(define (native-write-data filename data)
  (call-with-output-file* filename #:exists 'replace
    (lambda (out) (write-json (tmi->json data) out))))

(define (display-newline o)
  (display o)
  (display "\n"))

(define (rel-equal? rel0 rel1)
  (if
    (let ((rel0 (unconsify rel0))
          (rel1 (unconsify rel1)))
     (and (all? (map (lambda (rec0) (member? rec0 rel1)) rel0))
          (all? (map (lambda (rec1) (member? rec1 rel0)) rel1))))
    'True
    'False))

(define (ffi f . args)
  (let ((name (if (procedure? f) (symbol->string (object-name f)) (symbol->string f))))
    `(Seq (Command (Cons ,name ,(tmi->scheme args)))
          ,(lambda (result) `(Return ,(consify result))))))

(define (native->tmi f)
  (lambda args (consify (apply f (map native-unconsify args)))))

(define tmi-sort (native->tmi sort))
