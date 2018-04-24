(require json)

;; Must be True or False; not using mtch since it's used so much (probably).
(define (tmi-if b)
  (assert (or (eq? b 'True) (eq? b 'False)) 'not-boolean b)
  (eq? b 'True))

(define (traceo f . args)
  (shew 'traceo f args)
  (let ((result (apply f args)))
    (shew 'traceo-ret result)
    result))

(define (ffi-apply f args)
  (scheme->tmi (apply f (map tmi->scheme args))))

(define (driver-main io)
  (mtch io
    ;; TODO instead of renaming 'args' why not use hygienic macros?
    ('Command ('Cons proc args2))
        (ffi-apply proc args2)
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
      x x)))

;; Change symbol keys to strings
(define (string-hash-keys h)
  (if (hash? h)
    (make-immutable-hash 
      (map (lambda (k) (cons (symbol->string k) (string-hash-keys (hash-ref h k)))) (hash-keys h)))
    (mtch h
      (a . d) (map string-hash-keys o)
      x x)))

;; Change string keys to symbols
(define (symbol-hash-keys h)
  (if (hash? h)
    (make-immutable-hash 
      (map (lambda (k) (cons (string->symbol k) (symbol-hash-keys (hash-ref h k)))) (hash-keys h)))
    (mtch h
      (a . d) (map symbol-hash-keys o)
      x x)))

(define (scheme->tmi o)
  (cond
    ((hash? o)
      (make-immutable-hash
        (map (lambda (k) (cons (->string k) (scheme->tmi (hash-ref o k)))) (hash-keys o))))
    ((void? o)
      'Nil)
    (#t
      (mtch o
        (aaa . ddd)
          `(Cons ,(scheme->tmi aaa) ,(scheme->tmi ddd))
        '()
          'Nil
        #t
          'True
        #f
          'False
        x x))))
(define (tmi->scheme o)
  (if (hash? o)
    (make-immutable-hash
      (map (lambda (k) (cons (->symbol k) (tmi->scheme (hash-ref o k)))) (hash-keys o)))
    (mtch o
      ('Cons aaa ddd)
        `(,(tmi->scheme aaa) . ,(tmi->scheme ddd))
      'Nil
        '()
      'True
        #t
      'False
        #f
      x x)))
;(tracefun tmi->scheme scheme->tmi)

(define (native-read-data filename)
  (call-with-input-file* filename
    (lambda (in) (read-json in))))

(define (native-write-data filename data)
  (call-with-output-file* filename #:exists 'replace
    (lambda (out) (write-json (tmi->scheme data) out))))

(define (native-read-data-string s)
    (scheme->tmi (read-json (open-input-string s))))

(define (native-write-data-string data)
  (let ((out (open-output-string)))
    (write-json (tmi->scheme data) out)
    (get-output-string out)))

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
  `(Seq (Command (Cons ,f ,args))
        ,(lambda (result) `(Return ,result))))

(define (native->tmi f)
  (lambda args (scheme->tmi (apply f (map tmi->scheme args)))))

(define tmi-sort (native->tmi sort))
(define tmi-apply (native->tmi apply))
