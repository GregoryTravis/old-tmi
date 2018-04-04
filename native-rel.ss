(define (rec-to-entries h)
  (consify-1 (map (lambda (k) `(Entry ,k ,(hash-ref h k))) (hash-keys h))))
(asseq '(Cons (Entry a 10) (Cons (Entry b 20) Nil)) (rec-to-entries #hash((a . 10) (b . 20))))

(define (entries-to-rec es)
  (let ((assocs
    (map
      (lambda (e)
        (mtch e
          ('Entry k v) `(,k . ,v)))
      (unconsify es))))
    (assert (unique? (map car assocs)))
    (make-immutable-hash assocs)))

(define (hash-equal? h0 h1)
  (let ((k0 (hash-keys h0))
        (k1 (hash-keys h1)))
    (and (equal? k0 k1)
      (let ((v0 (map (lambda (k) (hash-ref h0 k)) k0))
            (v1 (map (lambda (k) (hash-ref h1 k)) k1)))
        (equal? v0 v1)))))
(assert (hash-equal? (entries-to-rec '(Cons (Entry a 10) (Cons (Entry b 20) Nil))) (entries-to-rec '(Cons (Entry a 10) (Cons (Entry b 20) Nil)))))

(define (extend rec k v)
  (hash-set rec k v))
