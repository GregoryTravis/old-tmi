(load "Lib.ss")
(load "mtch.ss")

(assert
  (equal?
    '((bar ee 12) (baz ee 13))
    (list
      (mtch '(foo 12)
        ('foo x) `(bar ee ,x)
        ('bar x) `(baz ee ,x))
      (mtch '(bar 13)
        ('foo x) `(bar ee ,x)
        ('bar x) `(baz ee ,x)))))

(assert (equal? '(3 4 5) (gen-integer-sequence 3 5)))
