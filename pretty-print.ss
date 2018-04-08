(define (unconsify e)
  (mtch e
    ('Cons a d) (cons a (unconsify d))
    'Nil '()))

(define (tmi-pp-render o)
  (cond
    ((hash? o)
      `("{" ,(join-things-list ", "
        (map (lambda (k) `(,(tmi-pp-render (string->symbol k)) ": "
                           ,(tmi-pp-render (hash-ref o k)))) (hash-keys o))) "}"))
    ((symbol? o) (symbol->string o))
    ((string? o) o)
    (#t (mtch o
          ('Cons a d)
            `("[" ,(join-things-list ", " (map tmi-pp-render (unconsify o))) "]")
          o (lsshew o)))))

(define (tmi-pretty-print o) 
  ;(shew 'ooo0 (tmi-pp-render o))
  ;(shew 'ooo1 (flatten (tmi-pp-render o)))
  ;(shew 'ooo2 (apply string-append (flatten (tmi-pp-render o))))
  (apply string-append (flatten (tmi-pp-render o))))
