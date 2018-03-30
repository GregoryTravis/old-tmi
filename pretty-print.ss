(define (tmi-pp-render o)
  (cond
    ((hash? o)
      `("{" ,(join-things-list ", "
        (map (lambda (k) `(,(tmi-pp-render k) ": " ,(tmi-pp-render (hash-ref o k)))) (hash-keys o))) "}"))
    ((symbol? o) (symbol->string o))
    (#t (lsshew o))))

;(tracefun tmi-pp-render)

(define (tmi-pretty-print o) 
  ;(shew 'ooo0 (tmi-pp-render o))
  ;(shew 'ooo1 (flatten (tmi-pp-render o)))
  ;(shew 'ooo2 (apply string-append (flatten (tmi-pp-render o))))
  (apply string-append (flatten (tmi-pp-render o))))
