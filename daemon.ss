(require errortrace)
(load "lib.ss")

(define (read-maybe port)
  (let* ((bstr (make-bytes 100))
         (n (read-bytes-avail!* bstr port)))
    (if (> n 0)
      (bytes->string/utf-8 bstr #f 0 (- n 1))
      #f)))

(define (check-ctrl port)
  (let ((a (read-maybe port)))
    (mtch a
      #f '()
      e (let ((e (read (open-input-string e))))
          ;(shew 'eval e)
          (display "\n")
          (execute e)))))

(define (show-exn exn)
  (shew exn)
  ;(shew (continuation-mark-set->context (vector-ref (struct->vector exn) 2)))
  (print-error-trace (current-output-port) exn))

(define (safe-run thunk)
  (with-handlers
    ([(lambda (exn) #t) (lambda (exn) (show-exn exn))])
    (thunk)))

(define (execute e)
  (safe-run (lambda () (eval e))))

(define dloaded-files (make-hash))

(define (reset-dloaded-files)
  (set! dloaded-files (make-hash)))

(define (dload filename)
  (let ((now (current-seconds))
        (mtime (file-or-directory-modify-seconds filename)))
    (if (or (not (hash-has-key? dloaded-files filename))
            (> mtime (hash-ref dloaded-files filename)))
      (begin
        (hash-set! dloaded-files filename now)
        (shew `(loading ,filename))
        (safe-run (lambda () (load filename))))
      ;(shew 'already filename)
      '())))

(define (reload-if-changed)
  (let ((files (hash-keys dloaded-files)))
    ;(shew 'checking files)
    (map dload files)))

(define (daemon-reload-all)
  (let ((files (hash-keys dloaded-files)))
    (reset-dloaded-files)
    (map dload files)))

; => #t | (error)
(define (sandbox-run thunk out)
  (with-handlers ([(lambda (exn) #t) (lambda (exn) `(,exn))])
    (parameterize ((current-output-port out))
      (let ((result (thunk)))
        (if (not (void? result))
          (display result)
          '())
        #t))))

(define (start-background-reloader)
  (letrec ((loop (lambda ()
      (reload-if-changed)
      (sleep .1)
      (loop))))
    (thread loop)))

(define (silence-errors exn-type thunk)
    (with-handlers ([exn-type (lambda (exn) '())]
                    [exn? (lambda (exn) (shew 'ERR exn))])
      (thunk)))

(define (daemon)
  (start-background-reloader)
  (letrec ((loop (lambda ()
    (let ((ss (tcp-listen 5001 4 #t)))
      (shew 'ready)
      (let-values (((bin bout) (tcp-accept ss)))
        ;(shew 'Connected)
        (reload-if-changed)
        (let ((input (read-objects-port bin)))
          ;(shew 'read input)
          (let ((all `(begin . ,input)))
            (mtch
              (sandbox-run
                (lambda ()
                  (execute all)
                  (close-output-port bout)
                  (tcp-close ss))
                bout)
              #t
                '()
              (error)
                (if (not (exn:fail:network? error))
                  (shew 'ERROR error)
                  '()))
            (silence-errors exn:fail:network? (lambda () (close-output-port bout)))
            (silence-errors exn:fail:network? (lambda () (tcp-close ss)))
            #|
            (with-handlers ([(lambda (exn) #t) (lambda (exn) (shew 'ERR exn))])
              (close-output-port bout))
            (with-handlers ([(lambda (exn) #t) (lambda (exn) (shew 'ERR exn))])
              (tcp-close ss))
              |#
            (loop))))))))
    (loop)))
