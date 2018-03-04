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

(define (safe-run thunk)
  (with-handlers
    ([(lambda (exn) #t) (lambda (exn) (shew 'show exn))])
    (thunk)))

(define (execute e)
  (safe-run (lambda () (eval e))))

(define dloaded-files (make-hash))
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

(define (daemon)
  (let ((ctrl-port (open-input-file "/tmp/racket-daemon-ctrl")))
    (letrec ((loop
      (lambda ()
        (let ((files (hash-keys dloaded-files)))
          ;(shew 'checking files)
          (map dload files)
          (sleep .1)
          (check-ctrl ctrl-port)
          (loop)))))
      (loop))))
