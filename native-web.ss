(require net/cgi)
(require net/uri-codec)
(load "lib.ss")

(define all-opened-listeners '())
(define (add-opened-listener listener)
  (set! all-opened-listeners (cons listener all-opened-listeners)))
(define (close-all-opened-listeners)
  (map
    (lambda (listener)
      (shew 'closing listener)
      (with-handlers ((exn:fail:network? (lambda (exn) (shew 'closy exn))))
        (tcp-close listener)))
    all-opened-listeners)
  (set! all-opened-listeners '()))

(define (web-create-server)
  (shew 'opened all-opened-listeners)
  (close-all-opened-listeners)
  (let ((listener (tcp-listen 5000 4 #t)))
    (add-opened-listener listener)
    listener))

; => (output-stream (url cgi-params))
(define (web-get-next-request ss)
  (let-values (((bin bout) (tcp-accept ss)))
    (file-stream-buffer-mode bin 'none)
    (file-stream-buffer-mode bout 'none)
    (let* ((lines (web-read-request bin)))
      (list bout (web-parse-request lines)))))

(define (web-read-request bin)
  (let ((line (read-line bin)))
    (if (or (eof-object? line) (equal? line "\r"))
      '()
      (cons line (web-read-request bin)))))

(define (parse-cgi cgi-string)
  (putenv "REQUEST_METHOD" "GET")
  (putenv "QUERY_STRING" cgi-string)
  (coll-make-hash (get-bindings)))

(define (web-parse-url url)
  (mtch (string-split url "?")
    (url) `(,url ())
    (url cgi-string) `(,url ,(parse-cgi cgi-string))))
(asseq '("/hey" ()) (web-parse-url "/hey"))
(asseq '("/hey" #hash(("a" . "1")("b" . "2"))) (web-parse-url "/hey?a=1&b=2"))

(define (web-parse-request lines)
  (web-parse-url (nth 1 (string-split (nth 0 lines) " "))))
(asseq '("/" ()) (web-parse-request '("GET / HTTP/1.1\n" "Host: localhost:5000\n")))

(define (write-thing o port)
  (if (string? o)
    (write-string o port)
    (write-bytes o port)))

(define (web-respond bout o)
  (map (lambda (o) (write-thing o bout))
    (list "HTTP/1.1 200 OK\n" "Content-Type: text/html\n\n" o))
  (close-output-port bout))

#|
(define (hash-to-assoc hash)
  (map (lambda (k) (cons k (hash-ref hash k))) (hash-keys hash)))

(define (web-encode-params rec)
  (alist->form-urlencoded (hash-to-assoc rec)))
(define web-encode-params (native->tmi web-encode-params))
|#

(define tmi-uri-encode (native->tmi uri-encode))
