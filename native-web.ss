(require net/cgi)
(load "lib.ss")

(define (web-create-server)
  (tcp-listen 5000 4 #t))

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
(asseq '("/" ()) (web-parse-request '("GET / HTTP/1.1\r" "Host: localhost:5000\r")))

(define (write-thing o port)
  (if (string? o)
    (write-string o port)
    (write-bytes o port)))

(define (web-respond bout o)
  (map (lambda (o) (write-thing o bout))
    (list "HTTP/1.1 200 OK\r\n" "Content-Type: text/html\r\n\r\n" o))
  (close-output-port bout))
