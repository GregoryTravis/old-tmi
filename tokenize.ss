;(shew (regexp-match #px"^(\\s+)(.*)$" " { "))
;(shew (regexp-match #px"^([\\{])(.*)$" "{ "))
;(shew (regexp-match #px"[\\s]" " "))

(define token-res '(
  (whitespace "\\s+")
  (let_keyword "let")
  (in_keyword "in(?![a-zA-Z0-9])")
  (do_keyword "do(?![a-zA-Z0-9])")
  (larrow "<-")
  (case_keyword "case")
  (of_keyword "of")
  (rdbl_arrow "=>")
  (where_keyword "where")
  (if_keyword "if")
  (then_keyword "then")
  (else_keyword "else")
  (integer "[0-9]+")
  (constructor "[A-Z][a-zA-Z0-9_]+")
  (identifier "[a-zA-Z0-9_][a-zA-Z0-9_=<>+/\\-_!@$%^&|*?]*")
  (comma ",")
  (comment ";;")
  (colon ":")
  (semicolon ";")
  (equals "=(?=\\s)")
  (lambda "/\\.(?=\\s)")
  (unary-operator "!(?!=)") ;; "!(?!=)|~"
  (operator "[=<>+/\\-_!@$%^&|*?]+")
  (lparen "[\\(]")
  (rparen "[\\)]")
  (lsb "[\\[]")
  (rsb "[\\]]")
  (p-lcb "\\{-")
  (p-rcb "-\\}")
  (lcb "\\{")
  (rcb "\\}")
  (string "\"[^\"]*\"")
))

(define combined-pattern
  (pregexp
    (let ((names (map car token-res))
          (res (map cadr token-res)))
      (string-join
        (map (lambda (re) (string-append "^(" re ")")) res)
        "|"))))

(define (which-token res groups)
  (if (not (eq? (car groups) #f))
    (caar res)
    (which-token (cdr res) (cdr groups))))

(define (match-token s start)
  (mtch (regexp-match combined-pattern s start)
    (all . groups) `(,(which-token token-res groups) ,all)
    x (err 'tokenization-failure (substring s start))))

; This should work too, and be faster but less portable?
; (string-split "\nab\nc\n\nde\n\n" "\n" #:trim? #f)

(define (next-rowcol rowcol token)
  (mtch rowcol (row col)
    (if (eq? (string-length token) 0)
        rowcol
        (next-rowcol
         (mtch (car (string->list token))
           #\newline `(,(+ row 1) 0)
           x `(,row ,(+ col 1)))
         (list->string (cdr (string->list token)))))))

(assert (equal? '(4 0) (next-rowcol '(3 2) "\n")))
(assert (equal? '(4 1) (next-rowcol '(3 2) "\n ")))
(assert (equal? '(4 1) (next-rowcol '(3 2) "\na")))
(assert (equal? '(4 0) (next-rowcol '(3 2) "b\n")))
(assert (equal? '(4 1) (next-rowcol '(3 2) "b\n ")))
(assert (equal? '(4 1) (next-rowcol '(3 2) "b\na")))
(assert (equal? '(5 2) (next-rowcol '(3 2) "b\na\ncd")))

(define (next-newline s start)
  (mtch (regexp-match #px"^([^\n]*\n)" s start)
    #f (string-length s)
    (all rest-of-line) (+ start (string-length rest-of-line))))

(define (tokenize s start rowcol)
  (if (eq? start (string-length s))
    '()
    (mtch (match-token s start)
      ('comment token-src)
        (tokenize s (next-newline s start) (mtch rowcol (r c) `(,(+ r 1) 0)))
      (token-type token-src)
        `((,token-type ,token-src ,rowcol) . ,(tokenize s (+ start (string-length token-src)) (next-rowcol rowcol token-src))))))

(define (remove-whitespace toks)
  (grep (lambda (token) (not (eq? (car token) 'whitespace))) toks))
  ;(mtch toks
    ;(('whitespace x) . rest) (remove-whitespace rest)
    ;(a . d) `(,a . ,(remove-whitespace d))))

(define (tokenize-top s)
  (remove-whitespace (tokenize s 0 '(0 0))))

;(tracefun tokenize-top)
;(hook-with timing-hook tokenize-top)
