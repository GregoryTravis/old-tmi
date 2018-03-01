;(shew (regexp-match #px"^(\\s+)(.*)$" " { "))
;(shew (regexp-match #px"^([\\{])(.*)$" "{ "))
;(shew (regexp-match #px"[\\s]" " "))

(define pat-decls '(
  (whitespace #px"(^\\s+)")
  (let_keyword #px"(^let)")
  (in_keyword #px"(^in)(?![a-zA-Z0-9])")
  (case_keyword #px"(^case)")
  (of_keyword #px"(^of)")
  (rdbl_arrow #px"(^=>)")
  (where_keyword #px"(^where)")
  (if_keyword #px"(^if)")
  (then_keyword #px"(^then)")
  (else_keyword #px"(^else)")
  (integer #px"(^[0-9_]+)")
  (constructor #px"(^[A-Z][a-zA-Z0-9_]+)")
  (identifier #px"(^[a-zA-Z0-9_][a-zA-Z0-9_=<>+/\\-_!@$%^&*?]*)")
  (comma #px"(^,)")
  (comment #px"(^;;)")
  (semicolon #px"(^;)")
  (equals #px"(^=)\\s")
  (lambda #px"(^/\\.)\\s")
  (operator #px"(^[=<>+/\\-_!@$%^&*?]+)")
  (lparen #px"(^[\\(])")
  (rparen #px"(^[\\)])")
  (lsb #px"(^[\\[])")
  (rsb #px"(^[\\]])")
  (lcb #px"(^[\\{])")
  (rcb #px"(^[\\}])")
))
#|
(define until-newline-regex #px"(^[^\n]*\n)(.*$)")
;(shew (regexp-match until-newline-regex "asdf\nzxcv\n"))
;(shew (regexp-match until-newline-regex "asdf\nzxcv\nasdfasfd"))
;(shew (regexp-match until-newline-regex "asdf\nzxcv"))
;(shew (regexp-match until-newline-regex "\nasdf\nzxcv"))
|#

(define (try-tokenize pat-decl s start)
  (mtch pat-decl
    (token-type token-regex)
      (mtch (regexp-match token-regex s start)
        (all token-src) `((,token-type ,token-src))
        #f #f)))
;(tracefun try-tokenize)

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

#|
(define (remove-until-newline s)
  (mtch (regexp-match until-newline-regex s)
    (all comment rest) rest))
|#

(define (next-newline s start)
  (mtch (regexp-match #px"^([^\n]*\n)" s start)
    #f (string-length s)
    (all rest-of-line) (+ start (string-length rest-of-line))))

(define (tokenize s start rowcol)
  (if (eq? start (string-length s))
    '()
    (mtch (find-first-maybe (lambda (pat-decl) (try-tokenize pat-decl s start)) pat-decls)
      (('comment token-src))
        (tokenize s (next-newline s start) (mtch rowcol (r c) `(,(+ r 1) 0)))
      ((token-type token-src))
        `((,token-type ,token-src ,rowcol) . ,(tokenize s (+ start (string-length token-src)) (next-rowcol rowcol token-src))))))

(define (remove-whitespace toks)
  (grep (lambda (token) (not (eq? (car token) 'whitespace))) toks))
  ;(mtch toks
    ;(('whitespace x) . rest) (remove-whitespace rest)
    ;(a . d) `(,a . ,(remove-whitespace d))))

(define (tokenize-top s)
  (remove-whitespace (tokenize s 0 '(0 0))))

;(shew (tokenize-top (read-file-as-string "input.tmi")))
(hook-with timing-hook tokenize-top)
