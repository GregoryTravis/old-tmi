(load "Lib.ss")
(load "mtch.ss")

#|
token_patterns = [
  { 'type': 'whitespace', 're': '(^\s+)(.*$)' },
  { 'type': 'string', 're': '("(\"|[^\"])*")(.*$)' },
  { 'type': 'let_keyword', 're': '(^let)(.*$)' },
  { 'type': 'in_keyword', 're': '(^in)(.*$)' },
  { 'type': 'case_keyword', 're': '(^case)(.*$)' },
  { 'type': 'of_keyword', 're': '(^of)(.*$)' },
  { 'type': 'rdbl_arrow', 're': '(^=>)(.*$)' },
  { 'type': 'where_keyword', 're': '(^where)(.*$)' },
  { 'type': 'identifier', 're': "(^[a-zA-Z0-9_]+)(.*$)" },
  { 'type': 'semicolon', 're': '(^;)(.*$)' },
  { 'type': 'equals', 're': "(^=)(.*$)" },
  { 'type': 'operator', 're': '(^[=<>+\-_!@$%^&*?]+)(.*$)' },
  { 'type': 'lparen', 're': '(^[\(])(.*$)' },
  { 'type': 'rparen', 're': '(^[\)])(.*$)' },
  { 'type': 'lcb', 're': '(^[\{])(.*$)' },
  { 'type': 'rcb', 're': '(^[\}])(.*$)' },
]
|#

;(shew (regexp-match #px"^(\\s+)(.*)$" " { "))
;(shew (regexp-match #px"^([\\{])(.*)$" "{ "))
;(shew (regexp-match #px"[\\s]" " "))

(define pat-decls '(
  (whitespace #px"(^\\s+)(.*$)")
  (let_keyword #px"(^let)(.*$)")
  (in_keyword #px"(^in)(.*$)")
  (case_keyword #px"(^case)(.*$)")
  (of_keyword #px"(^of)(.*$)")
  (rdbl_arrow #px"(^=>)(.*$)")
  (where_keyword #px"(^where)(.*$)")
  (if_keyword #px"(^if)(.*$)")
  (then_keyword #px"(^then)(.*$)")
  (else_keyword #px"(^else)(.*$)")
  (identifier #px"(^[a-zA-Z0-9_]+)(.*$)")
  (semicolon #px"(^;)(.*$)")
  (equals #px"(^=)(.*$)")
  (operator #px"(^[=<>+/\\-_!@$%^&*?]+)(.*$)")
  (lparen #px"(^[\\(])(.*$)")
  (rparen #px"(^[\\)])(.*$)")
  (lcb #px"(^[\\{])(.*$)")
  (rcb #px"(^[\\}])(.*$)")
))

(define (try-tokenize pat-decl s)
  (mtch pat-decl
    (token-type token-regex)
      (mtch (regexp-match token-regex s)
        (all token-src rest) `((,token-type ,token-src ,rest))
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

(define (tokenize s rowcol)
  (if (eq? (string-length s) 0)
    '()
    (mtch (find-first-maybe (lambda (pat-decl) (try-tokenize pat-decl s)) pat-decls)
      ((token-type token-src rest))
        `((,token-type ,token-src ,rowcol) . ,(tokenize rest (next-rowcol rowcol token-src))))))

(define (remove-whitespace toks)
  (grep (lambda (token) (not (eq? (car token) 'whitespace))) toks))
  ;(mtch toks
    ;(('whitespace x) . rest) (remove-whitespace rest)
    ;(a . d) `(,a . ,(remove-whitespace d))))

(define (tokenize-top s)
  (remove-whitespace (tokenize s '(0 0))))

;(shew (tokenize-top (read-file-as-string "input.tmi")))
