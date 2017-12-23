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

(shew (regexp-match #px"^(\\s+)(.*)$" " { "))
(shew (regexp-match #px"^([\\{])(.*)$" "{ "))
(shew (regexp-match #px"[\\s]" " "))

(define pat-decls '(
  (whitespace #px"(^\\s+)(.*$)")
  (let_keyword #px"(^let)(.*$)")
  (in_keyword #px"(^in)(.*$)")
  (case_keyword #px"(^case)(.*$)")
  (of_keyword #px"(^of)(.*$)")
  (rdbl_arrow #px"(^=>)(.*$)")
  (where_keyword #px"(^where)(.*$)")
  (identifier #px"(^[a-zA-Z0-9_]+)(.*$)")
  (semicolon #px"(^;)(.*$)")
  (equals #px"(^=)(.*$)")
  (operator #px"(^[=<>+\\-_!@$%^&*?]+)(.*$)")
  (lparen #px"(^[\\(])(.*$)")
  (rparen #px"(^[\\)])(.*$)")
  (lcb #px"(^[\\{])(.*$)")
  (rcb #px"(^[\\}])(.*$)")
))

(shew pat-decls)

(define (try-tokenize pat-decl s)
  (mtch pat-decl
    (token-type token-regex)
      (mtch (regexp-match token-regex s)
        (all token-src rest) `((,token-type ,token-src ,rest))
        #f #f)))
;(tracefun try-tokenize)

(define (tokenize s)
  (if (eq? (string-length s) 0)
    '()
    (mtch (find-first-maybe (lambda (pat-decl) (try-tokenize pat-decl s)) pat-decls)
      ((token-type token-src rest)) `((,token-type ,token-src) . ,(tokenize rest)))))

(define (remove-whitespace toks)
  (grep (lambda (token) (not (eq? (car token) 'whitespace))) toks))
  ;(mtch toks
    ;(('whitespace x) . rest) (remove-whitespace rest)
    ;(a . d) `(,a . ,(remove-whitespace d))))

(define (tokenize-top s)
  (remove-whitespace (tokenize s)))

(shew (tokenize-top (read-file-as-string "input.tmi")))
