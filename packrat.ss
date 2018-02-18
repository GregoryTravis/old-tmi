;(require errortrace)
(load "Lib.ss")
(load "mtch.ss")
(load "parse.ss")
(load "tokenize.ss")

;; S = epsilon | a S b
;; =>
;; S = Y | epsilon
;; Y = X b
;; X = a S

; (S (Y (X a (S epsilon)) b))

(define grammar
  '((S (alt Y epsilon))
    (Y (seq X b))
    (X (seq a S))))

;; Returns #f or (tree rest)
(define (parse symbol tokens top-level?)
  (cond
    ((eq? symbol 'epsilon)
      (if top-level?
        #f
        (list 'epsilon tokens)))
    ;;; Hack: this means anything not in the grammar is a token
    ((not (assoc symbol grammar))
      (if (and (not (null? tokens)) (eq? symbol (car tokens))) (list (car tokens) (cdr tokens)) #f))
    (#t (mtch (assoc symbol grammar)
          (_ ('seq x y))
            (mtch (parse x tokens top-level?)
              (tx rest)
                (mtch (parse y rest #f)
                  (ty rest)
                    (list (list symbol tx ty) rest)
                  #f #f)
              #f #f)
          (_ ('alt x y))
            (mtch (parse x tokens top-level?)
              (t rest)
                (list (list symbol t) rest)
              #f
                (mtch (parse y tokens top-level?)
                  (t rest)
                    (list (list symbol t) rest)
                  #f
                    #f))
          (_ x)
            (begin
              (assert (atom? x))
              (mtch (parse x tokens top-level?)
                (t rest)
                  (list (list symbol t) rest)
                #f #f))))))
(define (top-parse tokens)
  (mtch (parse 'S tokens #t)
    (t '()) t
    _ #f))

;(tracefun parse)
;(S (Y (X a (S epsilon)) b))
;(shew (parse 'S '(a b) #t))
;(shew (top-parse '(a b)))
;(shew (top-parse '(a a b b)))

;; S = subject predicate
;;; HACK clearly this is not what we want
;; subject = noun-phrase | noun-phrase
;; noun-phrase = noun | article noun | article adjective noun
;; predicate = verb direct-object
;; direct-object = noun-phrase

(define grammar
  '((S (seq subject predicate))
    (subject np)
    (np (alt noun np0))
    (np0 (alt np1 np2))
    (np1 (seq article noun))
    (np2 (seq article np3))
    (np3 (seq adjective noun))
    (predicate (seq verb direct-object))
    (direct-object np)))

(define _grammar
  '((S (seq subject predicate))
    (subject noun-phrase)
    (noun-phrase (alt noun (seq article noun-phrase) (seq article adjective noun-phrase)))
    (predicate (seq verb direct-object))
    (direct-object noun-phrase)))

(define gb-symgen (tagged-symbol-generator-generator "bg-"))

;; e -> (e, rules)
(define (flatten-expression e)
  (if (atom? e)
    (list e '())
    (mtch e
      (node-type . es)
        (let* ((nes (map flatten-expression es))
               (new-es (map car nes))
               (rules (apply append (map cadr nes)))
               (ns (gb-symgen)))
          `(,ns ((,ns (,node-type . ,new-es)) . ,rules))))))
(define (binarize-expression e)
  (mtch e
    (node-type a b c . rest)
      (let* ((ns (gb-symgen)))
        (mtch (binarize-expression `(,node-type ,b ,c . ,rest))
          (se rules)
            `((,node-type ,a ,ns) ((,ns ,se) . ,rules))))
    e
      (list e '())))
;(tracefun flatten-expression)
(define (binarize-grammar g)
  (map-append (lambda (rule)
    (mtch rule
      (sym e)
        (mtch (binarize-expression e)
          (ne rules)
            `((,sym ,ne) . ,rules))))
    (map-append (lambda (rule)
      (mtch rule
        (sym e)
          (mtch (flatten-expression e)
            (ne rules)
              `((,sym ,ne) . ,rules))))
      g)))

(define (parsed-unbinarize e)
  (if (list? e)
    (mtch (map parsed-unbinarize e)
      (s (bg . es))
        (if (starts-with (symbol->string bg) "bg-")
          `(,s . ,es)
          `(,s (,bg . ,es)))
      (s x (bg . es))
        (if (starts-with (symbol->string bg) "bg-")
          `(,s ,x . ,es)
          `(,s ,x (,bg . ,es)))
      x x)
    e))

(shew _grammar)
(define grammar (binarize-grammar _grammar))
(shew grammar)
(define parsed (top-parse '(article noun verb article adjective article adjective noun)))
(shew parsed)
(shew (parsed-unbinarize parsed))

(define grammar '(
  (S decls)
  (plet (seq let_keyword lcb decls rcb in_keyword exp))
  (pwhere (seq exp where_keyword lcb decls rcb))
  (definition (seq exp equals exp))
  (decls (alt (seq definition semicolon decls) definition))
  (parenexp (seq lparen exp rparen))
  (listexp (seq lsb comma-separated-exp-sequence rsb))
  (comma-separated-exp-sequence (alt exp (seq comma-separated-exp-sequence comma exp)))
  (lambda-exp (seq lambda parenexp exp))
  (base-exp (alt constructor identifier integer operator parenexp listexp lambda-exp))
  (base-exp-seq (alt base-exp (seq base-exp-seq base-exp)))
  (exp (alt pif plet pwhere case base-exp-seq))
  (case (seq case_keyword exp of_keyword lcb case_clauses rcb))
  (case_clauses (alt (seq case_clauses semicolon case_clause) case_clause))
  (case_clause (seq exp rdbl_arrow exp))
  (pif (seq if_keyword exp then_keyword exp else_keyword exp))
))
(define grammar (binarize-grammar grammar))
;(shew (top-parse (map car (tokenize-top (read-file-as-string "input.tmi")))))
