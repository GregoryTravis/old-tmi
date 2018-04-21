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

;; Hash set and return
(define (hsar hash key value)
  (hash-set! hash key value)
  value)

;; Returns #f or (tree rest)
(define (parse symbol tokens s e top-level? memo)
  (let ((memo-val (hash-ref memo (list symbol s) '())))
    (if (not (eq? memo-val '()))
      memo-val
      (begin
        ;; Innfinite recursion prevention
        (hsar memo (list symbol s) #f)
        (hsar memo (list symbol s)
          (cond
            ((eq? symbol 'epsilon)
              (if top-level?
                #f
                (list 'epsilon s)))
            ;;; Hack: this means anything not in the grammar is a token
            ((not (assoc symbol grammar))
              (if (and (not (eq? s e)) (eq? symbol (car (vector-ref tokens s)))) (list (vector-ref tokens s) (+ s 1)) #f))
            (#t (mtch (assoc symbol grammar)
                  (_ ('seq x y))
                    (mtch (parse x tokens s e top-level? memo)
                      (tx new-s)
                        (mtch (parse y tokens new-s e #f memo)
                          (ty new-new-s)
                            (list (list symbol tx ty) new-new-s)
                          #f #f)
                      #f #f)
                  (_ ('alt x y))
                    (mtch (parse x tokens s e top-level? memo)
                      (t new-s)
                        (list (list symbol t) new-s)
                      #f
                        (mtch (parse y tokens s e top-level? memo)
                          (t new-s)
                            (list (list symbol t) new-s)
                          #f
                            #f))
                  (_ x)
                    (begin
                      (assert (atom? x))
                      (mtch (parse x tokens s e top-level? memo)
                        (t new-s)
                          (list (list symbol t) new-s)
                        #f #f))))))))))
(define (top-parse tokens)
  ;(shew 'TOP tokens)
  (mtch (parse 'S (list->vector tokens) 0 (length tokens) #t (make-hash))
    (t final-s) (if (eq? final-s (length tokens)) t #f)
    _ #f))


#|
(tracefun-with
  (lambda (app runner)
    (mtch app (parse symbol tokens s e top-level? memo)
      (plain-ol-tracer (list 'parse symbol s (if (< s e) (vector-ref tokens s) '???) top-level?) runner)))
  parse)
|#

;(tracefun top-parse)
; (S (Y (X a (S epsilon)) b))
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
        (if (and (symbol? bg) (starts-with (symbol->string bg) "bg-"))
          `(,s . ,es)
          `(,s (,bg . ,es)))
      (s x (bg . es))
        (if (and (symbol? bg) (starts-with (symbol->string bg) "bg-"))
          `(,s ,x . ,es)
          `(,s ,x (,bg . ,es)))
      x x)
    e))

#|
(shew _grammar)
(define grammar (binarize-grammar _grammar))
(shew grammar)
(define parsed (top-parse '(article noun verb article adjective article adjective noun)))
(shew parsed)
(shew (parsed-unbinarize parsed))
|#

;; Can you guess why I am anding this with #t?
(define (really-add-libs) (and (not debug-compile) #t))
(define library-files '("overture.tmi" "rel.tmi" "node.tmi" "web.tmi" "cgi.tmi"))
(define (add-libs s)
  (if (really-add-libs)
    (string-append (apply string-append (map read-file-as-string library-files)) "\n" s)
    s))

(define (wrap-file tokens)
  (mtch (last tokens)
    (a as (row col))
      `((let_keyword "let" (-1 -1))
        ,@tokens
        (in_keyword "in" (,(+ row 1) -1))
        (identifier "main" (,(+ row 1) 2)))))

(define grammar '(
  ;(S decls)
  (S plet)
  (plet (seq let_keyword p-lcb decls p-rcb in_keyword exp))
  ;(pwhere (seq exp where_keyword p-lcb decls p-rcb))
  (pwhere-suffix (seq where_keyword p-lcb decls p-rcb))
  (pdo (alt (seq do_keyword p-lcb exp p-rcb)
            (seq do_keyword p-lcb do_assignments semicolon exp p-rcb)))
  (do_assignments (alt (seq do_assignment semicolon do_assignments) do_assignment))
  (do_assignment (seq exp larrow exp))
  (definition (seq exp equals exp))
  (decls (alt (seq definition semicolon decls) definition))
  (parenexp (seq lparen exp rparen))
  (listexp (alt (seq lsb rsb) (seq lsb comma-separated-exp-sequence rsb)))
  (comma-separated-exp-sequence (alt (seq exp comma comma-separated-exp-sequence) exp))
  (lambda-exp (seq lambda parenexp exp))
  (base-exp (alt constructor identifier integer operator unary-operator parenexp listexp lambda-exp string phash))
  (base-exp-seq (alt (seq base-exp base-exp-seq) base-exp))
  ;(base-exp-seq (alt base-exp (seq base-exp base-exp-seq)))
  (exp (alt where-exp non-where-exp))
  (where-exp (seq non-where-exp pwhere-suffices))
  (pwhere-suffices (alt (seq pwhere-suffix pwhere-suffices) pwhere-suffix))
  (non-where-exp (alt pif plet pdo case base-exp-seq))
  (case (seq case_keyword exp of_keyword p-lcb case_clauses p-rcb))
  (case_clauses (alt (seq case_clause semicolon case_clauses) case_clause))
  (case_clause (seq exp rdbl_arrow exp))
  (pif (seq if_keyword exp then_keyword exp else_keyword exp))
  (phash (alt (seq lcb rcb) (seq lcb phash-entries rcb)))
  (phash-entries (alt (seq phash-entry comma phash-entries) phash-entry))
  (phash-entry (seq identifier colon exp))
))
(define grammar (binarize-grammar grammar))
;(shew grammar)
;(hook-with timing-hook top-parse)
;(hook-with timing-hook preprocess-top)
;(hook-with timing-hook tokenize-top)
;(hook-with timing-hook parsed-unbinarize)

(define (split-into-tlfs tokens)
  (group-by-starts (lambda (token) (mtch token (_ _ (line column)) (eq? column 0))) tokens))

(define (parse-file filename)
  (parse-tokens (tokenize-top (add-libs (read-file-as-string filename))) filename))

(define (parse-tokens-maybe tokens filename)
  (parsed-unbinarize (top-parse (preprocess-top (wrap-file tokens)))))

(define (parse-tokens tokens filename)
  (let ((parsed (parse-tokens-maybe tokens filename)))
    (mtch parsed
      (S parsed) (list (parse-postprocess parsed))
      #f (parse-tlfs-separately tokens filename))))

(define (parse-tlfs-separately tokens filename)
  (let ((tlfs (split-into-tlfs tokens)))
    (if (< (length tlfs) 2)
      (err 'parse-failure tokens)
      (parse-until-failure tlfs filename))))
(define (parse-until-failure tlfs filename)
  (mtch tlfs
    (a . d)
      (mtch (parse-tokens-maybe a filename)
        (S parsed)
          (parse-until-failure d filename)
        #f
          (parse-error a filename))
    '()
      (err 'file-failed-but-tlfs-did-not-weird)))

(define (parse-error tokens filename)
  (let ((line (mtch (car tokens) (_ _ (line _)) line)))
    (display
      (string-append "Parse failure in " filename " on line " (number->string line) ":\n"
        "\n" (string-trim (tokens->src tokens)) "\n\n"))
    (err 'parse-failure)))

;(tracefun tokenize-top)
;(hook-with timing-hook parse-file)
