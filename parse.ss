(load "lib.ss")
(load "mtch.ss")
(load "preprocess.ss")
(load "tokenize.ss")

(define gram #hash(
  (top . ((decls)))
  (app . ((exp exp) (app exp)))
  (let . ((let_keyword lcb decls rcb in_keyword exp)))
  (where . ((exp where_keyword lcb decls rcb)))
  (definition . ((exp equals exp)))
  (decls . ((definition semicolon decls) (definition)))
  (parenexp . ((lparen exp rparen)))
  (exp . ((parenexp) (let) (where) (case) (app) (identifier) (operator)))
  (case . ((case_keyword exp of_keyword lcb case_clauses rcb)))
  (case_clauses . ((case_clauses semicolon case_clause) (case_clause)))
  (case_clause . ((exp rdbl_arrow exp)))
))

(define binarize-production-symgen (tagged-symbol-generator-generator "parsebin-"))
(define (binarize-production production)
  (mtch production
    (nt (a b c . rest))
      (let ((new-nt (binarize-production-symgen)))
        `((,nt (,a ,new-nt)) . ,(binarize-production `(,new-nt (,b ,c . ,rest)))))
    (nt rhses) `((,nt ,rhses))))

(define (regroup-binarized-grammar gram)
  (make-hash (map (lambda (group) (cons (caar group) (map cadr group))) (group-by car gram))))

(define (binarize grammar)
  (regroup-binarized-grammar
    (flatten1
      (map binarize-production
        (flatten1
          (map (lambda (ntdef)
                 (mtch ntdef
                      (nt . rhses) (map (lambda (rhs) `(,nt ,rhs)) rhses)))
               (hash->list grammar)))))))

; Returns (value) or #f
; TODO not using memo yet
(define (parse gram nt os s e memo)
  (if (eq? s e)
    ; TODO implement epsilon?
    #f
    (let ((memo-val (hash-ref memo (list nt s e) '())))
      (if (not (null? memo-val))
          ; This includes the recursion-prevention #f value
          memo-val
          ;; These two lines probably not needed, redudant to previous if
          (if (eq? memo-val #f)
            #f  
            ; memo placeholder for recursion prevention
            (begin
              (hash-set! memo (list nt s e) #f)
              (mtch
                (if (and (eq? (+ s 1) e) (eq? (car (nth s os)) nt))
                  ; TODO maybe check before the memo check?
                  `(,(nth s os))
                  (find-first-maybe
                    (lambda (production) (try-prodution gram nt os s e memo production))
                    (hash-ref gram nt (lambda () '()))))
                (value)
                  (begin
                    (hash-set! memo (list nt s e) `(,value))
                    `(,value))
                #f
                  (begin
                    ;(hash-remove! memo (list nt s e))
                    ;; Probably redundant?
                    (hash-set! memo (list nt s e) #f)
                    #f))))))))

(define (try-prodution gram nt os s e memo production)
  (mtch production
    (a b)
      (mtch (find-first-maybe (lambda (m) (try-two gram a b os s m e memo)) (gen-integer-sequence s e))
        ((resulta resultb)) `((,nt ,resulta ,resultb))
        #f #f)
    (a)
      (mtch (parse gram a os s e memo)
        (result) `((,nt ,result))
        #f #f)))

#|
(tracefun-with
  (lambda (app runner)
    (mtch app (try-prodution gram nt os s e memo production)
      (plain-ol-tracer (list 'try-prodution nt os s e production) runner)))
  try-prodution)
|#

(define (try-two gram a b os s m e memo)
  (mtch (parse gram a os s m memo)
    (resulta)
      (mtch (parse gram b os m e memo)
        (resultb) `((,resulta ,resultb))
        #f #f)
    #f #f))

#|
(tracefun-with
  (lambda (app runner)
    (mtch app (try-two gram a b os s m e memo)
      (plain-ol-tracer (list 'try-two a b os s m e) runner)))
  try-two)
|#

(define (grammar-unbinarize e)
  (mtch e
    (a (b . c))
      (if (and (symbol? b) (starts-with (symbol->string b) "parsebin-"))
          `(,(grammar-unbinarize a) . ,(grammar-unbinarize c))
          `(,(grammar-unbinarize a) (,(grammar-unbinarize b) . ,(grammar-unbinarize c))))
    (a . d)
      `(,(grammar-unbinarize a) . ,(grammar-unbinarize d))
    aa aa))
;(tracefun unbinarize)

(define (case-clause-unbinarize-1 e)
  (mtch e
    ('case_clauses ('case_clause . rest)) (list rest)
    ('case_clauses as semicolon ('case_clause . rest)) (append (case-clause-unbinarize-1 as) (list rest))
    x x))
(define (case-clause-unbinarize e) (apply-and-descend case-clause-unbinarize-1 e))

  ;(definition . ((exp equals exp)))
  ;(decls . ((definition semicolon decls) (definition)))
(define (decls-unbinarize-1 e)
  (mtch e
    ('decls ('definition . d)) (list d)
    ('decls ('definition . d) semicolon ('decls . rest)) (cons d (decls-unbinarize-1 `(decls . ,rest)))
    x x))
(define (decls-unbinarize e) (apply-and-descend decls-unbinarize-1 e))

(define (apply-and-descend f e)
  (let ((e (apply-until-fixpoint f e)))
    (if (pair? e)
      (cons (apply-and-descend f (car e)) (apply-and-descend f (cdr e)))
      e)))

(define (p2s-1 e)
  (mtch e
    ('let ('let_keyword . x) ('lcb . x) decls ('rcb . x) ('in_keyword . x) exp)
      `(let ,decls ,exp)
    ('where exp ('where_keyword . x) ('lcb . x) decls ('rcb . x))
      `(where ,decls ,exp)
    ('exp x . y) x
    ('case case_keyword exp of_keyword lcb case_clauses rcb) `(case ,exp ,case_clauses)
    x x))
(define (p2s e) (apply-and-descend p2s-1 e))
;(tracefun p2s-1)

(define (postprocess e)
  (p2s (decls-unbinarize (case-clause-unbinarize (grammar-unbinarize e)))))

(define (top-parse gram nt os)
  ;(shew 'parse os)
  (let ((gram (binarize gram)))
    ;(shew gram)
    (mtch (parse gram nt os 0 (length os) (make-hash))
      (value) `(,(postprocess value))
      #f #f)))

#|
(tracefun-with
  (lambda (app runner)
    (mtch app ('parse gram nt os s e memo) (plain-ol-tracer (list 'parse nt s e) runner)))
  parse)
|#

#|
(shew (top-parse gram 'rcb '(rcb)))
(shew (top-parse gram 'exp '(identifier)))
(shew (top-parse gram 'app '(identifier identifier)))
(shew (top-parse gram 'exp '(identifier identifier)))
(shew (top-parse gram 'exp '(identifier identifier identifier)))
(shew (top-parse gram 'exp '(lparen identifier rparen)))
(shew (top-parse gram 'exp '(lparen identifier rparen)))
(shew (top-parse gram 'exp '(lparen lparen identifier rparen rparen)))
(shew (top-parse gram 'decls '(identifier equals identifier identifier semicolon identifier equals identifier)))
|#

(define (wrap-file tokens)
  (mtch (last tokens)
    (a as (row col))
      `((let_keyword "let" (-1 -1))
        ,@tokens
        (in_keyword "in" (,(+ row 1) -1))
        (identifier "main" (,(+ row 1) 2)))))

(let
  ((pre (preprocess-top (wrap-file (tokenize-top (read-file-as-string "input.tmi"))))))
  (display (tokens->src pre))
  (shew (top-parse gram 'let pre)))

#|
(define gram
  '(sentence ((subject, predicate)))
  'sentence': [ [ 'subject', 'predicate' ] ],
  'subject': [ [ 'noun' ], [ 'adjective', 'noun' ], [ 'noun_phrase' ] ],
  'noun_phrase': [ [ 'noun', 'that', 'verb', 'noun' ] ],
  'predicate': [ [ 'verb', 'noun' ], [ 'verb', 'adjective', 'noun' ], [ 'verb', 'adjective', 'adjective', 'noun' ] ],
}

|#

#|
- Write binarize and parse_top
- Tokenize
- Preprocess
|#
