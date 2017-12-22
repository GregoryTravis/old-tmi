(load "Lib.ss")
(load "mtch.ss")

(define gram #hash(
  (top . ((decls)))
  (app . ((exp exp) (app exp)))
  (let . ((let_keyword lcb decls rcb in_keyword exp)))
  ;(where . ((exp where_keyword lcb decls rcb)))
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
    (let ((val (hash-ref memo (list nt s e) '())))
      (if (not (null? val))
          val
          (if (eq? val #f)
            #f  
            ; memo placeholder for recursion prevention
            (begin
              (hash-set! memo (list nt s e) #f)
              (mtch
                (if (and (eq? (+ s 1) e) (eq? (nth s os) nt))
                  ; TODO maybe check before the memo check?
                  `(,nt)
                  (find-first-maybe
                    (lambda (production) (try-prodution gram nt os s e memo production))
                    (hash-ref gram nt (lambda () '()))))
                (value)
                  (begin
                    (hash-set! memo (list nt s e) `(,value))
                    `(,value))
                #f
                  (begin
                    (hash-remove! memo (list nt s e))
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

(define (unbinarize e)
  (mtch e
    (a (b . c))
      (if (starts-with (symbol->string b) "parsebin-")
          `(,(unbinarize a) . ,(unbinarize c))
          `(,a (,(unbinarize b) . ,(unbinarize c))))
    (a . d)
      `(,(unbinarize a) . ,(unbinarize d))
    aa aa))
(tracefun unbinarize)

(define (top-parse gram nt os)
  (shew 'parse os)
  (let ((gram (binarize gram)))
    (shew gram)
    (mtch (parse gram nt os 0 (length os) (make-hash))
      (value) `(,(unbinarize value))
      #f #f)))

#|
(tracefun-with
  (lambda (app runner)
    (mtch app ('parse gram nt os s e memo) (plain-ol-tracer (list 'parse nt os s e) runner)))
  parse)
|#

(shew (top-parse gram 'rcb '(rcb)))
(shew (top-parse gram 'exp '(identifier)))
(shew (top-parse gram 'app '(identifier identifier)))
(shew (top-parse gram 'exp '(identifier identifier)))
(shew (top-parse gram 'exp '(identifier identifier identifier)))
(shew (top-parse gram 'exp '(lparen identifier rparen)))
(shew (top-parse gram 'exp '(lparen identifier rparen)))
(shew (top-parse gram 'exp '(lparen lparen identifier rparen rparen)))
(shew (top-parse gram 'decls '(identifier equals identifier identifier semicolon identifier equals identifier)))

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
