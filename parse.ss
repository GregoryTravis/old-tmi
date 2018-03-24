(define (general-recurser-s before-fun after-fun e)
  (let ((e (before-fun e))
        (rec (lambda (e) (general-recurser-s before-fun after-fun e))))
    (after-fun
      (mtch e
        ('let decls exp)
          `(let ,(map rec decls) ,(rec exp))
        ('pdo assignments exp)
          `(pdo ,(map rec assignments) ,(rec exp))
        ('do_assignments assignment)
          `(do_assignments ,(rec assignment))
        ('do_assignment pat body)
          `(do_assignment ,(rec pat) ,(rec body))
        ('where decls body)
          `(where ,(map rec decls) ,(rec body))
        ('definition lexp equalsk rexp)
          `(definition ,(rec lexp) ,equalsk ,(rec rexp))
        ('decls def)
          `(decls ,(rec def))
        ('lambda-exp args exp)
          `(lambda-exp ,(rec args) ,(rec exp))
        ('non-where-exp e)
          (rec e)
        ('binop a op b)
          `(binop ,(rec a) ,(rec op) ,(rec b))
        ('app es)
          `(app ,(map rec es))
        ('case e ccs)
          `(case ,(rec e) ,(map rec ccs))
        ('case_clauses cc)
          `(case_clauses ,(rec cc))
        ('case_clause pat exp)
          `(case_clause ,(rec pat) ,(rec exp))
        ('if b t e)
          `(if ,(rec b) ,(rec t) ,(rec e))
        ;; Tokens.  TODO put 'token at the front of these
        ('integer s . _) e
        ('string s . _) e
        ('constructor s . _) e
        ('identifier s . _) e
        ('operator s . _) e
        ))))
;(tracefun general-recurser-s)

(define (case-clause-unbinarize-2 e)
  (mtch e
    ('case_clauses ('case_clause . rest)) `((case_clause . ,rest))
    ('case_clauses ('case_clause . rest) semicolon as) (cons `(case_clause . ,rest) (case-clause-unbinarize-2 as)) ))

(define (decls-unbinarize-2 e)
  (mtch e
    ('decls d) (list d)
    ('decls d semicolon ('decls . rest)) (cons d (decls-unbinarize-2 `(decls . ,rest)))))

(define (flatten-base-exp-seq e)
  (mtch e
    ('base-exp-seq ('base-exp e) d)
      (cons e (flatten-base-exp-seq d))
    ('base-exp-seq ('base-exp e))
      (list e)))

(define (flatten-do-assignments e)
  (mtch e
    ('do_assignments assignment ('semicolon . _) assignments)
      (cons assignment (flatten-do-assignments assignments))
    ('do_assignments assignment)
      (list assignment)))

(define (un-cses-1 cses)
  (mtch cses
    ('comma-separated-exp-sequence ('exp a))
      `(,a)
    ('comma-separated-exp-sequence ('exp e) ('comma . _) rest)
      (cons e (un-cses-1 rest))))
(define (un-cses e)
  (foldr
    (lambda (a d)
      `(app ((constructor "Cons") ,a ,d)))
    `(app ((constructor "Nil")))
    (un-cses-1 e)))
;(tracefun un-cses)

(define (p2s e)
  (mtch e
    ('base-exp-seq ('base-exp be))
      (p2s be)
    ('base-exp-seq ('base-exp be) . d)
      `(app ,(map p2s (flatten-base-exp-seq e)))
    ('decls . _)
      (map p2s (decls-unbinarize-2 e))
    ('case_clauses . _)
      (case-clause-unbinarize-2 e)
    ('plet ('let_keyword . x) ('lcb . x) decls ('rcb . x) ('in_keyword . x) exp)
      `(let ,(p2s decls) ,(p2s exp))
    ;('pwhere exp ('where_keyword . x) ('lcb . x) decls ('rcb . x))
      ;`(where ,(p2s decls) ,(p2s exp))
    ('where-exp ('non-where-exp exp) ('pwhere-suffices ('pwhere-suffix ('where_keyword . x) ('lcb . x) decls ('rcb . x))))
      `(where ,(p2s decls) ,(p2s exp))
    ('pif ('if_keyword . x) b ('then_keyword . x) t ('else_keyword . x) e)
      `(if ,(p2s b) ,(p2s t) ,(p2s e))
    ('pdo ('do_keyword . _) ('lcb . _) assignments ('semicolon . _) exp ('rcb . _))
      `(pdo ,(map p2s (flatten-do-assignments assignments)) ,(p2s exp))
    ('do_assignment pat ('larrow . _) body)
      `(do_assignment ,(p2s pat) ,(p2s body))
    ('exp x)
      (p2s x)
    ('non-where-exp x)
      (p2s x)
    ('case case_keyword exp of_keyword lcb ('case_clauses . cc) rcb)
      `(case ,(p2s exp) ,(map p2s (p2s `(case_clauses . ,cc))))
    ('case_clause pat _ exp)
      `(case_clause ,(p2s pat) ,(p2s exp))
    ('listexp ('lsb . _) cses ('rsb . _))
      (p2s (un-cses cses))
    ('definition pat e body)
      `(definition ,(p2s pat) ,e ,(p2s body))
    ('app os)
      `(app ,(map p2s os))
    ('identifier . _)
      e
    ('integer . _)
      e
    ('string . _)
      e
    ('operator . _)
      e
    ('parenexp l e r)
      (p2s e)
    ('constructor . _)
      e
    ('lambda-exp sym pat body)
      `(lambda-exp ,(p2s pat) ,(p2s body))
      ))
;(tracefun p2s)

; Look for app trees and pass them to unapp-1
(define (unapp e)
  (mtch e
    ('app . x) `(app ,(unapp-1 e))
    (a . d) `(,(unapp a) . ,(unapp d))
    x x))
(define (unapp-1 e)
  (mtch e
    ('app x ('app y . z)) `(,(unapp x) . ,(unapp-1 `(app ,y . ,z)))
    ('app x y) `(,(unapp x) ,(unapp y))))
;(tracefun unapp)(tracefun unapp-1)

(define lambda-symgen (tagged-symbol-generator-generator 'lambda))

(define (blah-pat-args e)
  (mtch e
    ('app pat) pat
    pat `(,pat)))
(define (lambda->let-1 e)
  (mtch e
    ;('lambda-exp _ h _) (err h)
    ('lambda-exp pat body)
      (let ((name (lambda-symgen)))
        `(let ((definition (app ,(cons `(identifier ,(symbol->string name)) (blah-pat-args pat)))
                (equals "=")
                ,body))
              ;(equals "=")
              (app ((identifier ,(symbol->string name))))))
     x x))
(define (lambda->let e) (general-recurser-s lambda->let-1 id e))

(define (rewrite-do-1 e)
  (mtch e
    ('pdo '() exp)
      exp
    ('pdo (('do_assignment pat body) . assignments) exp)
      (begin
        (mtch pat ('identifier . _) #t) ;; Assertion
        `(app ((constructor "Seq") ,body (lambda-exp ,pat ,(rewrite-do-1 `(pdo ,assignments ,exp))))))
    x x))
(define (rewrite-do e) (general-recurser-s id rewrite-do-1 e))

(define (postprocess e)
  (lambda->let (rewrite-do (precedence (p2s e)))))
;(tracefun lambda->let rewrite-do separate-app-op precedence postprocess)

; Categorical!
(define (maybe-list ms)
  (if (any? (map (lambda (m) (eq? m #f)) ms))
    #f
    (list (map car ms))))
