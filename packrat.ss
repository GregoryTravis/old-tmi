;(require errortrace)
(load "Lib.ss")
(load "mtch.ss")

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
          (_ ('just x))
            (mtch (parse x tokens top-level?)
              (t rest)
                (list (list symbol t) rest)
              #f #f)))))

(define (top-parse tokens)
  (mtch (parse 'S tokens #t)
    (t '()) t
    _ #f))

(tracefun parse)
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
    (subject (just np))
    (np (alt noun np0))
    (np0 (alt np1 np2))
    (np1 (seq article noun))
    (np2 (seq article np3))
    (np3 (seq adjective noun))
    (predicate (seq verb direct-object))
    (direct-object (just np))))

(define _grammar
  '((S (seq subject predicate))
    (subject noun-phrase)
    (noun-phrase (alt noun (seq article noun) (seq article adjective noun)))
    (predicate (seq verb direct-object))
    (direct-object noun-phrase)))

(define gb-symgen (symbol-generator-generator))

(shew (top-parse '(article noun verb article adjective noun)))
