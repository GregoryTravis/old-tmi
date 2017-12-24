(load "Lib.ss")
(load "mtch.ss")
(load "tokenize.ss")

; def hya(src):
; Tried to translate python but stopped when I realized racket doesn't have while
(define (_preprocess tokens)
;   #print src
;   #print '=============='
;   tokens = tokenize(src)
;   indent_stack = []
;   output = []
;   inx = 0
  (let* ((indent_stack '())
         (output '())
         (inx 0)
         (push (lambda (x) (set! indent_stack (append indent_stack (list x)))))
         (pop (lambda () (set! indent_stack (reverse (cdr (reverse indent_stack))))))
         (append-output (lambda (x) (set! output (append output (list x))))))
; 
;   indent_stack.append(('let', 0, 0))
    (push '("let" 0 0))
; 
;   while inx < len(tokens):
    (while (< inx (length tokens))

;     current = tokens[inx]
      (let* ((current (nth inx tokens))
             (current_src (nth 1 current))
             (current_row (nth 0 (nth 2 current)))
             (current_col (nth 1 (nth 2 current))))
;     #print 'aaa', current, indent_stack
; 
;     # dedent
;     while True:
       (while #t
;       #print '-', inx, current, indent_stack
;       if len(indent_stack) > 0 and indent_stack[-1][1] > current['column_number'] and indent_stack[-1][2] < current['line_number']:
        (if (and (> (length indent_stack) 0)
                 (> (nth 1 (last indent_stack)) current_col)
                 (< (nth 2 (last indent_stack)) current_row))
;         #print 'dedent', current, indent_stack[-1]
            (cond
;         if indent_stack[-1][0] == 'let':
              ((equal? (nth 0 (last indent_stack)) "let")
;           assert current['src'] == 'in', ('Expected in', current, indent_stack)
;           break
                (begin
                  (assert (equal? current_src "in") 'expected-in)
                  (break)))
;           #output.append(mktok('}'))
;           #indent_stack.pop()
;         elif indent_stack[-1][0] == 'where':
              ((equal? (nth 0 (last indent_stack)) "where")
                (begin
;           output.append(mktok('}'))
                  (append-output '(rcb "}"))
;           indent_stack.pop()
                  (pop)))
;         elif indent_stack[-1][0] == 'case':
             ((equal? (nth 0 (last indent_stack)) "case")
               (begin
;           assert current['src'] == 'of'
                  (assert (equal? current_src "of"))
;           #indent_stack.pop()
;           break
                  (break)))
;         elif indent_stack[-1][0] == 'of':
              ((equal? (nth 0 (last indent_stack)) "of")
                (begin
;           output.append(mktok('}'))
                  (append-output '(rcb "}"))
;           indent_stack.pop()
                  (pop)
;           assert indent_stack[-1][0] == 'case'
                  (assert (equal? (nth 0 (last indent_stack)) "case"))
;           indent_stack.pop()
                  (pop)))
;         else:
              (else
;           assert False, '???'
                (assert #f "???")))
;       else:
;         break
            (break)))
; 
;     # eqdent
;     if len(indent_stack) > 0 and indent_stack[-1][1] == current['column_number'] and indent_stack[-1][2] < current['line_number']:
        (if (and (> (length indent_stack) 0)
                 (eq? (nth 1 (last indent_stack)) current_col)
                 (< (nth 2 (last indent_stack)) current_row))
;       if indent_stack[-1][0] == 'let' or indent_stack[-1][0] == 'where' or indent_stack[-1][0] == 'of':
          (if (or (eq? (nth 0 (last indent_stack)) "let")
                  (eq? (nth 0 (last indent_stack)) "where")
                  (eq? (nth 0 (last indent_stack)) "of"))
;         output.append(mktok(';'))
            (append-output '(semicolon ";"))
;       else:
;         assert False, ('Bad dedent?', indent_stack)
            (assert #f 'bad-dedent?))
          '())
; 
;     #print current, indent_stack
    (cond
;     if False:
;       assert False

;     elif current['src'] == '(':
;       indent_stack.append(('(', tokens[inx+1]['column_number'], tokens[inx+1]['line_number']))
;       output.append(current)
      ((eq? current_src "(")
        (begin  
          (let ((next-token (nth (+ inx 1) tokens)))
            (push '("(" (nth 1 (nth 2 next-token)) (nth 0 (nth 2 next-token)))))
          (append-output current)))

;     elif current['src'] == ')':
;       while indent_stack[-1][0] in ['where', 'of']:
;         if indent_stack[-1][0] == 'where':
;           output.append(mktok('}'))
;           indent_stack.pop()
;         elif indent_stack[-1][0] == 'of':
;           output.append(mktok('}'))
;           indent_stack.pop()
;           assert indent_stack[-1][0] == 'case'
;           indent_stack.pop()
;         else:
;           assert False
;       assert indent_stack[-1][0] == '(', indent_stack
;       indent_stack.pop()
;       output.append(current)
      ((eq? current_src ")")
        (begin
          (while (or (equal? (nth 0 (last indent_stack)) "where")
                     (equal? (nth 0 (last indent_stack)) "of"))
            (cond
              ((equal? (nth 0 (last indent_stack)) "where")
                (begin
                  (append-output '(rcb "}"))
                  (pop)))
              ((equal? (nth 0 (last indent_stack)) "of")
                (begin
                  (append-output '(rcb "}"))
                  (pop)
                  (assert (equal? (nth 0 (last indent_stack)) "case"))
                  (pop)))
              (else (assert #f))))
          (assert (equal? (nth 0 (last indent_stack)) "("))
          (pop)
          (append-output current)))

;     elif current['src'] == 'in':
;       assert len(indent_stack) > 0, ('initial in', current, indent_stack)
;       assert indent_stack[-1][0] == 'let', ('mismatched in', current, indent_stack)
;       indent_stack.pop()
;       output.append(mktok('}'))
;       output.append(current)
      ((eq? current_src "in")
        (begin
          (assert (> (length indent_stack) 0) 'initial-in)
          (assert (equal? (nth 0 (last indent_stack)) "let") 'mismatched-in)
          (pop)
          (append-output '(rcb "}"))
          (append-output current)))

;     elif current['src'] == 'let':
;       assert inx + 1 < len(tokens), 'Dangling let'
;       indent_stack.append(('let', tokens[inx+1]['column_number'], tokens[inx+1]['line_number']))
;       output.append(current)
;       output.append(mktok('{'))
      ((eq? current_src "let")
        (begin
          (assert (< (+ inx 1) (length tokens)) 'dangling-let)
          (let ((next-token (nth (+ inx 1) tokens)))
            (push '("let" (nth 1 (nth 2 next-token)) (nth 0 (nth 2 next-token)))))
          (append-output current)
          (append-output '(lcb "{"))))

;     elif current['src'] == 'where':
;       assert inx + 1 < len(tokens), 'Dangling where'
;       indent_stack.append(('where', tokens[inx+1]['column_number'], tokens[inx+1]['line_number']))
;       output.append(current)
;       output.append(mktok('{'))
      ((eq? current_src "where")
        (begin
          (assert (< (+ inx 1) (length tokens)) 'dangling-where)
          (let ((next-token (nth (+ inx 1) tokens)))
            (push '("where" (nth 1 (nth 2 next-token)) (nth 0 (nth 2 next-token)))))
          (append-output current)
          (append-output '(lcb "{"))))

;     elif current['src'] == 'case':
;       assert inx + 1 < len(tokens), 'Dangling case'
;       indent_stack.append(('case', tokens[inx+1]['column_number'], tokens[inx+1]['line_number']))
;       output.append(current)
      ((eq? current_src "case")
        (begin
          (assert (< (+ inx 1) (length tokens)) 'dangling-case)
          (let ((next-token (nth (+ inx 1) tokens)))
            (push '("case" (nth 1 (nth 2 next-token)) (nth 0 (nth 2 next-token)))))
          (append-output current)))

;     elif current['src'] == 'of':
;       assert inx + 1 < len(tokens), 'Dangling of'
;       indent_stack.append(('of', tokens[inx+1]['column_number'], tokens[inx+1]['line_number']))
;       output.append(current)
;       output.append(mktok('{'))
      ((eq? current_src "of")
        (begin
          (assert (< (+ inx 1) (length tokens)) 'dangling-of)
          (let ((next-token (nth (+ inx 1) tokens)))
            (push '("of" (nth 1 (nth 2 next-token)) (nth 0 (nth 2 next-token)))))
          (append-output current)
          (append-output '(lcb "{"))))

;     else:
;       output.append(current)
      (else
        (append-output current)))
;     inx += 1
    (setq inx (+ inx 1))))
; 
;   # Implicit final dedent
;   while len(indent_stack) > 1:
;     if indent_stack[-1][0] == 'let':
;       assert False, 'dangling let'
;     elif indent_stack[-1][0] == 'where':
;       output.append(mktok('}'))
;       indent_stack.pop()
;     elif indent_stack[-1][0] == 'case':
;       assert False, 'dangling case'
;     elif indent_stack[-1][0] == 'of':
;       output.append(mktok('}'))
;       indent_stack.pop()
;       assert indent_stack[-1][0] == 'case'
;       indent_stack.pop()
;     else:
;       assert False, '???'
    (while (> (length indent_stack) 1)
      (cond
        ((equal? (nth 0 (last indent_stack)) "let")
          (assert #f 'dangling-let))
        ((equal? (nth 0 (last indent_stack)) "where")
          (begin
            (append-output '(rcb "}"))
            (pop)))
        ((equal? (nth 0 (last indent_stack)) "case")
          (assert #f 'dangling-case))
        ((equal? (nth 0 (last indent_stack)) "of")
          (begin
            (append-output '(rcb "}"))
            (pop)
            (assert (equal? (nth 0 (last indent_stack)) "case"))
            (pop)))
        (else (assert #f "???"))))
; 
;   assert len(indent_stack) == 1, indent_stack
    (assert (eq? (length indent_stack) 1))
; 
;   return tokens_to_src(output)
    output))

(define (tokens->src-1 tokens current-line current-column)
  (mtch tokens
    ((a as) . rest)
      (++ " " as (tokens->src-1 rest current-line (+ 1 current-column (string-length as))))
    ((a as (line column)) . rest)
      (++ (apply ++ (ntimes (max 0 (- line current-line)) "\n"))
          (apply ++ (ntimes (max 1 (- column (if (> line current-line) 0 current-column))) " "))
          as
          (tokens->src-1 rest line (+ column (string-length as))))
    '() ""))
(define (tokens->src tokens) (tokens->src-1 tokens -1 0))

(define (should-insert-semicolon tokens group-stack)
  (mtch (list tokens group-stack)
    (((a as (ra ca)) . tokens) ((group-type (b bs (rb cb))) . gss))
    (and (member group-type '(let_keyword of_keyword where_keyword))
         (> ra rb)
         (eq? ca cb))
    x #f))

(define (is-dedent-block-close? tokens group-stack)
  (mtch (list tokens group-stack)
    (((a as (ra ca)) . tokens) ((group-type (b bs (rb cb))) . gss))
    (and (member group-type '(let_keyword of_keyword where_keyword))
         (> ra rb)
         (< ca cb))
    x #f))

(define (preprocess tokens group-stack)
  (if (is-dedent-block-close? tokens group-stack)
    (cons '(rcb "}") (preprocess tokens (cdr group-stack)))
    (mtch tokens
      '()
        '()
      (('let_keyword . x) next . rest)
        `((let_keyword . ,x) (lcb "{") . ,(preprocess `(,next . ,rest) `((let_keyword ,next) . ,group-stack)))
      (('where_keyword . x) next . rest)
        `((where_keyword . ,x) (lcb "{") . ,(preprocess `(,next . ,rest) `((where_keyword ,next) . ,group-stack)))
      (('of_keyword . x) next . rest)
        `((of_keyword . ,x) (lcb "{") . ,(preprocess `(,next . ,rest) `((of_keyword ,next) . ,group-stack)))
      ;(('in_keyword . x) . rest)
        ;(mtch group-stack
          ;(('let_keyword next) . gs-rest)
            ;`((rcb "}") (in_keyword . ,x) . ,(preprocess rest gs-rest)))
      (a . d)
        (append
          (if (should-insert-semicolon tokens group-stack) '((semicolon ";")) '())
          `(,a . ,(preprocess d group-stack))))))
;(tracefun preprocess)

(define (preprocess-top tokens) (preprocess tokens '()))

(display (tokens->src (preprocess-top (tokenize-top (read-file-as-string "input.tmi")))))
