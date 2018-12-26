module Preprocess where

import Debug.Trace (trace)

import Tokenize
import Util
-- module Preprocess (preprocess) where

-- Elements on the group stack -- ctor keyword and token is the one whose column we compare to
data GroupType = Let | Where | Of | Do | In deriving (Eq, Show)
data Group = Group GroupType PosToken deriving Show
--preprocess1 tokens groupStack | trace ("preprocess1" ++ (show tokens) ++ (show groupStack) ++ (show (isDedentBlockClose tokens groupStack)) ++ (show (shouldInsertSemiColon tokens groupStack))) False = undefined
preprocess1 tokens groupStack =
  if isDedentBlockClose tokens groupStack
    then case (tokens, groupStack) of
           -- Dedent from an 'in' inside a let
           (token@(PosToken "in_keyword" _ _) : ts, Group Let _ : gs) ->
             PosToken "p-rcb" "}" (rightBefore token) : token : preprocess1 ts gs
           -- Detent from an 'in' otherwise (redundant to the next one?)
           (token@(PosToken "in_keyword" _ _) : _, g : gs) ->
             PosToken "p-rcb" "}" (rightBefore token) : preprocess1 tokens gs
           -- EOF dedent: put the added token where the EOF was and move the EOF to be after it
           ([token@(PosToken "EOF" "eof" pos)], g : gs) ->
             PosToken "p-rcb" "}" pos : preprocess1 [PosToken "EOF" "eof" (rightAfter token)] gs
           ([PosToken "EOF" "eof" pos], []) ->
             []
           -- Any other dedent
           (token : _, g : gs) ->
             PosToken "p-rcb" "}" (rightBefore token) : preprocess1 tokens gs

{-
(define (preprocess tokens group-stack)
  (if (is-dedent-block-close? tokens group-stack)
    (mtch (list tokens group-stack)
      ((('in_keyword . x) . rest) (('let_keyword . xx) . gs-rest))
        (cons '(p-rcb "}") (cons `(in_keyword . ,x) (preprocess rest gs-rest)))
      ((('in_keyword . x) . rest) group-stack)
        (cons '(p-rcb "}") (preprocess tokens (cdr group-stack)))
      (x group-stack)
        (cons '(p-rcb "}") (preprocess tokens (cdr group-stack))))
-}
    else case tokens of
           -- Done
           [PosToken "EOF" "eof" pos] ->
             []
           [] ->
             error "No EOF"
           -- Open a let block
           token@(PosToken "let_keyword" _ _) : next : rest ->
             token : PosToken "p-lcb" "{" (rightAfter token) : preprocess1 (next : rest) (Group Let next : groupStack)
           -- Open a where block
           token@(PosToken "where_keyword" _ _) : next : rest ->
             token : PosToken "p-lcb" "{" (rightAfter token) : preprocess1 (next : rest) (Group Where next : groupStack)
           -- Open an of block
           token@(PosToken "of_keyword" _ _) : next : rest ->
             token : PosToken "p-lcb" "{" (rightAfter token) : preprocess1 (next : rest) (Group Of next : groupStack)
           -- Closing let decls, open an in block
           token@(PosToken "in_keyword" _ _) : rest ->
             case groupStack of (Group Let _) : gsRest -> PosToken "p-rcb" "}" (rightBefore token) : token : preprocess1 rest gsRest
           token@(PosToken "do_keyword" _ _) : next : rest ->
             token : PosToken "p-lcb" "{" (rightAfter token) : preprocess1 (next : rest) (Group Do next : groupStack)
           token : ts ->
             (if shouldInsertSemiColon tokens groupStack
               then [PosToken "semicolon" ";" (rightBefore token)]
               else []) ++ (token : preprocess1 ts groupStack)

-- Add sentinel EOF to the end of tokens
-- Start with a fake Let group on the stack, since the TLFs are sort of implicitly in a big let
preprocess tokens = (preprocess1 (pre ++ tokens ++ post) [])
  where pre = [PosToken "let_keyword" "let" (-1, -1)]
        post = [PosToken "in_keyword" "in" (-1, postLine),
                PosToken "identifier" "main" (2, postLine),
                PosToken "EOF" "eof" (7, postLine)]
        postLine = case (last tokens) of PosToken _ _ (_, lastLine) -> lastLine + 1

rightAfter (PosToken _ s (c, r)) = (c + length s, r)
rightBefore (PosToken _ _ (c, r)) = (c - 1, r)

{-
    (mtch tokens
      '()
        '()
      (('let_keyword . x) next . rest)
        `((let_keyword . ,x) (p-lcb "{") . ,(preprocess `(,next . ,rest) `((let_keyword ,next) . ,group-stack)))
      (('where_keyword . x) next . rest)
        `((where_keyword . ,x) (p-lcb "{") . ,(preprocess `(,next . ,rest) `((where_keyword ,next) . ,group-stack)))
      (('of_keyword . x) next . rest)
        `((of_keyword . ,x) (p-lcb "{") . ,(preprocess `(,next . ,rest) `((of_keyword ,next) . ,group-stack)))
      (('in_keyword . x) . rest)
        (mtch group-stack
          (('let_keyword next) . gs-rest)
            `((p-rcb "}") (in_keyword . ,x) . ,(preprocess rest gs-rest)))
      (('do_keyword . x) next . rest)
        `((do_keyword . ,x) (p-lcb "{") . ,(preprocess `(,next . ,rest) `((do_keyword ,next) . ,group-stack)))
      (a . d)
        (append
          (if (should-insert-semicolon tokens group-stack) '((semicolon ";")) '())
          `(,a . ,(preprocess d group-stack))))))
-}

isEqdent (ca, ra) (cb, rb) = ra > rb && ca == cb
isDedent (ca, ra) (cb, rb) = ra > rb && ca < cb

shouldInsertSemiColon (PosToken _ _ tPos : _) (Group groupType (PosToken _ _ gPos) : _) =
  -- Is eqdent and not an 'in' block
  groupType /= In && isEqdent tPos gPos
shouldInsertSemiColon _ _ = False

{-
(define (should-insert-semicolon tokens group-stack)
  (mtch (list tokens group-stack)
    (((a as (ra ca)) . tokens) ((group-type (b bs (rb cb))) . gss))
    (and (member group-type '(let_keyword of_keyword where_keyword do_keyword))
         (> ra rb)
         (eq? ca cb))
    x #f))
-}

-- The Let at the very end is the fake one we added at the start, so don't close it
isDedentBlockClose [PosToken "EOF" _ _] [Group Let _] = False
isDedentBlockClose [PosToken "EOF" _ _] _ = True
isDedentBlockClose (PosToken _ _ tPos : _) (Group groupType (PosToken _ _ gPos) : _) =
  groupType /= In && isDedent tPos gPos
isDedentBlockClose _ _ = False

{-
(define (is-dedent-block-close? tokens group-stack)
  (mtch (list tokens group-stack)
    (((a as (ra ca)) . tokens) ((group-type (b bs (rb cb))) . gss))
     (and (member group-type '(let_keyword of_keyword where_keyword do_keyword))
          (> ra rb)
          (< ca cb))
    ('() (('of_keyword (b bs (rb cb))) . gss))
      #t
    ('() (('where_keyword (b bs (rb cb))) . gss))
      #t
    x
      #f))
-}
