module Parser
( grammar
, tmiGrammar
, haha
, binarizeGrammar
, unbinarizeParse
, parseTmi ) where

import Data.List (find)
import Data.List.Utils (startswith)
import Tokenize

data GExp = NT String | T String | Alt [GExp] | Seq [GExp]
  deriving Show
data Rule = Rule String GExp
  deriving Show
data Grammar = Grammar [Rule]
  deriving Show

--niceGrammar (Grammar rules) = (intercalate "\n" (map niceRule rules)

--data NTs = "Top" | "Subject" | "Predicate" | "NounPhrase" | "DirectObject"
--data Ts = Noun | Verb | Article | Adjective
  --deriving Show
grammar :: Grammar
grammar = Grammar [
  Rule "Top" $ Seq [NT "Subject", NT "Predicate"],
  Rule "Subject" $ NT "NounPhrase",
  Rule "NounPhrase" $ Alt [T "Noun",
                           Seq [T "Adjective", T "Noun"],
                           Seq [T "Article", T "Adjective", T "Noun"]],
  Rule "Predicate" $ Seq [T "Verb", NT "DirectObject"],
  Rule "DirectObject" $ NT "NounPhrase"
  ]

binarizeGrammar :: Grammar -> Grammar
binarizeGrammar (Grammar rules) = Grammar $ binarizeRules rules 0

binarizeRules :: [Rule] -> Int -> [Rule]
binarizeRules (r : rs) n =
  case binarizeRule r n of (rules, newN) -> rules ++ binarizeRules rs newN
binarizeRules [] n = []

binarizeRule :: Rule -> Int -> ([Rule], Int)
binarizeRule (Rule nt exp) n =
  case binarizeExp exp n of
    (newExp, rules, n) -> (Rule nt newExp : rules, n)

binarizeExp :: GExp -> Int -> (GExp, [Rule], Int)
binarizeExp (Alt (a : b : c : rest)) n =
  let newSym = "yyalt" ++ (show n)
   in case binarizeExp a (n + 1) of
     (newA, aRules, newN) ->
       case binarizeExp (Alt (b : c : rest)) newN of
         (newExp, rules, newN) -> (Alt [newA, NT newSym], aRules ++ [(Rule newSym newExp)] ++ rules, newN)
binarizeExp (Alt [a, b]) n =
  case binarizeExp a n of
    (newA, aRules, newN) ->
      case binarizeExp b newN of
        (newB, bRules, newNewN) ->
          (Alt [newA, newB], aRules ++ bRules, newNewN)
binarizeExp (Alt [a]) n =
  case binarizeExp a n of
    (newA, aRules, newN) ->
      (Alt [newA], aRules, newN)
binarizeExp e@(Alt []) n = (e, [], n)
binarizeExp (Seq (a : b : c : rest)) n =
  let newSym = "yyseq" ++ (show n)
   in case binarizeExp a (n + 1) of
     (newA, aRules, newN) ->
       case binarizeExp (Seq (b : c : rest)) newN of
         (newExp, rules, newN) -> (Seq [newA, NT newSym], aRules ++ [(Rule newSym newExp)] ++ rules, newN)
binarizeExp (Seq [a, b]) n =
  case binarizeExp a n of
    (newA, aRules, newN) ->
      case binarizeExp b newN of
        (newB, bRules, newNewN) ->
          (Seq [newA, newB], aRules ++ bRules, newNewN)
binarizeExp (Seq [a]) n =
  case binarizeExp a n of
    (newA, aRules, newN) ->
      (Seq [newA], aRules, newN)
binarizeExp e@(Seq []) n = (e, [], n)
binarizeExp x n = (x, [], n)

lookupRule :: Grammar -> String -> Maybe Rule
lookupRule (Grammar rules) sym = find match rules
  where match :: Rule -> Bool
        match (Rule nt exp) = nt == sym

data Feh = PNT String Feh | PT String String | PSeq [Feh] deriving Show

unbinarizeParse :: Feh -> Feh
unbinarizeParse (PSeq [f, f2@(PNT ntName d@(PSeq fs))])
  | startswith "yyseq" ntName =
      case unbinarizeParse d of
        PSeq fs -> PSeq $ (unbinarizeParse f) : fs
        _ -> error "wut"
  | otherwise =
      PSeq [(unbinarizeParse f), (unbinarizeParse f2)]
unbinarizeParse (PSeq fs) = PSeq (map unbinarizeParse fs)
unbinarizeParse (PNT s f)
  | startswith "yyalt" s = unbinarizeParse f
  | otherwise = PNT s (unbinarizeParse f)
unbinarizeParse x@(PT _ _) = x

-- (PSeq [PT "let_keyword" "let",PNT "yy0" (PSeq [PT "p-lcb" "{",PNT "yy1" 

parse :: Grammar -> GExp -> [PosToken] -> Maybe (Feh, [PosToken])
parse grammar (NT sym) tokens =
  case (lookupRule grammar sym) of
    Just (Rule nt exp) ->
      case parse grammar exp tokens of
        Just (x, newTokens) -> Just (PNT sym x, newTokens)
        Nothing -> Nothing
    Nothing -> Nothing
parse grammar (T sym) (PosToken ty s _ : ts)
  | ty == sym = Just (PT sym s, ts)
  | otherwise = Nothing
parse grammar (T sym) [] = Nothing
parse grammar (Alt [a, b]) tokens =
  case parse grammar a tokens of
    Just x -> Just x
    Nothing -> parse grammar b tokens
parse grammar (Seq [a, b]) tokens =
  case parse grammar a tokens of
    Just (fehA, newTokens) ->
      case parse grammar b newTokens of
        Just (fehB, newNewTokens) ->
          Just $ (PSeq [fehA, fehB], newNewTokens)
        Nothing -> Nothing
    Nothing -> Nothing
parse grammar x@(Alt _) tokens = error ("nope" ++ show x)
parse grammar (Seq _) tokens = error "nope2"

haha = parse (binarizeGrammar grammar) (NT "Top") [
  --PosToken "Article" "q" (0, 0),
  PosToken "Noun" "q" (0, 0),
  PosToken "Verb" "q" (0, 0),
  --PosToken "Article" "q" (0, 0),
  --PosToken "Adjective" "q" (0, 0),
  PosToken "Article" "q" (0, 0),
  PosToken "Adjective" "q" (0, 0),
  PosToken "Noun" "q" (0, 0)
  ]

tmiGrammar = Grammar [
  Rule "Top" $ NT "decls",
  Rule "plet" $ Seq [T "let_keyword", T "p-lcb", NT "decls", T "p-rcb", T "in_keyword", NT "exp"],
  Rule "pwhere-suffix" $ Seq [T "where_keyword", T "p-lcb", NT "decls", T "p-rcb"],
  Rule "pdo" $ Alt [Seq [T "do_keyword", T "p-lcb", NT "exp", T "p-rcb"],
                    Seq [T "do_keyword", NT "p-lcb", NT "do_assignments", T "semicolon", NT "exp", T "p-rcb"]],
  Rule "do_assignments" $ Alt [Seq [NT "do_assignment", T "semicolon", NT "do_assignments"], NT "do_assignment"],
  Rule "do_assignment" $ Seq [NT "exp", T "larrow", NT "exp"],
  Rule "definition" $ Seq [NT "exp", T "equals", NT "exp"],
  Rule "decls" $ Alt [Seq [NT "definition", T "semicolon", NT "decls"], NT "definition"],
  Rule "parenexp" $ Seq [T "lparen", NT "exp", T "rparen"],
  Rule "listexp" $ Alt [Seq [T "lsb", T "rsb"], Seq [T "lsb", NT "comma-separated-exp-sequence", T "rsb"]],
  Rule "comma-separated-exp-sequence" $ Alt [Seq [NT "exp", T "comma", NT "comma-separated-exp-sequence"], NT "exp"],
  Rule "lambda-exp" $ Seq [T "lambda", NT "parenexp", NT "exp"],
  Rule "base-exp" $ Alt [NT "constructor", T "identifier", T "integer", T "operator", T "unary-operator",
                         NT "parenexp", NT "listexp", NT "lambda-exp", T "string", NT "phash"],
  Rule "base-exp-seq" $ Alt [Seq [NT "base-exp", NT "base-exp-seq"], NT "base-exp"],
  Rule "exp" $ Alt [NT "where-exp", NT "non-where-exp"],
  Rule "where-exp" $ Seq [NT "non-where-exp", NT "pwhere-suffices"],
  Rule "pwhere-suffices" $ Alt [Seq [NT "pwhere-suffix", NT "pwhere-suffices"], NT "pwhere-suffix"],
  Rule "non-where-exp" $ Alt [NT "pif", NT "plet", NT "pdo", NT "case", NT "base-exp-seq"],
  Rule "case" $ Seq [T "case_keyword", NT "exp", T "of_keyword", T "p-lcb", NT "case_clauses", T "p-rcb"],
  Rule "case_clauses" $ Alt [Seq [NT "case_clause", T "semicolon", NT "case_clauses"], NT "case_clause"],
  Rule "case_clause" $ Seq [NT "exp", T "rdbl_arrow", NT "exp"],
  Rule "pif" $ Seq [T "if_keyword", NT "exp", T "then_keyword", NT "exp", T "else_keyword", NT "exp"],
  Rule "phash" $ Alt [Seq [T "lcb", T "rcb"], Seq [T "lcb", NT "phash-entries", T "rcb"]],
  Rule "phash-entries" $ Alt [Seq [NT "phash-entry", T "comma", NT "phash-entries"], NT "phash-entry"],
  Rule "phash-entry" $ Seq [T "identifier", T "colon", NT "exp"]
  ]

parseTmi tokens = parse (binarizeGrammar tmiGrammar) (NT "Top") tokens