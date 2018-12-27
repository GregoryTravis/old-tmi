module Parser
( Feh (..)
, parseTmi ) where

import Data.List (find)
import Data.List.Utils (startswith)
import Debug.Trace (trace)
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

{-
memoize :: Ord a => (a -> b) -> (a -> b)
memoize f = unsafePerformIO $ do 
    r <- newIORef Map.empty
    return $ \ x -> unsafePerformIO $ do 
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do 
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y
-}

{-
memoize :: Ord a => (a -> b) -> (a -> b)
memoize f = unsafePerformIO $ do 
    r <- newIORef Map.empty
    return $ \ x -> unsafePerformIO $ do 
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do 
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y
-}

parse :: Grammar -> [PosToken] -> GExp -> Int -> Maybe (Feh, Int)
--parse _ _ e pos | trace ("parse " ++ (show e) ++ " " ++ (show pos)) False = undefined
parse grammar tokens (NT sym) pos =
  case (lookupRule grammar sym) of
    Just (Rule nt exp) ->
      case parse grammar tokens exp pos of
        Just (x, newPos) -> Just (PNT sym x, newPos)
        Nothing -> Nothing
    Nothing -> Nothing
parse grammar tokens (T sym) pos =
  if pos < length tokens
    then case (tokens !! pos) of PosToken ty s _ -> if ty == sym then Just (PT sym s, pos + 1) else Nothing
    else Nothing
--parse grammar tokens (T sym) _ = Nothing
parse grammar tokens (Alt [a, b]) pos =
  case parse grammar tokens a pos of
    Just x -> Just x
    Nothing -> parse grammar tokens b pos
parse grammar tokens (Seq [a, b]) pos =
  case parse grammar tokens a pos of
    Just (fehA, newPos) ->
      case parse grammar tokens b newPos of
        Just (fehB, newnewPos) ->
          Just $ (PSeq [fehA, fehB], newnewPos)
        Nothing -> Nothing
    Nothing -> Nothing
parse grammar tokens x@(Alt _) _ = error ("nope" ++ show x)
parse grammar tokens (Seq _) _ = error "nope2"

{-
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
-}

tmiGrammar = Grammar [
  Rule "Top" $ NT "plet",
  Rule "plet" $ Seq [T "let_keyword", T "p-lcb", NT "decls", T "p-rcb", T "in_keyword", NT "exp"],
  Rule "pwhere-suffix" $ Seq [T "where_keyword", T "p-lcb", NT "decls", T "p-rcb"],
  Rule "pdo" $ Alt [Seq [T "do_keyword", T "p-lcb", NT "exp", T "p-rcb"],
                    Seq [T "do_keyword", T "p-lcb", NT "do_assignments", T "semicolon", NT "exp", T "p-rcb"]],
  Rule "do_assignments" $ Alt [Seq [NT "do_assignment", T "semicolon", NT "do_assignments"], NT "do_assignment"],
  Rule "do_assignment" $ Seq [NT "exp", T "larrow", NT "exp"],
  Rule "definition" $ Seq [NT "exp", T "equals", NT "exp"],
  Rule "decls" $ Alt [Seq [NT "definition", T "semicolon", NT "decls"], NT "definition"],
  Rule "parenexp" $ Seq [T "lparen", NT "exp", T "rparen"],
  Rule "listexp" $ Alt [Seq [T "lsb", T "rsb"], Seq [T "lsb", NT "comma-separated-exp-sequence", T "rsb"]],
  Rule "comma-separated-exp-sequence" $ Alt [Seq [NT "exp", T "comma", NT "comma-separated-exp-sequence"], NT "exp"],
  Rule "lambda-exp" $ Seq [T "lambda", NT "parenexp", NT "exp"],
  Rule "base-exp" $ Alt [T "constructor", T "identifier", T "integer", T "operator", T "unary-operator",
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
  Rule "phash" $ Alt [Seq [T "p-lcb", T "p-rcb"], Seq [T "p-lcb", NT "phash-entries", T "p-rcb"]],
  Rule "phash-entries" $ Alt [Seq [NT "phash-entry", T "comma", NT "phash-entries"], NT "phash-entry"],
  Rule "phash-entry" $ Seq [T "identifier", T "colon", NT "exp"]
  ]

parseTmi tokens = case parse (binarizeGrammar tmiGrammar) tokens (NT "Top") 0 of
                       Just (binarizedParse, finalPos) | finalPos == length tokens -> Just (unbinarizeParse binarizedParse)
                       Nothing -> Nothing
