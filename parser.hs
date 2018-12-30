module Parser
( Feh (..)
, GExp(..)
, Grammar(..)
, Rule(..)
, parse
) where

import Data.Function (fix)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (partition, find)
import Data.Maybe-- (isJust)
import Data.List.Utils (startswith)
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)

import Preprocess
import Tokenize
import Util

data GExp = NT String | T String | Alt [GExp] | Seq [GExp]
  deriving (Show, Ord, Eq)
data Rule = Rule String GExp
  deriving Show
data Grammar = Grammar [Rule]
  deriving Show

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

data Feh = PNT String Feh | PT String String | PSeq [Feh] deriving (Eq, Show)

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

memoize :: (Ord a, Ord b) => (rec -> a -> b -> c) -> (rec -> a -> b -> c)
memoize f = unsafePerformIO $ do 
    r <- newIORef Map.empty
    --hitCountRef <- newIORef 0
    --callCountRef <- newIORef 0
    --putStrLn "Wrapping"
    return $ \ rec x y -> unsafePerformIO $ do 
        --hitCount <- readIORef hitCountRef
        --callCount <- readIORef callCountRef
        --writeIORef callCountRef $ callCount + 1
        m <- readIORef r
        res <- case Map.lookup (x, y) m of
                   Just v  -> do
                     --writeIORef hitCountRef $ hitCount + 1
                     return v
                   Nothing -> do 
                           let v = f rec x y
                           writeIORef r (Map.insert (x, y) v m)
                           return v
        --hitCount <- readIORef hitCountRef
        --callCount <- readIORef callCountRef
        --putStrLn $ "MEM " ++ (show hitCount) ++ " " ++ (show callCount)
        return res

parse1 :: Grammar -> Vector PosToken -> GExp -> Int -> Maybe (Feh, Int)
parse1 grammar tokens = fix (memoize parseOR)
  where parseOR :: (GExp -> Int -> Maybe (Feh, Int)) -> GExp -> Int -> Maybe (Feh, Int)
        parseOR rec (NT sym) pos =
          case (lookupRule grammar sym) of
            Just (Rule nt exp) ->
              case rec exp pos of
                Just (x, newPos) -> Just (PNT sym x, newPos)
                Nothing -> Nothing
            Nothing -> Nothing
        parseOR rec (T sym) pos =
          if pos < V.length tokens
            then case (tokens ! pos) of PosToken ty s _ -> if ty == sym then Just (PT sym s, pos + 1) else Nothing
            else Nothing
        parseOR rec (Alt [a, b]) pos =
          case rec a pos of
            Just x -> Just x
            Nothing -> rec b pos
        parseOR rec (Seq [a, b]) pos =
          case rec a pos of
            Just (fehA, newPos) ->
              case rec b newPos of
                Just (fehB, newnewPos) ->
                  Just $ (PSeq [fehA, fehB], newnewPos)
                Nothing -> Nothing
            Nothing -> Nothing
        parseOR rec x@(Alt _) _ = error ("nope" ++ show x)
        parseOR rec (Seq _) _ = error "nope2"

parse :: Grammar -> String -> [PosToken] -> Maybe Feh
parse grammar top tokens =
  case parse1 (binarizeGrammar grammar) (V.fromList tokens) (NT top) 0 of
    Just (binarizedParse, finalPos) | finalPos == length tokens -> Just (unbinarizeParse binarizedParse)
    Nothing -> Nothing
