module Tokenize
( tokenizeString
--, renderTokens
, PosToken (..)
, Token (..)) where

import Data.Char (ord)
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import Text.Regex.PCRE.Light

data Token = Token String String deriving Show
data PosToken = PosToken String String (Int, Int) deriving Show

tokenPatterns = [
  ("whitespace", "[ \\t\\n]+"),
  ("let_keyword", "let"),
  ("in_keyword", "in(?![a-zA-Z0-9])"),
  ("do_keyword", "do(?![a-zA-Z0-9])"),
  ("larrow", "<-"),
  ("case_keyword", "case"),
  ("of_keyword", "of"),
  ("rdbl_arrow", "=>"),
  ("where_keyword", "where"),
  ("if_keyword", "if"),
  ("then_keyword", "then"),
  ("else_keyword", "else"),
  ("integer", "[0-9]+"),
  ("constructor", "[A-Z][a-zA-Z0-9_]*"),
  ("identifier", "[a-zA-Z0-9_][a-zA-Z0-9_=<>+/\\-_!@$%^&|*?]*"),
  ("comma", ","),
  ("comment", ";;[^\\n]*\\n"),
  ("colon", ":"),
  ("semicolon", ";"),
  ("equals", "=(?=\\s)"),
  ("unary-operator", "!(?!=)"), -- "!(?!=)|~"
  ("lambda", "/\\.(?=\\s)"),
  ("operator", "[=<>+/\\-_!@$%^&|*?]+"),
  ("lparen", "[\\(]"),
  ("rparen", "[\\)]"),
  ("lsb", "[\\[]"),
  ("rsb", "[\\]]"),
  ("lcb", "\\{"),
  ("rcb", "\\}"),
  --("string", "\"((\\\\\\\\)|(\\\\\")|[^\"])*\"")
  -- Must insert dummies if you move this up
  ("string", "(\"((\\\\([tn]|\\\\))|(\\\\\")|[^\"])*\")")
  -- Must insert dummies if you add more here
  ]

bigRegex = compile (BS.pack $ "^(" ++ combined ++ ")(.*$)") [dotall]
  where combined = intercalate "|" subRes
        subRes = map enParen $ map snd tokenPatterns
        enParen x = "(" ++ x ++ ")"

nextToken :: BS.ByteString -> (Token, BS.ByteString)
nextToken s = case (match bigRegex s []) of
                Just (_ : mtch : m) -> (Token (getTokenName mtch m) (BS.unpack mtch), last m)
                otherwise -> error $ "Bad token at \"" ++ (BS.unpack s) ++ "\""
      where
        regexNames = map fst tokenPatterns
        getTokenName mtch m = regexNames !! (fromJust $ elemIndex mtch m)

removeWhitespace (PosToken "whitespace" _ _ : ts) = removeWhitespace ts
removeWhitespace (t : ts) = t : removeWhitespace ts
removeWhitespace [] = []

tokenizeString1 :: BS.ByteString -> (Int, Int) -> [PosToken]
tokenizeString1 s pos
  | s == BS.empty = []
  | otherwise =
    case (nextToken s) of
      (Token "comment" s, rest) ->
        (tokenizeString1 rest (advanceByString pos (BS.pack s)))
      (token@(Token name s), rest) ->
        (PosToken name s pos) : (tokenizeString1 rest (advanceByString pos (BS.pack s)))
  where advanceByString :: (Int, Int) -> BS.ByteString -> (Int, Int)
        advanceByString (col, row) s
          | s == BS.empty = (col, row)
          | (BS.head s == '\n') = advanceByString (0, row + 1) (BS.tail s)
          | otherwise = advanceByString (col + 1, row) (BS.tail s)
tokenizeString s = removeWhitespace $ tokenizeString1 (BS.pack s) (0, 0)

{-
advanceTo currentCol currentRow destCol destRow =
  (spaces, newlines)
  where newlines
          | destRow > currentRow = destRow - currentRow
          | otherwise = 0
          -- | destRow > currentRow = replicate (destRow - currentRow) '\n'
          -- | otherwise = ""
        spaces
          | currentCol < destCol = destCol - currentCol
          | otherwise = 0
          -- | currentCol < destCol = replicate (destCol - currentCol) ' '
          -- | otherwise = ""
renderWhitespace spaces newlines =
  (replicate newlines '\n') ++ (replicate spaces ' ')

renderTokens tokens = (renderTokens1 tokens 0 0) ++ "\n"
renderTokens1 (PosToken ty s (c, r) : tokens) col row =
  let (spaces, newlines) = (advanceTo col row c r)
   in (renderWhitespace spaces newlines) ++ s ++ renderTokens1 tokens (c + (length s)) r
renderTokens1 [] _ _ = ""
-}
