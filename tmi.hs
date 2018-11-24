import Data.List
import Data.Maybe
import Text.Regex.Posix
import Text.Regex

tokenPatterns = [
  ("whitespace", "[ \\t\\n]+"),
  --("let_keyword", "let"),
  --("in_keyword", "in(?![a-zA-Z0-9])"),
  --("do_keyword", "do(?![a-zA-Z0-9])"),
  --("larrow", "<-"),
  --("case_keyword", "case"),
  --("of_keyword", "of"),
  --("rdbl_arrow", "=>"),
  --("where_keyword", "where"),
  --("if_keyword", "if"),
  --("then_keyword", "then"),
  --("else_keyword", "else"),
  --("integer", "[0-9]+"),
  --("constructor", "[A-Z][a-zA-Z0-9_]*"),
  ("identifier", "[a-zA-Z0-9_][a-zA-Z0-9_=<>+/\\-_!@$%^&|*?]*"),
  --("comma", ","),
  --("comment", ";;"),
  --("colon", ":"),
  --("semicolon", ";"),
  --("equals", "=(?=\\s)"),
  --("lambda", "/\\.(?=\\s)"),
  --("unary-operator", "!(?!=)"), -- "!(?!=)|~"
  --("operator", "[=<>+/\\-_!@$%^&|*?]+"),
  --("lparen", "[\\(]"),
  --("rparen", "[\\)]"),
  --("lsb", "[\\[]"),
  --("rsb", "[\\]]"),
  --("lcb", "\\{"),
  --("rcb", "\\}"),
  --("string", "\"((\\\\\\\\)|(\\\\\")|[^\"])*\"")
  ("asdf", "asdf")
  ]

bigRegex = "^(" ++ combined ++ ")(.*)"
  where combined = intercalate "|" subRes
        subRes = map enParen $ map snd tokenPatterns
        enParen x = "(" ++ x ++ ")"

main = do
  s <- readFile "input.tmi"
  --putStrLn $ show ("bar" =~ "(foo|bar)" :: Bool)
  let re2 = mkRegexWithOpts bigRegex False True 
  let re3 = mkRegexWithOpts "([ \\t\\n])" False True 
  putStrLn bigRegex
  putStrLn s
  putStrLn $ show (matchRegex re2 s)
  putStrLn (tail s)
  putStrLn $ show (matchRegex re2 (tail s))
  putStrLn $ show $ matchOne s
  putStrLn $ show $ matchOne (tail s)
  where matchOne s = case (matchRegex re2 s) of Just m -> (regexNames !! (fromJust $ findIndex ((head m) ==) (tail m)), head m)
        regexNames = map fst tokenPatterns
        re2 = mkRegexWithOpts bigRegex False True 

  --(whitespace "\\s+")
