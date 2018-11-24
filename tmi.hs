import Data.List
import Data.Maybe
import Text.Regex.Posix
import Text.Regex

tokenPatterns = [
  ("whitespace", "[ \t\n]+"),
  ("let_keyword", "let"),

  --("in_keyword", "in(?![a-zA-Z0-9])"),
  ("in_keyword", "in"),
  --("do_keyword", "do(?![a-zA-Z0-9])"),
  ("do_keyword", "do"),

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
  ("comment", ";;"),
  ("colon", ":"),
  ("semicolon", ";"),

  --("equals", "=(?=\\s)"),
  ("equals", "="),
  --("lambda", "/\\.(?=\\s)"),
  ("lambda", "/\\."),
  --("unary-operator", "!(?!=)"), -- "!(?!=)|~"
  ("unary-operator", "!"), -- "!(?!=)|~"

  ("operator", "[=<>+/\\-_!@$%^&|*?]+"),
  ("lparen", "[\\(]"),
  ("rparen", "[\\)]"),
  ("lsb", "[\\[]"),
  ("rsb", "[\\]]"),
  ("lcb", "\\{"),
  ("rcb", "\\}"),
  --("string", "\"((\\\\\\\\)|(\\\\\")|[^\"])*\"")
  ("asdf", "asdf")
  ]

bigRegex = "^(" ++ combined ++ ")(.*)"
  where combined = intercalate "|" subRes
        subRes = map enParen $ map snd tokenPatterns
        enParen x = "(" ++ x ++ ")"

nextToken s = case (matchRegex re2 s) of
                Just m -> ((getTokenName m, head m), last m)
                otherwise -> error $ "Bad token at \"" ++ s ++ "\""
      where
        re2 = mkRegexWithOpts bigRegex False True 
        regexNames = map fst tokenPatterns
        getTokenName m = regexNames !! (fromJust $ findIndex ((head m) ==) (tail m))

tokenizeString s = case (nextToken s) of
  (token, "") -> [token]
  (token, rest) -> token : (tokenizeString rest)

main = do
  s <- readFile "input.tmi"
  putStrLn $ show $ tokenizeString s
