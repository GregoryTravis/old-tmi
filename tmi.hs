import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import System.IO.Unsafe
import Text.Regex.PCRE.Light
--import Text.Regex

esp a = unsafePerformIO $ do
  putStrLn $ show $ a
  return a

eesp s a = unsafePerformIO $ do
  putStrLn $ show $ s
  return a

tokenPatterns = [
  ("whitespace", "[ \t\n]+"),
  ("let_keyword", "let"),

  --("in_keyword", "in(?![a-zA-Z0-9])"),
  ("in_keyword", "in(?![a-zA-Z0-9])"),
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
  ("equals", "=(?=\\s)"),
  ("unary-operator", "!(?!=)"), -- "!(?!=)|~"
  ("operator", "[=<>+/\\-_!@$%^&|*?]+"),
  ("lambda", "/\\.(?=\\s)"),
  ("lparen", "[\\(]"),
  ("rparen", "[\\)]"),
  ("lsb", "[\\[]"),
  ("rsb", "[\\]]"),
  ("lcb", "\\{"),
  ("rcb", "\\}"),
  --("string", "\"((\\\\\\\\)|(\\\\\")|[^\"])*\"")
  ("asdf", "asdf")
  ]

bigRegex = "^(" ++ combined ++ ")(.*$)"
  where combined = intercalate "|" subRes
        subRes = map enParen $ map snd tokenPatterns
        enParen x = "(" ++ x ++ ")"

nextToken :: BS.ByteString -> ((String, BS.ByteString), BS.ByteString)
nextToken s = case (match re2 s []) of
                Just (_ : mtch : m) -> ((getTokenName mtch m, mtch), last m)
                otherwise -> error $ "Bad token at \"" ++ (BS.unpack s) ++ "\""
      where
        re2 = compile (BS.pack bigRegex) [multiline]
        regexNames = map fst tokenPatterns
        getTokenName mtch m = regexNames !! (fromJust $ elemIndex mtch m)

tokenizeString :: BS.ByteString -> [(String, BS.ByteString)]
tokenizeString s =
  let (token, rest) = (nextToken s) in
    if ((BS.length rest) == 0)
      then [token]
      else token : (tokenizeString rest)

main = do
  s <- readFile "input.tmi"
  putStrLn $ show $ tokenizeString (BS.pack s)
