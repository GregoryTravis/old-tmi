import Data.Char (ord)
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import System.IO
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
  ("comment", ";;"),
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

nextToken :: BS.ByteString -> ((String, BS.ByteString), BS.ByteString)
nextToken s = case (match bigRegex s []) of
                Just (_ : mtch : m) -> ((getTokenName mtch m, mtch), last m)
                otherwise -> error $ "Bad token at \"" ++ (BS.unpack s) ++ "\""
      where
        regexNames = map fst tokenPatterns
        getTokenName mtch m = regexNames !! (fromJust $ elemIndex mtch m)

tokenizeString1 :: BS.ByteString -> (Int, Int) -> [((String, BS.ByteString), (Int, Int))]
tokenizeString1 s pos
  | s == BS.empty = []
  | otherwise =
    case (nextToken s) of
      (("comment",_), rest) -> tokenizeString1 (skipNewline rest) (advanceByString pos $ BS.take ((BS.length s) - (BS.length (skipNewline rest))) s)
      (token@(_,s), rest) -> (token, pos) : (tokenizeString1 rest (advanceByString pos s))
  where skipNewline :: BS.ByteString -> BS.ByteString
        skipNewline s = case BS.elemIndex '\n' s of
                          Just n -> BS.drop (n + 1) s
                          Nothing -> BS.empty
        --advance (col, row) dcol drow = (col + dcol, row + drow)
        advanceByString :: (Int, Int) -> BS.ByteString -> (Int, Int)
        --advanceByString pos s = eesp (pos, s) $ qadvanceByString pos s
        advanceByString (col, row) s
          | s == BS.empty = (col, row)
          | (BS.head s == '\n') = advanceByString (0, row + 1) (BS.tail s)
          | otherwise = advanceByString (col + 1, row) (BS.tail s)
        --advanceByString pos s = advance pos ((BS.length s) - numNewlines) numNewlines
          --where numNewlines = BS.length (BS.filter ('\n' ==) s)
tokenizeString s = tokenizeString1 s (0, 0)

main = do
  hSetBuffering stdout NoBuffering
  s <- readFile "input.tmi"
  putStrLn $ show $ tokenizeString (BS.pack s)
