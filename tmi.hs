import Data.Maybe (fromJust)
import System.IO

import Parser
import Preprocess
import Semantic
import Tokenize
import Util

main = do
  hSetBuffering stdout NoBuffering
  s <- readFile "input.tmi"
  let tokens = tokenizeString s
  putStrLn $ show tokens
  putStrLn $ renderTokens tokens
  let prep = (preprocess tokens)
  putStrLn $ show prep
  putStrLn $ renderTokens prep
  let parsed = fromJust $ parseTmi prep
  putStrLn $ show parsed
  --putStrLn $ unpack $ toStrict $ pShow $ p2s parsed
  let sem = p2s parsed
  msp sem
  let parsed2 = splitParse tokens
  --putStrLn $ show parsed2
  --msp parsed2
  case parsed2 of
    Left parsed -> do
      let sem2 = relet $ map unlet $ map p2s parsed
      msp sem2
      msp $ sem == sem2
    Right bads -> msp bads
  where unlet (Let (Decls [def]) _) = def
        relet defs = Let (Decls defs) (App [(Id "main")])
