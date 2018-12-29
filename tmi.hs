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
  let prep = (preprocess tokens)

  let parsed = fromJust $ parseTmi prep
  let sem = p2s parsed
  msp $ length (show sem)

  let parsed2 = splitParse tokens
  case parsed2 of
    Left parsed -> do
      let sem2 = relet $ map unlet $ map p2s parsed
      msp $ length (show sem2)
      msp $ sem == sem2
      --msp sem2
    Right bads -> msp bads
  where unlet (Let (Decls [def]) _) = def
        relet defs = Let (Decls defs) (App [(Id "main")])
