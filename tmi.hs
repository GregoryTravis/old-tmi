import Data.Maybe (fromJust)
import System.IO

import TmiParser
import Preprocess
import Tokenize
import Util

main = do
  hSetBuffering stdout NoBuffering
  s <- readFile "input.tmi"
  let tokens = tokenizeString s
  let prep = (preprocess tokens)

  let sem = fromJust $ tmiParse prep
  msp $ length (show sem)

  let sem2 = fromLeftReal $ tmiSplitParse tokens
  msp $ length (show sem2)
  msp $ sem == sem2
  --msp $ sem == sem2
{-
  case parsed2 of
    Left parsed -> do
      let sem2 = relet $ map unlet $ map p2s parsed
      msp $ length (show sem2)
      msp $ sem == sem2
      --msp sem2
    Right bads -> msp bads
  where unlet (Let (Decls [def]) _) = def
        relet defs = Let (Decls defs) (App [(Id "main")])
-}
