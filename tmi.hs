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
