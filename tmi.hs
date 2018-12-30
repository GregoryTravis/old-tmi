import Data.Maybe (fromJust)
import System.IO

import TmiParser
import Tokenize
import Util

main = do
  hSetBuffering stdout NoBuffering
  s <- readFile "input.tmi"
  let tokens = tokenizeString s
  msp $ tmiParse tokens
