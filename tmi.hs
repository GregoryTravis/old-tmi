import Data.Maybe (fromJust)
import System.IO

import TmiParser
import Tokenize
import Util

main = do
  hSetBuffering stdout NoBuffering
  sem <- tmiParseFile "input.tmi"
  msp sem
