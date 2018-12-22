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
  let prep = preprocess tokens
  putStrLn $ show prep
  putStrLn $ renderTokens prep
  let parsed = fromJust $ parseTmi prep
  putStrLn $ show parsed
  --putStrLn $ unpack $ toStrict $ pShow $ p2s parsed
  msp $ p2s parsed
