import System.IO
import Preprocess
import Tokenize

main = do
  hSetBuffering stdout NoBuffering
  s <- readFile "input.tmi"
  let tokens = tokenizeString s
  putStrLn $ show tokens
  --putStrLn $ renderTokens tokens
  let prep = preprocess tokens
  putStrLn $ show prep
  --putStrLn $ renderTokens prep
