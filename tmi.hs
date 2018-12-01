import System.IO
import Parser
import Preprocess
import Tokenize

data Foo = Foo Int

main = do
  hSetBuffering stdout NoBuffering
  s <- readFile "input.tmi"
  let tokens = tokenizeString s
  putStrLn $ show tokens
  putStrLn $ renderTokens tokens
  let prep = preprocess tokens
  putStrLn $ show prep
  putStrLn $ renderTokens prep
  putStrLn $ show grammar
