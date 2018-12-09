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
  --putStrLn $ show grammar
  --putStrLn $ show $ binarizeGrammar grammar
  --putStrLn $ show haha
  putStrLn $ show $ binarizeGrammar tmiGrammar
  let parsed = parseTmi prep
  putStrLn $ show parsed
  putStrLn $ show $ (case parsed of Just (f, []) -> unbinarizeParse f)
