import System.IO
import Tokenize

main = do
  hSetBuffering stdout NoBuffering
  s <- readFile "input.tmi"
  putStrLn $ show $ tokenizeString s
