import qualified Data.ByteString.Char8 as BS
import System.IO
import Tokenize

main = do
  hSetBuffering stdout NoBuffering
  s <- readFile "input.tmi"
  putStrLn $ show $ tokenizeString (BS.pack s)
