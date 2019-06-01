module Util
( esp
, eesp
, sp
, msp
, fromLeftReal
) where

import Data.Either
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import System.IO.Unsafe
import Text.Pretty.Simple (pShow, pShowNoColor)

esp a = unsafePerformIO $ do
  putStrLn $ show $ a
  return a

eesp s a = unsafePerformIO $ do
  putStrLn $ show $ s
  return a

sp x = unpack $ toStrict $ pShowNoColor $ x
msp x = putStrLn $ sp x

-- Really surprised this doesn't exist
fromLeftReal (Left a) = a
