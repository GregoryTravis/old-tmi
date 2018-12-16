module Semantic
( p2s
) where

import Parser (Feh (..))

data Sem = Um Feh
  deriving Show

p2s :: Feh -> Sem
p2s (PNT "Top" f) = p2s f
p2s x = Um x
