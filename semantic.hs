module Semantic
( p2s
) where

import Parser (Feh (..))

data Sem = Decls [Sem] | Def Sem Sem | Um Feh
  deriving Show

p2s :: Feh -> Sem
p2s (PNT "Top" f) = p2s f
p2s (PNT "decls" (PSeq [def, _, decls])) =
  case (p2s decls) of Decls defs -> Decls $ (p2s def) : defs
p2s (PNT "decls" def) = Decls [p2s def]
p2s (PNT "definition" (PSeq [lhs, _, rhs])) = Def (p2s lhs) (p2s rhs)
p2s x = Um x
