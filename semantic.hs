module Semantic
( p2s
) where

import Parser (Feh (..))

data Sem = Decls [Sem] | Def Sem Sem | Id String | Let Sem Sem | SInt Int | App [Sem] | Op String |
  Do [Sem] Sem | Binding Sem Sem | Lambda Sem Sem | Um Feh
  deriving Show

p2s :: Feh -> Sem
p2s (PNT "Top" f) = p2s f
p2s (PNT "decls" (PSeq [def, _, decls])) =
  case (p2s decls) of Decls defs -> Decls $ (p2s def) : defs
p2s (PNT "decls" def) = Decls [p2s def]
p2s (PNT "definition" (PSeq [lhs, _, rhs])) = Def (p2s lhs) (p2s rhs)
p2s (PNT "exp" e) = p2s e
p2s (PNT "base-exp" e) = p2s e
p2s (PNT "non-where-exp" e) = p2s e
p2s (PNT "base-exp-seq" (PSeq [be, bes])) =
  case (p2s bes) of App es -> App $ (p2s be) : es
p2s (PNT "base-exp-seq" e) = App [p2s e]
p2s (PNT "plet" (PSeq [_, _, decls, _, _, e])) = Let (p2s decls) (p2s e)
p2s (PNT "pdo" (PSeq [_, _, bindings, _, ret, _])) = Do (map p2s (unwrap bindings)) (p2s ret)
  where unwrap (PNT "do_assignments" (PSeq [binding, _, bindings])) = binding : unwrap bindings
        unwrap (PNT "do_assignments" binding) = [binding]
p2s (PNT "do_assignment" (PSeq [var, _, exp])) = Binding (p2s var) (p2s exp)
p2s (PNT "parenexp" (PSeq [_, exp, _])) = p2s exp
p2s (PNT "lambda-exp" (PSeq [_, args, body])) = Lambda (p2s args) (p2s body)
p2s (PT "identifier" id) = Id id
p2s (PT "integer" id) = SInt (read id :: Int)
p2s (PT "operator" op) = Op op
p2s x = Um x
