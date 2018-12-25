module Semantic
( p2s
) where

import Parser (Feh (..))

data Sem = Decls [Sem] | Def Sem Sem | Id String | Let Sem Sem | SInt Int | App [Sem] | Op String
  | Do [Sem] Sem | Binding Sem Sem | Lambda Sem Sem | Ctor String | Str String | Where Sem [Sem]
  | If Sem Sem Sem | List [Sem] | Case Sem [Sem] | Clause Sem Sem | PHash [Sem] | Entry Sem Sem
  | Um Feh
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
p2s (PNT "pdo" (PSeq [_, _, ret, _])) = Do [] (p2s ret)
p2s (PNT "pdo" (PSeq [_, _, bindings, _, ret, _])) = Do (map p2s (unwrap bindings)) (p2s ret)
  where unwrap (PNT "do_assignments" (PSeq [binding, _, bindings])) = binding : unwrap bindings
        unwrap (PNT "do_assignments" binding) = [binding]
p2s (PNT "where-exp" (PSeq [PNT "non-where-exp" e, suffices])) = Where (p2s e) (unwrap suffices)
  where unwrap (PNT "pwhere-suffices" (PSeq [suffix, suffices])) = (p2s suffix) : unwrap suffices
        unwrap (PNT "pwhere-suffices" (PNT "pwhere-suffix" (PSeq [_, _, decls, _]))) = [p2s decls]
p2s (PNT "case" (PSeq [_, e, _, _, clauses, _])) = Case (p2s e) (map p2s (unwrap clauses))
  where unwrap (PNT "case_clauses" (PSeq [clause, _, clauses])) = clause : unwrap clauses
        unwrap (PNT "case_clauses" clause) = [clause]
p2s (PNT "case_clause" (PSeq [pat, _, exp])) = Clause (p2s pat) (p2s exp)
p2s (PNT "do_assignment" (PSeq [var, _, exp])) = Binding (p2s var) (p2s exp)
p2s (PNT "pif" (PSeq [_, b, _, t, _, e])) = If (p2s b) (p2s t) (p2s e)
p2s (PNT "listexp" (PSeq [(PT "lsb" _), (PT "rsb" _)])) = List []
p2s (PNT "listexp" (PSeq [_, cses, _])) = List (map p2s (unwrap cses))
  where unwrap (PNT "comma-separated-exp-sequence" (PSeq [e, _, es])) = e : unwrap es
        unwrap (PNT "comma-separated-exp-sequence" e) = [e]
p2s (PNT "parenexp" (PSeq [_, exp, _])) = p2s exp
p2s (PNT "phash" (PSeq [(PT "p-lcb" _), (PT "p-rcb" _)])) = PHash []
p2s (PNT "phash" (PSeq [_, entries, _])) = PHash (map p2s (unwrap entries))
  where unwrap (PNT "phash-entries" (PSeq [entry, _, entries])) = entry : unwrap entries
        unwrap (PNT "phash-entries" entry) = [entry]
p2s (PNT "phash-entry" (PSeq [id, _, e]))= Entry (p2s id) (p2s e)
p2s (PNT "lambda-exp" (PSeq [_, args, body])) = Lambda (p2s args) (p2s body)
p2s (PT "identifier" id) = Id id
p2s (PT "constructor" id) = Ctor id
p2s (PT "integer" id) = SInt (read id :: Int)
p2s (PT "string" s) = Str s
p2s (PT "operator" op) = Op op
p2s (PT "unary-operator" op) = Op op
p2s x = Um x
