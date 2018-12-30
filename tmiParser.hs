module TmiParser
( tmiParse
, tmiSplitParse
) where

import Data.Either
import qualified Data.List.Split as Split

import Parser
import Preprocess
import Semantic
import Tokenize
import Util

tmiGrammar = Grammar [
  Rule "Top" $ NT "plet",
  Rule "plet" $ Seq [T "let_keyword", T "p-lcb", NT "decls", T "p-rcb", T "in_keyword", NT "exp"],
  Rule "pwhere-suffix" $ Seq [T "where_keyword", T "p-lcb", NT "decls", T "p-rcb"],
  Rule "pdo" $ Alt [Seq [T "do_keyword", T "p-lcb", NT "exp", T "p-rcb"],
                    Seq [T "do_keyword", T "p-lcb", NT "do_assignments", T "semicolon", NT "exp", T "p-rcb"]],
  Rule "do_assignments" $ Alt [Seq [NT "do_assignment", T "semicolon", NT "do_assignments"], NT "do_assignment"],
  Rule "do_assignment" $ Seq [NT "exp", T "larrow", NT "exp"],
  Rule "definition" $ Seq [NT "exp", T "equals", NT "exp"],
  Rule "decls" $ Alt [Seq [NT "definition", T "semicolon", NT "decls"], NT "definition"],
  Rule "parenexp" $ Seq [T "lparen", NT "exp", T "rparen"],
  Rule "listexp" $ Alt [Seq [T "lsb", T "rsb"], Seq [T "lsb", NT "comma-separated-exp-sequence", T "rsb"]],
  Rule "comma-separated-exp-sequence" $ Alt [Seq [NT "exp", T "comma", NT "comma-separated-exp-sequence"], NT "exp"],
  Rule "lambda-exp" $ Seq [T "lambda", NT "parenexp", NT "exp"],
  Rule "base-exp" $ Alt [T "constructor", T "identifier", T "integer", T "operator", T "unary-operator",
                         NT "parenexp", NT "listexp", NT "lambda-exp", T "string", NT "phash"],
  Rule "base-exp-seq" $ Alt [Seq [NT "base-exp", NT "base-exp-seq"], NT "base-exp"],
  Rule "exp" $ Alt [NT "where-exp", NT "non-where-exp"],
  Rule "where-exp" $ Seq [NT "non-where-exp", NT "pwhere-suffices"],
  Rule "pwhere-suffices" $ Alt [Seq [NT "pwhere-suffix", NT "pwhere-suffices"], NT "pwhere-suffix"],
  Rule "non-where-exp" $ Alt [NT "pif", NT "plet", NT "pdo", NT "case", NT "base-exp-seq"],
  Rule "case" $ Seq [T "case_keyword", NT "exp", T "of_keyword", T "p-lcb", NT "case_clauses", T "p-rcb"],
  Rule "case_clauses" $ Alt [Seq [NT "case_clause", T "semicolon", NT "case_clauses"], NT "case_clause"],
  Rule "case_clause" $ Seq [NT "exp", T "rdbl_arrow", NT "exp"],
  Rule "pif" $ Seq [T "if_keyword", NT "exp", T "then_keyword", NT "exp", T "else_keyword", NT "exp"],
  Rule "phash" $ Alt [Seq [T "p-lcb", T "p-rcb"], Seq [T "p-lcb", NT "phash-entries", T "p-rcb"]],
  Rule "phash-entries" $ Alt [Seq [NT "phash-entry", T "comma", NT "phash-entries"], NT "phash-entry"],
  Rule "phash-entry" $ Seq [T "identifier", T "colon", NT "exp"]
  ]

tmiParse :: [PosToken] -> Either Sem [[PosToken]]
tmiParse tokens = case parse tmiGrammar "Top" tokens of
                    Just feh -> Left $ p2s feh
                    Nothing -> Right [tokens]

tmiSplitParse :: [PosToken] -> Either Sem [[PosToken]]
tmiSplitParse tokens =
    case partitionEithers $ map tmiParse $ map preprocess $ splitTLD tokens of
      (oks, []) -> Left $ reLet oks
      (_, [bad]) -> Right bad
  where splitTLD xs = removeEmptyFirst (Split.split tld xs)
        tld = Split.keepDelimsL $ Split.whenElt isCol0
        isCol0 (PosToken _ _ (0, _)) = True
        isCol0 _ = False
        removeEmptyFirst ([] : xs) = xs
        -- Commented this out because the first token should always be at column 0
        --removeEmptyFirst x = x
        reLet sems = addLIM (map removeLIM sems)
          where removeLIM (Let (Decls [decl]) (App [(Id "main")])) = decl
                addLIM decls = Let (Decls decls) (App [(Id "main")])
