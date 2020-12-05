module PrettyPrinter  where

import Term (Term(..))

termToString :: Term -> String
termToString (Var x) = [x]
termToString (App x@(Var _) y@(App _ _)) = termToString x <> "(" <> termToString y <> ")"
termToString (App x@(Lambda _ _) y@(Lambda _ _)) = "(" <> termToString x <> ")" <> "(" <> termToString y <> ")"
termToString (App x@(Lambda _ _) y) = "(" <> termToString x <> ")" <> termToString y
termToString (App x y@(Lambda _ _)) = termToString x <> "(" <> termToString y <> ")"
termToString (App x y) = termToString x <> termToString y
termToString (Lambda head body) = "λ" <> [head] <> "." <> termToString body

pp :: Term -> IO ()
pp = putStrLn . termToString

