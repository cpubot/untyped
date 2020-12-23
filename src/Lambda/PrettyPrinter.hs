module Lambda.PrettyPrinter (termToString, pp) where

import Lambda.Term (Term(..))

termToString :: Term -> String
termToString (Var x) = [x]
termToString (App x@(Lambda _ _) y@(Lambda _ _)) = "(" <> termToString x <> ")" <> "(" <> termToString y <> ")"
termToString (App x@(Lambda _ _) y) = "(" <> termToString x <> ")" <> termToString y
termToString (App x y@(Var _)) = termToString x <> termToString y
termToString (App x y) = termToString x <> "(" <> termToString y <> ")"
termToString (Lambda h body) = "λ" <> [h] <> "." <> termToString body

pp :: Term -> IO ()
pp = putStrLn . termToString

