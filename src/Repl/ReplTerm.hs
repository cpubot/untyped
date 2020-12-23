module Repl.ReplTerm (ReplTerm(..)) where

import Lambda.Term (Term)

data ReplTerm = Binding Char Term
              | Lookup Term
              | Eval Term
              | Equiv Term Term
              | ShowContext
              | Trace Term
              | Fv Term
              deriving (Show)
