module Lambda.Term (Term(..)) where

-- Λ = V | (Λ Λ) | (λV.Λ)
data Term = Var Char 
          | Lambda Char Term
          | App Term Term
          deriving (Show, Eq)
