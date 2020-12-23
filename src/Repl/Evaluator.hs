module Repl.Evaluator where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

import Lambda.Term (Term)
import Lambda.Evaluator (fv, (∩), σ, βnf)
import Repl.ReplTerm (ReplTerm(..))

type Γ = M.Map Char Term

-- | Given a context @Γ@ and a term @T@, find all @FV(T)@ which exist in @Γ@.
getFvsInContext :: Γ -> Term -> S.Set Char
getFvsInContext c t = fv t ∩ S.fromList (M.keys c)

-- | Given a context @Γ@ and a term @T@, subtitute all @FV(T) ∩ Γ@
-- for corresponding variables in @Γ@.
σΓ1 :: Γ -> Term -> Term
σΓ1 c t =
  let
    σ' y [] = y
    σ' y (x:xs) = σ' (σ (fromJust (M.lookup x c)) x y) xs
  in
    σ' t (S.toList (getFvsInContext c t))

-- | Recursive version @σΓ1@. Given a context @Γ@ and a term @T@, 
-- recursively subtitute all @FV(T) ∩ Γ@ for corresponding variables in @Γ@
-- until @FV(T) ∉ Γ@.
σΓ :: Γ -> Term -> Term
σΓ c t =
  let t' = σΓ1 c t
  in
    if null (getFvsInContext c t')
    then t'
    else σΓ c t'

-- | Given a context @Γ@ and a term @T@, subtitute all @FV(T) ∩ Γ@
-- for corresponding variables in @Γ@, then evaluate to β-normal-form.
βnfΓ :: Γ -> Term -> Term
βnfΓ c t = βnf (σΓ c t)

-- | Update given context @Γ@ with given variable binding.
updateΓ :: Γ -> ReplTerm -> Γ
updateΓ c (Binding x t) = M.insert x t c
updateΓ c _ = c
