module Lambda.Evaluator where

import qualified Data.Set as S

import Lambda.Term (Term(..))

(∈) :: Ord a => a -> S.Set a -> Bool
(∈) = S.member

(∉):: Ord a => a -> S.Set a -> Bool
(∉) = (not . ) . (∈)

(∅) :: Monoid a => a
(∅) = mempty

(∪) :: Ord a => S.Set a -> S.Set a -> S.Set a
(∪) = S.union

(∩) :: Ord a => S.Set a -> S.Set a -> S.Set a
(∩) = S.intersection

fv' :: S.Set Char -> Term -> S.Set Char
fv' s (Var c) = if c ∉ s then S.fromList [c] else (∅)
fv' s (App x y) = fv' s x ∪ fv' s y
fv' s (Lambda x y) = fv' (S.insert x s) y

fv :: Term -> S.Set Char
fv = fv' (∅)

newVar :: S.Set Char -> Char
newVar s = head [x | x <- reverse ['a'..'z'], x ∉ s]

-- | substitute @Λ@ for free @Vx@ in another @Λ@.
σ :: Term -> Char -> Term -> Term
σ n x y@(Var x') 
  | x == x'   = n   -- x[x := N] ≡ N
  | otherwise = y   -- y[x := N] ≡ y

σ n x (App p q) =   -- PQ[x := N] ≡ (P[x := N])(Q[x := N])
  App 
    (σ n x p)       -- P[x := N]
    (σ n x q)       -- Q[x := N]

σ n x (Lambda y p)
  | x ∉ fvP   = -- (λy.P)[x := N] ≡ λy.P              where x ∉ FV(P)
      Lambda y p
  | y ∉ fvN   = -- (λy.P)[x := N] ≡ λy.(P[x := N])    where y ∉ FV(N)
      Lambda y (σ n x p)
  | otherwise = -- (λy.P)[x := N] ≡ λz.(Pʸ→ᶻ[x := N]) where z ∉ FV(N) && z ∉ FV(P)
      Lambda z (σ n x (αCon y z p))
  where
    fvN = fv n
    fvP = fv p
    z = newVar (fvN ∪ fvP)

-- | Rename @Vx@ to @Vy@ in @Λ@.
-- α-conversion is a special case of substitution.
αCon :: Char -> Char -> Term -> Term
αCon x y = σ (Var y) x

isRedex :: Term -> Bool
isRedex (Var _) = False
isRedex (App (Lambda _ _) _) = True
isRedex (App x y) = isRedex x || isRedex y
isRedex (Lambda _ y) = isRedex y

-- | Zero or one step β-reduction.
β :: Term -> Term
β (Var x) = Var x
β (App (Lambda x y) n) = σ n x y
β (App x y) 
  | isRedex x = App (β x) y
  | isRedex y = App x (β y)
  | otherwise = App x y 
β (Lambda x y) = Lambda x (β y)

-- | Zero or more step β-reduction.
-- Reduces to β-normal-form (βnf).
βnf :: Term -> Term
βnf t
  | isRedex t = βnf (β t)
  | otherwise = t

-- | α-equivalence.
αEquiv :: Term -> Term -> Bool
αEquiv (Var x) (Var y) = x == y
αEquiv (App w x) (App y z) = αEquiv w y && αEquiv x z
αEquiv m@(Lambda w _) (Lambda y z) = Lambda w (αCon y w z) == m
αEquiv _ _ = False

-- | η-equivalance (extensional equality).
-- Evaluate both terms to β-normal-form and compare
-- for α-equivalence.
(≡) :: Term -> Term -> Bool
(≡) m n = αEquiv (βnf m) (βnf n)
