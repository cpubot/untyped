module Repl (repl) where

import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.Haskeline
    (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)

import Lambda.PrettyPrinter (termToString)
import Lambda.Evaluator (fv, isRedex, β, (∪), αEquiv) 
import Lambda.Term (Term)
import Repl.Parser (runParse, ReplTerm(..))
import Repl.LogUtil (stylePrompt, styleError, res, info)
import Repl.Evaluator (Γ, getFvsInContext, σΓ, σΓ1, βnfΓ, updateΓ)

bindingLog :: Char -> Term -> String
bindingLog x t = [x] <> " := " <> termToString t

deductionStep :: String -> String
deductionStep s = s <> "\n" <> replicate (length s) '-'

deductionContextHeader :: Γ -> Term -> String
deductionContextHeader c t =
  let 
    tSub = σΓ c t 
    replacements = S.toList
      (getFvsInContext c t ∪ getFvsInContext c (σΓ1 c t))
    fvs = fv tSub
  in 
    "Γ, " 
    <> (if null fvs
        then ""
        else intercalate ", " (map (:[]) (S.toList fvs)) <> ", ")
    <> (if null replacements
        then ""
        else intercalate ", " (map (\x -> bindingLog x (fromJust (M.lookup x c))) replacements) 
          <> ", ")
    <> termToString t

evalReplTerm :: Γ -> ReplTerm -> InputT IO ()
evalReplTerm c (Lookup t) = res (termToString (σΓ c t))
evalReplTerm c (Fv t) =
  res ("{" <> intercalate "," (map (:[]) (S.toList (fv (σΓ c t)))) <> "}")
evalReplTerm c ShowContext = mapM_ (info . uncurry bindingLog) (M.toList c)
evalReplTerm c (Binding x _) = info (bindingLog x (fromJust (M.lookup x c)))
evalReplTerm c (Eval t) = res (termToString (βnfΓ c t))
evalReplTerm c (Equiv x y) = do
  let
    βnf1 = βnfΓ c x 
    βnf2 = βnfΓ c y
  (if βnf1 `αEquiv` βnf2
  then res "True"
  else res "False")
  info "  via α?.β"
  info ("    " <> termToString βnf1)
  info ("    " <> termToString βnf2)
evalReplTerm c (Trace t) = do
  let 
    tSub = σΓ c t
    go term
      | isRedex term = do
          let t' = β term
          if isRedex t' then
            info (deductionStep (termToString t')) >> go t'
          else
            void (res (termToString t'))
      | otherwise = void (res (termToString term))
  info (deductionStep (deductionContextHeader c t))
  info (deductionStep (termToString tSub))
  go tSub

repl :: IO ()
repl = runInputT defaultSettings (loop mempty) where
  loop :: Γ -> InputT IO ()
  loop c = do
    line <- getInputLine (stylePrompt "λ> ")
    case line of
      Nothing -> return ()
      Just input -> do
        nextCtx <- case runParse input of
          Left e -> outputStrLn (styleError (show e)) >> return c
          Right term -> do
            let c' = updateΓ c term
            evalReplTerm c' term
            return c'
        loop nextCtx
