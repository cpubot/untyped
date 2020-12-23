module Repl.Parser (runParse, Term(..), ReplTerm(..)) where

import Text.ParserCombinators.Parsec
    (char, oneOf, spaces, string, (<|>), parse, ParseError, Parser)

import Lambda.Term (Term(..))
import Lambda.Parser (p)
import Repl.ReplTerm (ReplTerm(..))

parseEval :: Parser ReplTerm
parseEval = char '!' >> spaces >> Eval <$> p

parseFv :: Parser ReplTerm
parseFv = string "fv" >> spaces >> Fv <$> p

parseLookup :: Parser ReplTerm
parseLookup = Lookup <$> p

parseBinding :: Parser ReplTerm
parseBinding = do
  string "let" >> spaces
  binding <- oneOf ['a'..'z']
  spaces >> string "=" >> spaces
  Binding binding <$> p

parseShowContext :: Parser ReplTerm
parseShowContext = ShowContext <$ (char '?' >> spaces)

parseTrace :: Parser ReplTerm
parseTrace = do
  string "trace" >> spaces
  Trace <$> p

parseEquiv :: Parser ReplTerm
parseEquiv = string "=" >> spaces >> Equiv <$> p <*> (spaces >> p)

parseReplTerm :: Parser ReplTerm
parseReplTerm = spaces >> (parseShowContext
             <|> parseTrace
             <|> parseFv
             <|> parseEquiv
             <|> parseBinding
             <|> parseEval
             <|> parseLookup)

runParse :: String -> Either ParseError ReplTerm
runParse = parse parseReplTerm "Repl"
