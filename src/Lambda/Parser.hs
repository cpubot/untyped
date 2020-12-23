module Lambda.Parser (runParse, unsafeRunParse, p)  where

import Text.ParserCombinators.Parsec

import Lambda.Term (Term(..))

asciiAlpha :: Parser Char
asciiAlpha = oneOf ['a'..'z']

parseVar :: Parser Term
parseVar = Var <$> asciiAlpha

handleCurry :: [Char] -> Term -> Term
handleCurry [x] body = Lambda x body
handleCurry (x:xs) body = foldl (flip Lambda) (Lambda x body) xs

parseLambda :: Parser Term
parseLambda = do
  char 'λ'
  bindings <- many1 asciiAlpha
  char '.'
  handleCurry (reverse bindings) <$> parseTerms

parseApp :: Parser Term
parseApp = App <$> simple <*> simple

simple :: Parser Term
simple = try parseVar <|> try parens

parens :: Parser Term 
parens = char '(' *> parseTerms <* char ')'

parseTerm :: Parser Term
parseTerm = try parseLambda <|> try parseApp <|> simple

termsToApps :: [Term] -> Term
termsToApps [x] = x
termsToApps (x:xs) = foldl App x xs

parseTerms :: Parser Term
parseTerms = termsToApps <$> many1 parseTerm

p :: Parser Term
p = termsToApps <$> (try (many1 simple) <|> many1 parseTerm)

runParse :: String -> Either ParseError Term
runParse = parse p "Lambda-Calc"

unsafeRunParse :: String -> Term
unsafeRunParse s = case parse p "Lambda-Calc" s of
  (Left e) -> error (show e)
  (Right x) -> x