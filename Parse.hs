module Parse where

import Control.Applicative hiding (many, (<|>), optional)
import Text.ParserCombinators.Parsec
import Ast

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

symbol :: String -> Parser String
symbol = lexeme . string

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

program :: Parser [Expr]
program = many statement

statement :: Parser Expr
statement = expr <* symbol "."

expr :: Parser Expr
expr = binOpChain
  where
    realThing = (parens expr) <|> numbers <|> identifier

    numbers = do
      ns <- lexeme $ many1 digit
      return $ Constant (read ns)

    identifier = do
      name <- lexeme $ many letter
      return $ Identifier name

    binOpChain = assignment

    assignment = chainr1 plusminus binOp
      where binOp = symbol "=" >> return Assignment

    plusminus = chainl1 timesdivide binOp
      where binOp = (symbol "+" >> return Plus) <|>
                    (symbol "-" >> return Minus)

    timesdivide = chainl1 realThing binOp
      where binOp = (symbol "*" >> return Times) <|>
                    (symbol "/" >> return Divide)
