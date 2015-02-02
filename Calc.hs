{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import System.Environment
import Prelude hiding (lookup)
import Text.ParserCombinators.Parsec (parseFromFile)
import Parse
-- import State
import Ast

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
                    ast <- parseFromFile program filename
                    case ast of
                      Right ast' -> case evalProgram ast' of
                        Right (ValueInteger i) -> putStrLn $ show i
                        Left e -> putStrLn e
                      Left e -> putStrLn $ show e
    _ -> putStrLn "Usage: calc <filename>"

evalProgram :: Program -> Either String Value
evalProgram p = case evalStatements [] p of
  Right (_, result) -> Right result
  Left err -> Left err

evalStatements :: SymTab -> Program -> Either String (SymTab, Value)
evalStatements _ [] = Left "Empty program"
evalStatements symtab [line] = case eval symtab line of
  Right (symtab', result) -> Right (symtab', result)
  Left err -> Left err
evalStatements symtab (l:ls) = case eval symtab l of
  Right (symtab', _) -> evalStatements symtab' ls
  Left err -> Left err

eval :: SymTab -> Expr -> Either String (SymTab, Value)
eval symtab expr =
  case expr of
    Constant n -> Right (symtab, n)

    Plus e1 e2 -> evalBinOp symtab e1 e2 (+)
    Minus e1 e2 -> evalBinOp symtab e1 e2 (-)
    Times e1 e2 -> evalBinOp symtab e1 e2 (*)
    Divide e1 e2 -> evalBinOp symtab e1 e2 div

    Assignment id e -> case id of
      Identifier id' -> case eval symtab e of
        Right (symtab', result) -> Right ((id', result):symtab', result)
        e -> e
      _ -> Left "Can't assign to non-identifier"

    Identifier id -> case lookup symtab id of
      Just value -> Right (symtab, value)
      Nothing -> Left (show id ++ " is not defined")

lookup :: SymTab -> Name -> Maybe Value
lookup [] _ = Nothing
lookup ((name, value):names) id = if name == id
                                 then Just value
                                 else lookup names id

evalBinOp :: SymTab -> Expr -> Expr -> (Int -> Int -> Int) -> Either String (SymTab, Value)
evalBinOp symtab e1 e2 f =
  case eval symtab e1 of
    Right (symtab', ValueInteger e1') -> case eval symtab e2 of
      Left e -> Left e
      Right (symtab'', ValueInteger e2') ->
        let result = ValueInteger $ f e1' e2'
        in Right (symtab' ++ symtab'', result)
    Left error -> Left error
