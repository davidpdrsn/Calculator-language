module Main where

import System.Environment
import Prelude hiding (lookup)
import Text.ParserCombinators.Parsec (parseFromFile)
import Parse
import Ast
import Optimize

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
                    ast <- parseFromFile program filename
                    case ast of
                      Right ast' -> case evalProgram ast' of
                        Right result -> putStrLn $ show result
                        Left e -> putStrLn e
                      Left e -> putStrLn $ show e
    _ -> putStrLn "Usage: calc <filename>"

evalProgram :: Program -> Either String Int
evalProgram p = case evalStatements [] (optimize p) of
  Right (_, result) -> Right result
  Left error -> Left error

evalStatements :: SymTab -> Program -> Either String (SymTab, Int)
evalStatements symtab [] = Left "Empty program"
evalStatements symtab [line] = case eval symtab line of
  Right (symtab', result) -> Right (symtab', result)
  error -> error
evalStatements symtab (line:lines) = case eval symtab line of
  Right (symtab', _) -> evalStatements symtab' lines
  error -> error

eval :: SymTab -> Expr -> Either String (SymTab, Int)
eval symtab expr =
  case expr of
    Constant n -> Right (symtab, n)

    Plus e1 e2 -> evalBinOp symtab e1 e2 Plus (+)
    Minus e1 e2 -> evalBinOp symtab e1 e2 Minus (-)
    Times e1 e2 -> evalBinOp symtab e1 e2 Times (*)
    Divide e1 e2 -> evalBinOp symtab e1 e2 Divide div

    Assignment id e -> case id of
      Identifier id' -> case eval symtab e of
        Right (symtab', result) -> Right ((id', result):symtab', result)
        e -> e
      _ -> Left "Can't assign to non-identifier"

    Identifier id -> case lookup symtab id of
      Just value -> Right (symtab, value)
      Nothing -> Left (show id ++ " is not defined")

lookup :: SymTab -> Name -> Maybe Int
lookup [] _ = Nothing
lookup ((name, value):names) id = if name == id
                                 then Just value
                                 else lookup names id

evalBinOp :: SymTab -> Expr -> Expr -> (Expr -> Expr -> Expr) -> (Int -> Int -> Int) -> Either String (SymTab, Int)
evalBinOp symtab e1 e2 binOp f =
  case eval symtab e1 of
    Right (symtab', e1') -> case eval symtab e2 of
      Left e -> Left e
      Right (symtab'', e2') ->
        let result = f e1' e2'
        in Right (symtab' ++ symtab'', result)
    error -> error
