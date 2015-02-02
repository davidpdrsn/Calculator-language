{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import System.Environment
import Text.ParserCombinators.Parsec (parseFromFile)
import Parse
import State as S
import Control.Monad.Trans.State
import Ast

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
                    ast <- parseFromFile program filename
                    case ast of
                      Right ast' -> case evalProgram ast' of
                        ValueInteger i -> putStrLn $ show i
                      Left e -> putStrLn $ show e
    _ -> putStrLn "Usage: calc <filename>"

evalProgram :: Program -> Value
evalProgram p = evalState (evalStatements p) []

evalStatements :: Program -> State SymTab Value
evalStatements [] = error "Empty program"
evalStatements [line] = eval line
evalStatements  (l:ls) = do
  _ <- eval l
  evalStatements ls

eval :: Expr -> State SymTab Value
eval expr =
  case expr of
    Constant n -> return n

    Plus e1 e2 -> evalBinOp e1 e2 (+)
    Minus e1 e2 -> evalBinOp e1 e2 (-)
    Times e1 e2 -> evalBinOp e1 e2 (*)
    Divide e1 e2 -> evalBinOp e1 e2 div

    Assignment id e -> case id of
      Identifier id' -> do
        e' <- eval e
        st <- get
        put $ (id', e') : st
        return e'
      _ -> error "Can't assign to non-identifier"

    Identifier id -> do
      value <- S.lookup id
      case value of
        Just value' -> return value'
        Nothing -> error (show id ++ " is not defined")

evalBinOp :: Expr -> Expr -> (Int -> Int -> Int) -> State SymTab Value
evalBinOp e1 e2 f = do
  (ValueInteger e1') <- eval e1
  (ValueInteger e2') <- eval e2
  let sum = e1' `f` e2'
  return $ ValueInteger sum

