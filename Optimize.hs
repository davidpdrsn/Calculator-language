module Optimize where

import System.Environment
import Prelude hiding (join)
import Text.ParserCombinators.Parsec (parse)
import Data.List
import qualified Parse
import Ast

optimize :: Program -> Program
optimize = map optimizeExpr

optimizeExpr :: Expr -> Expr
optimizeExpr = eliDeadCode . cpConstProp

cpConstProp :: Expr -> Expr
cpConstProp e = case e of
  Plus (Constant m) (Constant n) -> Constant (m + n)
  Plus e1 e2 -> Plus (optimizeExpr e1) (optimizeExpr e2)

  Minus (Constant m) (Constant n) -> Constant (m - n)
  Minus e1 e2 -> Minus (optimizeExpr e1) (optimizeExpr e2)

  Times (Constant m) (Constant n) -> Constant (m * n)
  Times e1 e2 -> Times (optimizeExpr e1) (optimizeExpr e2)

  Divide (Constant m) (Constant n) -> Constant (m `div` n)
  Divide e1 e2 -> Divide (optimizeExpr e1) (optimizeExpr e2)

  Assignment id e -> Assignment id (optimizeExpr e)
  x -> x

eliDeadCode :: Expr -> Expr
eliDeadCode e = case e of
  Plus e (Constant 0) -> optimizeExpr e
  Plus (Constant 0) e -> optimizeExpr e
  Plus e1 e2 -> Plus (optimizeExpr e1) (optimizeExpr e2)

  Minus e1 (Constant 0) -> e1
  Minus (Constant 0) e2 -> e2
  Minus e1 e2 -> Minus (optimizeExpr e1) (optimizeExpr e2)

  Times _ (Constant 0) -> Constant 0
  Times (Constant 0) _ -> Constant 0
  Times e (Constant 1) -> optimizeExpr e
  Times (Constant 1) e -> optimizeExpr e
  Times e1 e2 -> Times (optimizeExpr e1) (optimizeExpr e2)

  Divide e1 e2 -> Divide (optimizeExpr e1) (optimizeExpr e2)

  Assignment id e -> Assignment id (optimizeExpr e)
  x -> x
