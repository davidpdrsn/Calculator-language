module Optimize where

import System.Environment
import Prelude hiding (join)
import Text.ParserCombinators.Parsec (parse)
import Data.List
import qualified Parse
import Ast

optimize :: Expr -> Expr
optimize = eliDeadCode . cpConstProp

cpConstProp :: Expr -> Expr
cpConstProp e = case e of
  Plus (Constant m) (Constant n) -> Constant (m + n)
  Plus e1 e2 -> Plus (optimize e1) (optimize e2)

  Minus (Constant m) (Constant n) -> Constant (m - n)
  Minus e1 e2 -> Minus (optimize e1) (optimize e2)

  Times (Constant m) (Constant n) -> Constant (m * n)
  Times e1 e2 -> Times (optimize e1) (optimize e2)

  Divide (Constant m) (Constant n) -> Constant (m `div` n)
  Divide e1 e2 -> Divide (optimize e1) (optimize e2)

  Assignment id e -> Assignment id (optimize e)
  x -> x

eliDeadCode :: Expr -> Expr
eliDeadCode e = case e of
  Plus e (Constant 0) -> optimize e
  Plus (Constant 0) e -> optimize e
  Plus e1 e2 -> Plus (optimize e1) (optimize e2)

  Minus e1 (Constant 0) -> e1
  Minus (Constant 0) e2 -> e2
  Minus e1 e2 -> Minus (optimize e1) (optimize e2)

  Times _ (Constant 0) -> Constant 0
  Times (Constant 0) _ -> Constant 0
  Times e (Constant 1) -> optimize e
  Times (Constant 1) e -> optimize e
  Times e1 e2 -> Times (optimize e1) (optimize e2)

  Divide e1 e2 -> Divide (optimize e1) (optimize e2)

  Assignment id e -> Assignment id (optimize e)
  x -> x
