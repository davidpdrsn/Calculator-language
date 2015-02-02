module Ast where

type Program = [Expr]
type Name = String
type SymTab = [(String, Value)]

data Expr = Constant Value
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Divide Expr Expr
          | Assignment Expr Expr
          | Identifier Name
          deriving (Show)

-- TODO: Would be neat to make this a functor, but the "kind" is wrong
data Value = ValueInteger Int
           deriving (Show)
