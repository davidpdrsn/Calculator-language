module Ast where

type Program = [Expr]
type Name = String
type SymTab = [Binding]
type Binding = (Name, Int)

data Expr = Constant Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Divide Expr Expr
          | Assignment Expr Expr
          | Identifier Name
          deriving (Show)
