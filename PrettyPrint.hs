module PrettyPrint where

import System.Environment
import Prelude hiding (join)
import Text.ParserCombinators.Parsec (parse)
import Data.List
import qualified Parse
import Ast

prettyPrint :: String -> String
prettyPrint p = case parse Parse.program "" p of
  Right lines -> intercalate ".\n" (map prettyPrintExpr lines)
  Left error -> show error

prettyPrintExpr :: Expr -> String
prettyPrintExpr e = case e of
  Constant i -> show i
  Plus e1 e2 -> (prettyPrintExpr e1) ++ " + " ++ (prettyPrintExpr e2)
  Minus e1 e2 -> (prettyPrintExpr e1) ++ " - " ++ (prettyPrintExpr e2)
  Times e1 e2 -> (prettyPrintExpr e1) ++ " * " ++ (prettyPrintExpr e2)
  Divide e1 e2 -> (prettyPrintExpr e1) ++ " / " ++ (prettyPrintExpr e2)
  Assignment id e -> (prettyPrintExpr id) ++ " = " ++ (prettyPrintExpr e)
  Identifier id -> id

