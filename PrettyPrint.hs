module PrettyPrint where

import Text.ParserCombinators.Parsec (parse)
import Prelude hiding (error, lines)
import Data.List hiding (lines)
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
  Assignment identifier val -> (prettyPrintExpr identifier) ++ " = " ++ (prettyPrintExpr val)
  Identifier identifier -> identifier

