{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module State where

import Prelude
import Control.Monad.Trans.State
import Ast

bind :: String -> Value -> State SymTab ()
bind key value = state $ \tab -> doBind key value tab
  where
    doBind :: String -> Value -> SymTab -> ((), SymTab)
    doBind key' value' tab = ((), (key', value') : tab)

lookup :: String -> State SymTab (Maybe Value)
lookup id = state $ \fullTable -> doLookup id fullTable fullTable
  where
    doLookup :: String -> SymTab -> SymTab -> (Maybe Value, SymTab)
    doLookup _ [] fullTable = (Nothing, fullTable)
    doLookup needle ((id', value) : xs) fullTable =
      if needle == id' then (Just value, fullTable)
      else doLookup id xs fullTable
