module Latte.Shows where

import qualified Latte.Abs

{- Showing positions, idents, types etc. -}

showsType :: Latte.Abs.Type' a -> ShowS
showsType t = case t of
  Latte.Abs.Int _       -> showString "int"
  Latte.Abs.Bool _      -> showString "bool"
  Latte.Abs.Str  _      -> showString "string"
  Latte.Abs.Void _      -> showString "void"
  Latte.Abs.Ref _ typ   -> showString "&" . showsType typ
  Latte.Abs.Fun _ typ a -> case a of
    []   -> showsType typ . showString "()"
    h:tl  -> showsType typ . showString "(" . showsType h .
      foldl (\p t -> p . showString "," . showsType t) (showString "") tl .
      showString ")"

showsPos :: Latte.Abs.BNFC'Position -> ShowS
showsPos p = case p of
  Just (line, col) ->
    showString "line " . shows line . showString ", column " . shows col
  Nothing          -> id

showsIdent :: Latte.Abs.Ident -> ShowS
showsIdent (Latte.Abs.Ident name) = showString name
