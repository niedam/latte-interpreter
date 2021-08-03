{-# LANGUAGE FlexibleContexts #-}

module Latte.Types where

import qualified Latte.Abs
import qualified Latte.Error as Error


import Control.Monad.Except


simplify :: Latte.Abs.Type' a -> Latte.Abs.Type' ()
simplify t = case t of
  Latte.Abs.Int _ -> Latte.Abs.Int ()
  Latte.Abs.Str _ -> Latte.Abs.Str ()
  Latte.Abs.Bool _ -> Latte.Abs.Bool ()
  Latte.Abs.Void _ -> Latte.Abs.Void ()
  Latte.Abs.Ref _ t1 -> Latte.Abs.Ref () $ simplify t1
  Latte.Abs.Fun _ t1 targ -> Latte.Abs.Fun () (simplify t1) (map simplify targ)

int :: Latte.Abs.Type' ()
int = Latte.Abs.Int ()

str :: Latte.Abs.Type' ()
str = Latte.Abs.Str ()

bool :: Latte.Abs.Type' ()
bool = Latte.Abs.Bool ()

void :: Latte.Abs.Type' ()
void = Latte.Abs.Void ()

ref :: Latte.Abs.Type' () -> Latte.Abs.Type' ()
ref = Latte.Abs.Ref ()

fun :: Latte.Abs.Type' () -> [Latte.Abs.Type' ()] -> Latte.Abs.Type' ()
fun = Latte.Abs.Fun ()

funSign :: Latte.Abs.Type' a -> [Latte.Abs.Arg' b] -> Latte.Abs.Type' ()
funSign resType args =
  fun (simplify resType) $ map (\(Latte.Abs.Arg _ t _) -> simplify t) args
