{-# LANGUAGE FlexibleContexts #-}

module Latte.Error where

import qualified Latte.Abs
import Latte.Shows
import Prelude (($), (.), Int, String, Show, shows, show, showString, Maybe(..))
import Control.Monad.Except
import System.Exit
import System.IO
import System.IO.Error
import Data.Maybe (fromJust)

type Position = Latte.Abs.BNFC'Position
type Ident = Latte.Abs.Ident
type Type a = Latte.Abs.Type' a

undefinedName :: MonadError String m => Position -> Ident -> m a
undefinedName loc ident = throwError $ message where
  message = showString "Name \"" . showsIdent ident .
    showString "\" in " . showsPos loc $ " is not defined"

notCallable :: MonadError String m => Position -> Ident -> m a
notCallable pos ident = throwError $ message where
  message = showString "Object \"" . showsIdent ident . showString "\" at " .
    showsPos pos $ "is not callable"

unexpectedType :: MonadError String m => Position -> Type b -> Type c -> m a
unexpectedType loc expected got = throwError $ message where
  message = showString "At " . showsPos loc . showString " expected " .
    showsType expected . showString " but got " . showsType got $ ""

wrongArguments :: MonadError String m => Position -> Ident -> Int -> Int -> m a
wrongArguments pos ident expected got = throwError $ message where
  message = showsIdent ident . showString "() takes " . shows expected .
    showString " arguments but " . shows got $ " were given"

duplicateDefs :: MonadError String m => Ident -> Position -> Position -> m a
duplicateDefs ident pos1 pos2 = throwError message where
  message = showString "Duplicate definition for \"" . showsIdent ident .
    showString "\" at (" . showsPos pos2 . showString ") and (" .
    showsPos pos1 $ ")"

mainType :: MonadError String m => m a
mainType = throwError message where
  message = "\"main\" must return int"

notLoopStmt :: MonadError String m => Position -> String -> m a
notLoopStmt pos stmt = throwError message where
  message = showString "\"" . showString stmt .
    showString "\" statement not in loop or switch statement at " .
    showsPos pos $ ""

noReturn :: MonadError String m =>  Position -> m a
noReturn pos = throwError $ message where
  message = showString "Non-void function does not return a value at " .
    showsPos pos $ ""

runtimeError :: MonadError String m =>  Position -> m a
runtimeError pos = throwError $ message
  where
    message = showString "Runtime error at " . showsPos pos $ ""

zeroDivisionError :: MonadError String m =>  Position -> m a
zeroDivisionError pos = throwError $ message where
  message = showString "Runtime error: At " . showsPos pos $
    " division by zero is undefined"

uninitializedIdent :: MonadError String m =>  Position -> Ident -> m a
uninitializedIdent pos ident = throwError $ message where
  message = showString "Runtime error: uninitialized name \"" .
    showsIdent ident . showString "\" at " . showsPos pos $ ""

parseIntegerErr :: MonadError String m => Position -> m a
parseIntegerErr pos = throwError $ message where
  message = showString "Runtime error: Data read from stdin at "
    . showsPos pos $ " is not number"

duplicatedArg :: MonadError String m => Position -> Ident -> m a
duplicatedArg pos ident = throwError $ message where
  message = showString "Redefinition of argument \"" . showsIdent ident .
    showString "\" at " . showsPos pos $ ""

noScript :: IO a
noScript = die $ message where
  message = "No path to script, run with --help for information"

parseError :: String -> IO a
parseError err = die $ message where
  message = showString "Parse failed: " $ err

openFileError :: IOError -> IO a
openFileError err = die $ message where
  message = showString "Error: File " $ ioeGetErrorString err
