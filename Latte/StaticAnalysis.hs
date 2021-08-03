{-# LANGUAGE FlexibleContexts #-}

module Latte.StaticAnalysis where

import Latte.Abs as Abs
import Latte.Error as Error
import System.Exit

import Control.Monad.Except

type ReturnM a = Except String a
type WhileM a = Except String a



checkRetFunBlock :: Abs.Block -> ReturnM Bool
checkRetFunBlock block = checkRetBlock block True

checkRetInnerBlock :: Abs.Block -> ReturnM Bool
checkRetInnerBlock block = checkRetBlock block False

checkRetBlock :: Abs.Block -> Bool -> ReturnM Bool
checkRetBlock (Abs.Block pos block) requireReturn = checkRetStmts block False where
  checkRetStmts [] acc = if (requireReturn && not acc) then
    Error.noReturn pos
  else
    return acc
  checkRetStmts (hd : tl) acc = case hd of
    Abs.Empty _ -> checkRetStmts tl acc
    Abs.BStmt _ bl -> do
      resBl <- checkRetInnerBlock bl
      checkRetStmts tl (if resBl then True else acc)
    Abs.Decl _ _ itms -> do
      forM_ itms $ \item -> case item of
        Abs.NoInit _ _ -> return ()
        Abs.Init _ _ ex -> checkRetExpr ex
      checkRetStmts tl acc
    Abs.NestFun _ (Abs.FnDef _ t _ _ bl) -> do
      case t of
        Abs.Void _ -> checkRetInnerBlock bl
        _          -> checkRetFunBlock bl
      checkRetStmts tl acc
    Abs.Ass _ _ ex -> do
      checkRetExpr ex
      checkRetStmts tl acc
    Abs.Incr _ _ -> checkRetStmts tl acc
    Abs.Decr _ _ -> checkRetStmts tl acc
    Abs.Ret _ expr -> do
      checkRetExpr expr
      checkRetStmts tl True
    Abs.VRet _ -> checkRetStmts tl True
    Abs.Break _ -> checkRetStmts tl acc
    Abs.Continue _ -> checkRetStmts tl acc
    Abs.Cond _ _ stm -> do
      checkRetInnerBlock (Abs.Block Nothing [stm])
      checkRetStmts tl acc
    Abs.CondElse _ _ stm1 stm2 -> do
      resStm1 <- checkRetInnerBlock (Abs.Block Nothing [stm1])
      resStm2 <- checkRetInnerBlock (Abs.Block Nothing [stm2])
      checkRetStmts tl (if (resStm1 && resStm2) then True else acc)
    Abs.While _ _ stm -> do
      void $ checkRetInnerBlock (Abs.Block Nothing [stm])
      checkRetStmts tl acc
    Abs.SExp _ ex -> do
      checkRetExpr ex
      checkRetStmts tl acc
  checkRetExpr ex = case ex of
    Abs.ELambda _ t _ bl -> case t of
      Abs.Void _ -> void $ checkRetInnerBlock bl
      _          -> void $ checkRetFunBlock bl
    _                    -> return ()

checkReturnProgram :: Abs.Program -> ReturnM ()
checkReturnProgram (Abs.Program _ fndefs) = do
  forM_ fndefs $ \(Abs.FnDef _ t _ _ bl) ->
    case t of
      Abs.Void _ -> void $ checkRetInnerBlock bl
      _          -> void $ checkRetFunBlock bl

controlWhileProgram :: Abs.Program -> WhileM ()
controlWhileProgram (Abs.Program _ fndefs) = do
  forM_ fndefs $ \(Abs.FnDef _ _ _ _ bl) ->
    controlWhileFun bl

controlBreakContBl :: Abs.Block -> Bool -> WhileM ()
controlBreakContBl (Abs.Block _ stmts) inwhile = checkBrContStmts stmts where
  checkBrContStmts [] = return ()
  checkBrContStmts (hd : tl) = case hd of
    Abs.Empty _ -> checkBrContStmts tl
    Abs.BStmt _ bl -> do
      controlBreakContBl bl inwhile
      checkBrContStmts tl
    Abs.Decl _ t itms -> do
      forM_ itms $ \it -> do
        case it of
          Abs.NoInit _ _ -> return ()
          Abs.Init _ _ ex -> controlBreakContinueExpr ex
      checkBrContStmts tl
    Abs.NestFun _ (Abs.FnDef _ _ _ _ bl) -> do
      controlWhileFun bl
      checkBrContStmts tl
    Abs.Ass _ _ ex -> do
      checkBrContStmts tl
    Abs.Incr _ _ -> checkBrContStmts tl
    Abs.Decr _ _ -> checkBrContStmts tl
    Abs.Ret _ ex -> do
      controlBreakContinueExpr ex
      checkBrContStmts tl
    Abs.VRet _ -> checkBrContStmts tl
    Abs.Break pos -> do
      when (not inwhile) $ Error.notLoopStmt pos "break"
      checkBrContStmts tl
    Abs.Continue pos -> do
      when (not inwhile) $ Error.notLoopStmt pos "continue"
      checkBrContStmts tl
    Abs.Cond _ ex stm1 -> do
      checkBrContStmts tl
    Abs.CondElse _ _  stm1 stm2 -> do
      controlBreakContBl (Abs.Block Nothing [stm1]) inwhile
      controlBreakContBl (Abs.Block Nothing [stm2]) inwhile
      checkBrContStmts tl
    Abs.While _ _ stmt -> do
      controlBreakContinueInner (Abs.Block Nothing [stmt])
      checkBrContStmts tl
    Abs.SExp _ ex -> do
      controlBreakContinueExpr ex
      checkBrContStmts tl

controlWhileFun :: Abs.Block -> WhileM ()
controlWhileFun block = controlBreakContBl block False

controlBreakContinueInner :: Abs.Block -> WhileM ()
controlBreakContinueInner block = controlBreakContBl block True

controlBreakContinueExpr :: Abs.Expr -> WhileM ()
controlBreakContinueExpr ex = case ex of
  Abs.ELambda _ _ _ bl -> controlWhileFun bl
  _                    -> return ()

staticAnalysis :: Abs.Program -> IO ()
staticAnalysis program = do
  ret <- case (runExcept (checkReturnProgram program)) of
    Left err -> die err
    _        -> return ()
  whl <- case (runExcept (controlWhileProgram program)) of
    Left err -> die err
    _        -> return ()
  return ()
