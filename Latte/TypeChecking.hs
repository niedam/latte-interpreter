{-# LANGUAGE FlexibleContexts #-}

module Latte.TypeChecking where

import qualified Latte.Abs as Abs
import qualified Latte.Types as Types
import qualified Latte.Error as Error

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import System.Exit

type Type = Abs.Type' ()
type Position = Abs.BNFC'Position
type Ident = Abs.Ident

type Env = Map.Map Abs.Ident Type
type TypeCheckingM a = ExceptT String (Reader Env) a

type UniqueM a = ExceptT String (State (Map.Map Abs.Ident Position)) a

{-insertArgs :: [Abs.Arg] -> Env -> Env
insertArgs args = case args of
  [] -> id
  (Abs.Arg _ (Abs.Ref _ typ) ident) : tl ->
    Map.insert ident (Types.simplify typ) . insertArgs tl
  (Abs.Arg _ (typ) ident) : tl ->
      Map.insert ident (Types.simplify typ) . insertArgs tl
-}

insertArgs :: [Abs.Arg] -> TypeCheckingM (Env -> Env)
insertArgs args = insertArgs' args (Map.empty) where
  insertArgs' []       env = return $ Map.union env
  insertArgs' ((Abs.Arg pos typ ident) : tl) env = do
    when (Map.member ident env) $ Error.duplicatedArg pos ident
    let typ' = Types.simplify typ
    case typ' of
      Abs.Ref _ refT -> insertArgs' tl (Map.insert ident refT env)
      _              -> insertArgs' tl (Map.insert ident typ' env)


insertItems :: Type -> (Env -> Env) -> Abs.Item -> TypeCheckingM (Env -> Env)
insertItems typ acc items = case items of
  Abs.NoInit _ ident -> return $ Map.insert ident typ . acc
  Abs.Init _ ident expr -> do
    exprT <- checkTypExpr expr
    when (exprT /= typ) $ do
      let posExpr = Abs.hasPosition expr
      Error.unexpectedType posExpr typ exprT
    return $ Map.insert ident typ . acc

checkTypOper :: Type -> [Abs.Expr] -> TypeCheckingM Type
checkTypOper t exprs = do
  forM_ exprs $ \ex -> do
    exTyp <- checkTypExpr ex
    when (exTyp /= t) $ do
      let exPos = Abs.hasPosition ex
      Error.unexpectedType exPos t exTyp
  return t

checkTypIdent :: Position -> Ident -> TypeCheckingM Type
checkTypIdent pos ident = do
  env <- ask
  case (Map.lookup ident env) of
    Just identTyp -> return identTyp
    Nothing       -> Error.undefinedName pos ident

checkTypExpr :: Abs.Expr -> TypeCheckingM Type
checkTypExpr ex = case ex of
  Abs.EVar pos ident -> do
    checkTypIdent pos ident
  Abs.ERef pos ident -> do
    identTyp <- checkTypIdent pos ident
    return $ Types.ref identTyp
  Abs.ELitInt _ _ ->
    return Types.int
  Abs.ELitTrue _ ->
    return Types.bool
  Abs.ELitFalse _ ->
    return Types.bool
  Abs.EApp pos ident exprs -> do
    funTyp <- checkTypIdent pos ident
    (retTyp, argsTyp) <- case funTyp of
      Abs.Fun _ retT argsT -> return (retT, argsT)
      _                    -> Error.notCallable pos ident
    let lenExprs = length exprs
    let lenArgs = length argsTyp
    when (lenExprs /= lenArgs) $ Error.wrongArguments pos ident lenArgs lenExprs
    forM_ (zip argsTyp exprs) $ \(argT, expr) -> do
      exprT <- checkTypExpr expr
      let exprPos = Abs.hasPosition expr
      when (exprT /= argT) $ Error.unexpectedType exprPos argT exprT
    return retTyp
  Abs.EString _ _ ->
    return Types.str
  Abs.ELambda _ typ args block -> do
    let simpleTyp = Types.simplify typ
    insertedArgs <- insertArgs args
    local (insertedArgs) $ do
      checkTypBlock block simpleTyp
    return $ Types.funSign simpleTyp args
  Abs.Neg _ expr ->
    checkTypOper (Types.int) [expr]
  Abs.Not _ expr ->
    checkTypOper (Types.bool) [expr]
  Abs.EMul _ expr1 _ expr2 ->
    checkTypOper (Types.int) [expr1, expr2]
  Abs.EAdd _ expr1 _ expr2 ->
      checkTypOper (Types.int) [expr1, expr2]
  Abs.ERel _ expr1 _ expr2 -> do
    checkTypOper (Types.int) [expr1, expr2]
    return Types.bool
  Abs.EAnd _ expr1 expr2 ->
    checkTypOper (Types.bool) [expr1, expr2]
  Abs.EOr _ expr1 expr2 ->
    checkTypOper (Types.bool) [expr1, expr2]

checkTypFunDef :: Abs.FunDef -> TypeCheckingM Type
checkTypFunDef (Abs.FnDef _ typ ident args block) = do
  let signature = Types.funSign typ args
  insertedArgs <- insertArgs args
  local (insertedArgs . Map.insert ident signature) $ do
    checkTypBlock block (Types.simplify typ)
  return signature

checkTypBlock :: Abs.Block -> Type -> TypeCheckingM ()
checkTypBlock (Abs.Block _ lStmts) typ =
  checkTypStmts lStmts
  where
    checkTypStmts [] = return ()
    checkTypStmts (hd : tl) = case hd of
      Abs.Empty _ ->
        checkTypStmts tl
      Abs.BStmt _ innerBlock -> do
        checkTypBlock innerBlock typ
        checkTypStmts tl
      Abs.Decl _ decTyp items -> do
        inserter <- foldM (insertItems (Types.simplify decTyp)) id items
        local inserter $ do
          checkTypStmts tl
      Abs.NestFun _ funDef@(Abs.FnDef _ _ ident _ _) -> do
        funSig <- checkTypFunDef funDef
        local (Map.insert ident funSig) $ do
          checkTypStmts tl
      Abs.Ass pos ident expr -> do
        idenTyp <- checkTypIdent pos ident
        exprTyp <- checkTypExpr expr
        when (idenTyp /= exprTyp) $ do
          let exprPos = Abs.hasPosition expr
          Error.unexpectedType exprPos idenTyp exprTyp
        checkTypStmts tl
      Abs.Incr pos ident -> do
        idenTyp <- checkTypIdent pos ident
        when (idenTyp /= Types.int) $ do
          Error.unexpectedType pos Types.int idenTyp
        checkTypStmts tl
      Abs.Decr pos ident -> do
        idenTyp <- checkTypIdent pos ident
        when (idenTyp /= Types.int) $ do
          Error.unexpectedType pos Types.int idenTyp
        checkTypStmts tl
      Abs.Ret _ expr -> do
        exprTyp <- checkTypExpr expr
        when (exprTyp /= typ) $ do
          Error.unexpectedType (Abs.hasPosition expr) typ exprTyp
        checkTypStmts tl
      Abs.VRet pos -> do
        when (Types.void /= typ) $ do
          Error.unexpectedType pos typ Types.void
        checkTypStmts tl
      Abs.Break _ ->
        checkTypStmts tl
      Abs.Continue _ ->
        checkTypStmts tl
      Abs.Cond _ expr stmt -> do
        exprT <- checkTypExpr expr
        when (exprT /= Types.bool) $ do
          let exprPos = Abs.hasPosition expr
          Error.unexpectedType exprPos Types.bool exprT
        checkTypStmts [stmt]
        checkTypStmts tl
      Abs.CondElse _ expr stmt1 stmt2 -> do
        exprT <- checkTypExpr expr
        when (exprT /= Types.bool) $ do
          let exprPos = Abs.hasPosition expr
          Error.unexpectedType exprPos Types.bool exprT
        checkTypStmts [stmt1]
        checkTypStmts [stmt2]
        checkTypStmts tl
      Abs.While _ expr stmt -> do
        exprT <- checkTypExpr expr
        when (exprT /= Types.bool) $ do
          let exprPos = Abs.hasPosition expr
          Error.unexpectedType exprPos Types.bool exprT
        checkTypStmts [stmt]
        checkTypStmts tl
      Abs.SExp _ expr -> do
        checkTypExpr expr
        checkTypStmts tl

stdlibTypes = [(Abs.Ident "printInt", Types.fun Types.void [Types.int]),
               (Abs.Ident "printString", Types.fun Types.void [Types.str]),
               (Abs.Ident "error", Types.fun Types.void []),
               (Abs.Ident "readInt", Types.fun Types.int []),
               (Abs.Ident "readString", Types.fun Types.str [])]

globalFunction :: Abs.Program' b -> Map.Map Ident Type
globalFunction (Abs.Program _ fndefs) =
  Map.fromList $ funs fndefs
  where
  funs = map (\(Abs.FnDef _ typ idn args _) -> (idn, Types.funSign typ args))

checkUnique :: Abs.Program -> UniqueM ()
checkUnique (Abs.Program _ fndefs) = do
  let identPosList = map (\(Abs.FnDef pos _ idn _ _) -> (idn, pos)) fndefs
  forM_ identPosList $ \(idn, pos) -> do
    defs <- get
    when (Map.member idn defs) $ do
      let pos2 = (Map.!) defs idn
      Error.duplicateDefs idn pos pos2
    modify (Map.insert idn pos)


checkTypes :: Abs.Program -> IO ()
checkTypes p@(Abs.Program _ fndefs) = do
  case (unique p) of
    Left err -> die err
    _        -> return ()
  let mainType = Map.lookup main globalScope
  case mainType of
    Nothing -> die "Undefined \"main\""
    Just t  -> case t of
      Abs.Fun _ (Abs.Int _) []      -> return ()
      Abs.Fun _ (Abs.Int _) (_ : _) -> die "\"main\" takes no arguments"
      Abs.Fun _ _ []                -> die "\"main\" must return int"
  case (check $ mapM_ checkTypFunDef fndefs) of
    Left err -> die err
    Right _  -> return ()
  where
    main = Abs.Ident "main"
    globalScope = Map.union (globalFunction p) $ Map.fromList stdlibTypes
    check m = runReader (runExceptT m) $ globalScope
    unique p = evalState (runExceptT $ checkUnique p) $ Map.empty
