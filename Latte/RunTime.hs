{-# LANGUAGE FlexibleContexts #-}

module Latte.RunTime where

import Latte.Abs as Abs
import qualified Latte.Types as Types
import qualified Latte.Error as Error

import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import Data.Foldable
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class
import qualified Text.Read
import System.Exit


type Expr = Abs.Expr' ()
type Type = Abs.Type' ()

data Data = IntVal Integer
          | StrVal [Char]
          | BoolVal Bool
          | Reference Loc
          | Function [Abs.Arg] Abs.Block Env
          | NoResult
          | BuiltinBreak
          | BuiltinContinue
          | BuiltinUninitialized
          | BuiltinPrintStr
          | BuiltinPrintInt
          | BuiltinReadInt
          | BuiltinReadStr
          | BuiltinError
  deriving (Eq, Show)

type Loc = Int
type Env = Map.Map Abs.Ident Loc
type Store = (Loc, Map.Map Loc Data)

type RunTimeM a = ExceptT String (ReaderT Env (StateT Store IO)) a

newloc :: RunTimeM Loc
newloc = do
  (loc, store) <- get
  put (loc + 1, store)
  return loc

modifyState :: (Map.Map Loc Data -> Map.Map Loc Data) -> RunTimeM ()
modifyState f = do
  modify (\(l, s) -> (l, f s))

getVariable :: BNFC'Position -> Abs.Ident -> RunTimeM Data
getVariable pos ident = do
  env <- ask
  state <- get
  let loc = fromJust $ Map.lookup ident env
  (_, state) <- get
  let val = fromJust $ Map.lookup loc state
  if (val /= BuiltinUninitialized) then
    return val
  else
    Error.uninitializedIdent pos ident

declareArgs :: [Abs.Arg] -> [Data] -> RunTimeM a -> RunTimeM a
declareArgs [] [] cont = cont
declareArgs ((Abs.Arg _ _ ident) : tlId) (val : tlVal) cont =
  case val of
    Reference var -> do
      env <- ask
      local (Map.insert ident var) $ declareArgs tlId tlVal cont
    _ -> do
      loc <- newloc
      modifyState (Map.insert loc val)
      env <- ask
      local (Map.insert ident loc) $ declareArgs tlId tlVal cont

declareVars :: Data -> [Abs.Item] -> RunTimeM a -> RunTimeM a
declareVars defl l cont = do
  env <- ask
  declV l cont env
  where
    declV []        cont env = cont
    declV (hd : tl) cont env = case hd of
      Abs.NoInit _ ident -> do
        loc <- newloc
        modifyState (Map.insert loc defl)
        local (Map.insert ident loc) $ declV tl cont env
      Abs.Init _ ident expr -> do
        val <- local (const env) $ evalExpr expr
        loc <- newloc
        modifyState (Map.insert loc val)
        local (Map.insert ident loc) $ declV tl cont env

interpretBlock :: Abs.Block -> RunTimeM (Maybe Data)
interpretBlock (Abs.Block _ stmts) = interpretStmts stmts where
  interpretStmts [] = return Nothing
  interpretStmts (hd : tl) = case hd of
    Abs.Empty _ -> interpretStmts tl
    Abs.BStmt _ block -> do
      blockRes <- interpretBlock block
      if (isJust blockRes) then
        return blockRes
      else
        interpretStmts tl
    Abs.Decl _ typ items -> do
      deflaultVal <- case typ of
        Abs.Int   _ -> return $ IntVal 0
        Abs.Str   _ -> return $ StrVal ""
        Abs.Bool  _ -> return $ BoolVal False
        _           -> return $ BuiltinUninitialized
      declareVars deflaultVal items $ interpretStmts tl
    Abs.NestFun _ (Abs.FnDef _ typ ident args block) -> do
      loc <- newloc
      local (Map.insert ident loc) $ do
        env <- ask
        modifyState (Map.insert loc (Function args block env))
        interpretStmts tl
    Abs.Ass _ ident expr -> do
      loc <- liftM (\x -> fromJust $ Map.lookup ident x) ask
      val <- evalExpr expr
      modifyState (Map.insert loc val)
      interpretStmts tl
    Abs.Incr p ident -> do
      loc <- liftM (\x -> fromJust $ Map.lookup ident x) ask
      oldVal' <- getVariable p ident
      let IntVal oldVal = oldVal'
      modifyState (Map.insert loc (IntVal $ oldVal + 1))
      interpretStmts tl
    Abs.Decr p ident -> do
      loc <- liftM (\x -> fromJust $ Map.lookup ident x) ask
      oldVal' <- getVariable p ident
      let IntVal oldVal = oldVal'
      modifyState (Map.insert loc (IntVal $ oldVal - 1))
      interpretStmts tl
    Abs.Ret _ expr -> do
      val <- evalExpr expr
      return $ Just val
    Abs.VRet     _ -> return $ Just NoResult
    Abs.Break    _ -> return $ Just BuiltinBreak
    Abs.Continue _ -> return $ Just BuiltinContinue
    Abs.Cond _ expr stmt -> do
      val' <- evalExpr expr
      let BoolVal val = val'
      res <- if val then
        interpretStmts [stmt]
      else
        return Nothing
      if (isJust res) then
        return res
      else
        interpretStmts tl
    Abs.CondElse _ expr stmt1 stmt2 -> do
      val' <- evalExpr expr
      let BoolVal val = val'
      res <- if val then
        interpretStmts [stmt1]
      else
        interpretStmts [stmt2]
      if (isJust res) then
        return res
      else
        interpretStmts tl
    Abs.While a expr stmt -> do
      val' <- evalExpr expr
      let BoolVal val = val'
      res <- if val then
        interpretStmts [stmt]
      else
        return $ Just BuiltinBreak
      if (res == (Just BuiltinBreak)) then
        interpretStmts tl
      else if (res == (Just BuiltinContinue)) then
        interpretStmts (hd : tl)
      else if (isJust res) then do
        return res
      else
        interpretStmts (hd : tl)
    Abs.SExp _ expr -> do
      evalExpr expr
      interpretStmts tl


zeroDivSens :: Abs.MulOp' a -> Bool
zeroDivSens op = case op of
  Abs.Times _ -> False
  Abs.Div   _ -> True
  Abs.Mod   _ -> True

evalExpr :: Abs.Expr -> RunTimeM Data
evalExpr expr = do
  case expr of
    Abs.EVar p ident -> do
      getVariable p ident
    Abs.ERef _ ident -> do
      env <- ask
      return . Reference . fromJust $ Map.lookup ident env
    Abs.ELitInt _ val ->
      return . IntVal $ val
    Abs.ELitTrue _ ->
      return . BoolVal $ True
    Abs.ELitFalse _ ->
      return . BoolVal $ False
    Abs.EApp pos ident exprs -> do
      fun <- getVariable pos ident
      store <- get
      valExprs <- mapM evalExpr exprs
      case fun of
        Function args block env -> do
          local (const env) $ do
            declareArgs (reverse args) (reverse valExprs) $ do
              val <- interpretBlock block
              return $ fromJust val
        BuiltinPrintStr -> do
          let StrVal strVal = head valExprs
          liftIO $ putStr strVal
          return NoResult
        BuiltinPrintInt -> do
          let IntVal intVal = head valExprs
          liftIO $ print intVal
          return NoResult
        BuiltinReadInt -> do
          numS <- liftIO $ getLine
          case Text.Read.readEither numS of
            Right num -> return $ IntVal num
            Left _    -> Error.parseIntegerErr pos
        BuiltinReadStr -> do
          str <- liftIO $ getLine
          return $ StrVal str
        BuiltinError -> do
          Error.runtimeError pos
    Abs.EString _  str ->
      return $ StrVal str
    Abs.ELambda _ _ args block -> do
      env <- ask
      return $ Function args block env
    Abs.Neg _ ex -> do
      val' <- evalExpr ex
      let IntVal val = val'
      return $ IntVal (-val)
    Abs.Not _ ex -> do
      val' <- evalExpr ex
      let BoolVal val = val'
      return $ BoolVal (not val)
    Abs.EMul _ ex1 op ex2 -> do
      val1' <- evalExpr ex1;
      val2' <- evalExpr ex2;
      let IntVal val1 = val1'
      let IntVal val2 = val2'
      when (val2 == 0 && (zeroDivSens op)) $ do
        let pos = Abs.hasPosition op
        Error.zeroDivisionError pos
      operation <- case op of
        Abs.Times _ -> return (*)
        Abs.Div   _ -> return (div)
        Abs.Mod   _ -> return (mod)
      return . IntVal $ operation val1 val2
    Abs.EAdd _ ex1 op ex2 -> do
      val1' <- evalExpr ex1
      val2' <- evalExpr ex2
      let IntVal val1 = val1'
      let IntVal val2 = val2'
      operation <- case op of
        Abs.Plus  _ -> return (+)
        Abs.Minus _ -> return (-)
      return . IntVal $ operation val1 val2
    Abs.ERel _ ex1 op ex2 -> do
      val1' <- evalExpr ex1
      val2' <- evalExpr ex2
      let IntVal val1 = val1'
      let IntVal val2 = val2'
      operation <- case op of
        Abs.LTH _ -> return (<)
        Abs.LE  _ -> return (<=)
        Abs.GTH _ -> return (>)
        Abs.GE  _ -> return (>=)
        Abs.EQU _ -> return (==)
        Abs.NE  _ -> return (/=)
      return . BoolVal $ operation val1 val2
    Abs.EAnd _ ex1 ex2 -> do
      val1' <- evalExpr ex1
      val2' <- evalExpr ex2
      let BoolVal val1 = val1'
      let BoolVal val2 = val2'
      return . BoolVal $ (&&) val1 val2
    Abs.EOr _ ex1 ex2 -> do
      val1' <- evalExpr ex1
      val2' <- evalExpr ex2
      let BoolVal val1 = val1'
      let BoolVal val2 = val2'
      return . BoolVal $ (||) val1 val2

stdlib :: [(String, Int, Data)]
stdlib = [("printInt", 0, BuiltinPrintInt),
          ("printString", 1, BuiltinPrintStr),
          ("error", 2, BuiltinError),
          ("readInt", 3, BuiltinReadInt),
          ("readString", 4, BuiltinReadStr)]

doStart :: [Abs.FunDef] -> RunTimeM Data
doStart fndefs = do
  idenLocs <- forM fndefs $ \(Abs.FnDef _ _ ident _ _) -> do
    loc <- newloc
    return (ident, loc)
  let mapIdenLoc = Map.fromList idenLocs
  local (Map.union mapIdenLoc) $ do
    env <- ask
    forM_ fndefs $ \(Abs.FnDef _ typ ident args block) -> do
      let loc = (Map.!) mapIdenLoc ident
      modifyState (Map.insert loc $ Function args block env)
    evalExpr (Abs.EApp Nothing main [])
  where
    loadIdent monad ((Abs.FnDef _ _ idn _ _), loc) = do
      local (Map.insert idn loc) monad
    main = Abs.Ident "main"

runProgram :: Abs.Program -> IO ()
runProgram (Abs.Program _ fndefs) = do
  result <- run $ doStart fndefs
  case result of
    Right (IntVal 0)  -> exitSuccess
    Right (IntVal c)  -> exitWith $ ExitFailure $ fromIntegral c
    Left str          -> die str
  where
    run c = evalStateT (runReaderT (runExceptT c) initEnv) initState
    initState = (length stdlib,
      Map.fromList $ map (\(_, y, z) -> (y, z)) stdlib)
    initEnv = Map.fromList $ map (\(x, y, _) -> (Abs.Ident(x), y)) stdlib
