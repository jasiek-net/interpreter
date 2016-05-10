module Main where

import AbsJC
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import DataType
import Data.Map hiding (null, foldr)
import ErrM
import EvalExp
import Helpers
import LexJC
import ParJC
import PrintJC
import SkelJC
import System.IO.Error
import System.Environment
import System.IO

main = do
  file <- getArgs
  if null file then do
    putStrLn "Go to interactive mode"
    out <- runErrorT (runReaderT (runStateT (shell) emptyStore) empty)
    return ()
  else do
    handle <- openFile (head file) ReadMode
    program <- hGetContents handle
    case pProgram (myLexer program) of
      Ok (Pro pro) -> do
        out <- runErrorT (runReaderT (runStateT (evalPro pro) emptyStore) empty)
        case out of
          Left err -> hPutStrLn stderr err
          _ -> return ()
      Bad s -> hPutStrLn stderr $ "Parsing failed: " ++ show s

shell :: Interpreter (Env, Val)
shell = do
  liftIO $ putStr ">> " >> hFlush stdout
  program <- liftIO $ getLine
  case pProgram (myLexer program) of
    Ok (Pro (e:es)) -> do
      catchError
        (do
          (env, val) <- evalExp e
          print' $ show val
          local (const env) shell
        )
        (\err -> do
          liftIO $ hPutStrLn stderr $ "Error: " ++ err
          env <- ask
          local (const env) shell
        )
    Bad s -> do
      liftIO $ hPutStrLn stderr $ "Parsing failed: " ++ show s
      env <- ask
      local (const env) shell

evalPro :: [Exp] -> Interpreter ()
evalPro [] = do
  pro <- valSafe "main"
  case pro of
    (FUN (par, fun)) -> do
      (env, val) <- evalExp fun
      return ()
    _ -> throwError "Function main() not declared"

evalPro ((EDecEmp (Ident id)):es) = do
  loc <- alloc
  modify (insert loc NIL)
  local (insert id loc) $ evalPro es

evalPro ((EDecVar (Ident id) exp):es) = do
  loc <- alloc
  (env, val) <- evalExp exp
  modify (insert loc val)
  local (insert id loc) $ evalPro es

evalPro ((EDecFun (Ident id) par fun):es) = do
  loc <- alloc
  let ids = foldr (\(Ident i) acc -> i:acc) [] par
  modify (insert loc (FUN (ids, fun)))
  local (insert id loc) $ evalPro es

evalPro (e:es) = do
  liftIO $ putStrLn $ "Warning: illegal construction: " ++ show e
  evalPro es

