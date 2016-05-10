module EvalExp where

import AbsJC
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import DataType
import Data.Map hiding (foldr)
import Data.Maybe
import Helpers
import Prelude hiding (lookup)

evalExp :: Exp -> Interpreter (Env, Val)

-- STATEMENTS: block, if, ifelse, while, for

evalExp (EBlock []) = do
  return (empty, NIL)

evalExp (EBlock (e:es)) = do
  ret <- evalExp e
  return'' ret (EBlock es)
  env <- ask
  return (env, NIL)

evalExp (EIfStmt ex1 ex2) = do
  (env, val) <- evalExp ex1
  con <- evalCon val
  if con then do
    ret <- local (const env) $ evalExp ex2
    return' ret
  else do
    return (env, NIL)

evalExp (EIfElse ex1 ex2 ex3) = do
  (env, val) <- evalExp ex1
  con <- evalCon val
  if con then do
    ret <- local (const env) $ evalExp ex2
    return' ret
  else do
    ret <- local (const env) $ evalExp ex3
    return' ret

evalExp whi@(EWhile ex1 ex2) = do
    (env, val) <- evalExp ex1
    con <- evalCon val
    if con then do
      ret <- local (const env) $ evalExp ex2
      return'' ret whi 
    else return (env, NIL)

evalExp (EForLoo (e1:e2:e3:ee) exp) = do
  (env, val) <- evalExp e1
  local (const env) $ evalExp (EWhile e2 $ EBlock $ exp:e3:[])

-- DECLARIATIONS: var, fun, array

evalExp (EDecEmp (Ident id)) = do
  env <- ask
  loc <- alloc
  modify (insert loc NIL)
  return (insert id loc env, NIL)

evalExp (EDecVar (Ident id) exp) = do
  loc <- alloc
  (env, val) <- evalExp exp
  modify (insert loc val)
  return ((insert id loc env), NIL)

evalExp (EDecAno par fun) = do
  env <- ask
  let ids = foldr (\(Ident i) acc -> i:acc) [] par
  return (env, FUN (ids, fun))

evalExp (EDecFun (Ident id) par fun) = do
  env <- ask
  loc <- alloc
  let ids = foldr (\(Ident i) acc -> i:acc) [] par
  modify (insert loc (FUN (ids, fun)))
  return ((insert id loc env), NIL)

-- FUNCTIONS: own, str, int, print

evalExp (EFun (EVar (Ident id)) exps) = do
  fun <- valSafe id
  case fun of
    (FUN (ids, exp)) -> do
      locs <- alloc' (length ids)
      (_, vals) <- evalExps exps
      modify (insert' $ zip locs vals)
      (_, ret) <- local (insert' $ zip ids locs) $ evalExp exp
      env' <- ask
      return (env', ret)
    _ -> throwError $ "Error: " ++ id ++ " is not a function"

evalExp (EFunExp []) = do
  env <- ask
  return (env, NIL)

evalExp (EFunExp (e:es)) = do
  (env, val) <- evalExp e
  -- Important: EFunExp unpack RET value before return
  -- it breaks chain of returning RET (look at return'')
  case val of
    (RET v) -> return (env, v)
    _ -> local (const env) $ evalExp (EFunExp es)

evalExp (EReturn exp) = do
    (env, val) <- evalExp exp
    -- Occurrence of EReturn stops further computation
    -- and return until achieve EFunExp which unpack the value
    return (env, RET val)

evalExp (EToStr e) = do
  (env, val) <- evalExp e
  case val of
    (INT i) -> return (env, (STR $ show i))
    _ -> throwError "Error: convertion integer to string with wrong type"

evalExp (EToInt e) = do
  (env, val) <- evalExp e
  case val of
    (STR s) -> case reads s of
      [(i, "")] -> return (env, (INT i))
      _ -> throwError $ "Error: cannot convert string " ++ s ++ " to integer"
    _ -> throwError "Error: convertion string to integer with wrong type"

evalExp (EPrint e) = do
  (env, val) <- evalExp e
  print' $ show val
  return (env, NIL)

-- ARRAYS

evalExp (EElem (Ident id) exp) = do
  (env, mInt) <- evalExp exp
  case mInt of
    (INT int) -> do
      arr <- valSafe id
      case arr of
        (ARR map) -> do
          env <- ask
          case (lookup int map) of
            Nothing -> return (env, NIL)
            Just val -> return (env, val)
        _ -> throwError $ "Error: " ++ id ++ " is not an array!"
    _ -> throwError "Error: array is indexing by integers only!"

evalExp (EAss (EElem (Ident id) ex1) ASgn ex2) = do
  (env, mInt) <- evalExp ex1
  case mInt of
    (INT int) -> do
      loc <- askSafe id
      arr <- getSafe loc
      case arr of
        (ARR map) -> do
          (env, val) <- evalExp ex2
          modify (insert loc (ARR (insert int val map)))
          return (env, NIL)
        _ -> throwError $ "Error: " ++ id ++ " is not an array!"
    _ -> throwError "Error: array is indexing with integers only!"


evalExp (EAss (EVar (Ident id)) ass exp) = do
  (env, new) <- evalExp exp
  loc <- askSafe id
  old <- getSafe loc
  val <- old `op` new
  modify (insert loc val)
  return (env, NIL)
  where op = case ass of {
    ASgn -> (=.) ;
    AAdd -> (+.) ;
    ASub -> (-.) ;
    AMul -> (*.) ;
    ADiv -> (/.) ;
  }

evalExp (EPre sym (EVar (Ident id))) = do
  loc <- askSafe id
  mInt <- getSafe loc
  case mInt of 
    (INT n) -> do
      let new = case sym of {
        (SInc) -> (INT $ n + 1);
        (SDec) -> (INT $ n - 1);
      }
      modify (insert loc new)
      env <- ask
      return (env, NIL)
    _ -> throwError "Error: ++/-- works only with integers"

evalExp (EPreOp unop exp) = do
  (env, val) <- evalExp exp
  new <- op val
  return (env, new)
  where op = case unop of {
    Poz -> (.+.);
    Neg -> (.-.);
    LNe -> (.!.);
  }

evalExp (EPost ide sym) = do
  evalExp (EPre sym ide)

-- Other expressions:
evalExp exp = do
  env <- ask
  case exp of

-- RAW VALUES

    (ENil) -> do
      return (env, NIL)

    (EBool n) -> do
      case n of
        (BTrue) -> return (env, BOOL True)
        (BFalse) -> return (env, BOOL False)

    (EInt n) -> do
      return (env, INT n)

    (EStr n) -> do
      return (env, STR n)

    (EVar (Ident id)) -> do
      val <- valSafe id
      return (env, val)

    (EArr exps) -> do
      (env', vals) <- evalExps exps
      return (env', ARR (fromList (zip [0..] vals)))

-- LOGICAL EXPRESSIONS:

    (ELor e1 e2) -> do
      (env', v1:v2:_) <- evalExps $ e1:e2:[]
      val <- v1 ||. v2
      return (env', val)

    (EAnd e1 e2) -> do
      (env', v1:v2:_) <- evalExps $ e1:e2:[]
      val <- v1 &&. v2
      return (env', val)

    (EEqu e1 e2) -> do
      (env', v1:v2:_) <- evalExps $ e1:e2:[]
      val <- v1 ==. v2
      return (env', val)

    (ENeq e1 e2) -> do
      (env', v1:v2:_) <- evalExps $ e1:e2:[]
      val <- v1 !=. v2
      return (env', val)

    (ELrt e1 e2) -> do
      (env', v1:v2:_) <- evalExps $ e1:e2:[]
      val <- v1 <. v2
      return (env', val)

    (EGrt e1 e2) -> do
      (env', v1:v2:_) <- evalExps $ e1:e2:[]
      val <- v1 >. v2
      return (env', val)

-- MATHEMATICAL EXPRESSIONS

    (EAdd e1 e2) -> do
      (env', v1:v2:_) <- evalExps $ e1:e2:[]
      val <- v1 +. v2
      return (env', val)

    (ESub e1 e2) -> do
      (env', v1:v2:_) <- evalExps $ e1:e2:[]
      val <- v1 -. v2
      return (env', val)

    (EMul e1 e2) -> do
      (env', v1:v2:_) <- evalExps $ e1:e2:[]
      val <- v1 *. v2
      return (env', val)

    (EDiv e1 e2) -> do
      (env', v1:v2:_) <- evalExps $ e1:e2:[]
      val <- v1 /. v2
      return (env', val)

    const -> do
      liftIO $ putStrLn $ "Warning: illegal construction: " ++ show const
      return (env, NIL)

-- HELPERS FUNCTIONS

evalExps :: [Exp] -> Interpreter (Env, [Val])
evalExps [] = do
  env <- ask
  return (env, [])

evalExps (e:es) = do
  (_, v) <- evalExp e
  (env, vs) <- evalExps es
  return $ (env, v:vs)

return' :: (Env, Val) -> Interpreter (Env, Val)
return' (env, val) = case val of
  (RET v) -> return (env, RET v)
  _ -> return (env, val)

return'' :: (Env, Val) -> Exp -> Interpreter (Env, Val)
return'' (env, val) exp = case val of
  (RET v) -> return (env, RET v)
  _ -> local (const env) $ evalExp exp
