module Helpers where

import AbsJC
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import Data.Maybe
import DataType
import Prelude hiding (lookup, null)

emptyStore :: Store
emptyStore = insert 0 (INT 0) empty

-- we keep on first place address of last allocated item
alloc :: Interpreter Loc
alloc = do
  (INT loc) <- getSafe 0
  modify (insert 0 (INT $ loc+1))
  return (loc+1)

alloc' :: Int -> Interpreter [Loc]
alloc' n = do
  (INT loc) <- getSafe 0
  modify (insert 0 (INT $ loc + toInteger n))
  return ([(loc + 1)..(loc + toInteger n)])

print' :: String -> Interpreter ()
print' s = liftIO $ putStrLn s

insert' :: Ord k => [(k,v)] -> Map k v -> Map k v
insert' list map =
  Prelude.foldr (\(k,v) m -> insert k v m) map list

evalCon :: Val -> Interpreter Bool
evalCon val = case val of
  (NIL) -> return False
  (BOOL b) -> return b
  (INT i) -> return (i > 0)
  (STR s) -> return (s /= "")
  (ARR m) -> return (not $ null m)
  (FUN _) -> return True
  -- should not happen, for developer purpose
  _ -> throwError $ "Error: type is not convertible to bool: " ++ show val

-- EXPRESSIONS AND ERROR HANDLERS

valSafe :: Var -> Interpreter Val
valSafe id = do
  loc <- askSafe id
  getSafe loc

askSafe :: Var -> Interpreter Loc
askSafe id = do
  mLoc <- asks (lookup id)
  case mLoc of
    Just loc -> return loc
    Nothing -> throwError $ "Variable " ++ id ++ " does not exists"

getSafe :: Loc -> Interpreter Val
getSafe loc = do
  mVal <- gets (lookup loc)
  case mVal of
    Just val -> return val
    -- should not happen, for developer purpose
    Nothing -> throwError $ "Null pointer expception: " ++ show loc

-- UNARY OPERATORS

(.+.) :: Val -> Interpreter Val
(.+.) v = case v of
  (INT n) -> return (INT $ abs n)
  _ -> throwError $ "Positive operator to wrong type: " ++ show v

(.-.) :: Val -> Interpreter Val
(.-.) v = case v of
  (INT n) -> return (INT $ -n)
  _ -> throwError $ "Negative operator to wrong type: " ++ show v

(.!.) :: Val -> Interpreter Val
(.!.) v = case v of
  (BOOL b) -> return (BOOL $ not b)
  _ -> throwError $ "Negative operator to wrong type: " ++ show v

-- MATHEMATICAL EXPRESSIONS

(=.) :: Val -> Val -> Interpreter Val
(=.) _ a = return a

(+.) :: Val -> Val -> Interpreter Val
(+.) (INT a) (INT b) = return (INT $ a + b)
(+.) (STR a) (STR b) = return (STR $ a ++ b)
(+.) a b = throwError $ "Addition to wrong types: " ++ show a ++ " " ++ show b

(-.) :: Val -> Val -> Interpreter Val
(-.) (INT a) (INT b) = return (INT $ a - b)
(-.) a b = throwError $ "Substraction to wrong types: " ++ show a ++ " " ++ show b

(*.) :: Val -> Val -> Interpreter Val
(*.) (INT a) (INT b) = return (INT $ a * b)
(*.) a b = throwError $ "Multiplication to wrong types: " ++ show a ++ " " ++ show b

(/.) :: Val -> Val -> Interpreter Val
(/.) (INT a) (INT b) =  case b of
  0 -> throwError "Division by zero is strictly prohibited!"
  _ -> return (INT $ div a b)
(/.) a b = throwError $ "Divition to wrong types: " ++ show a ++ " " ++ show b

-- LOGICAL EXPRESSIONS:

(||.) :: Val -> Val -> Interpreter Val
(||.) v1 v2 = do
  b1 <- evalCon v1
  b2 <- evalCon v2
  return (BOOL $ b1 || b2)

(&&.) :: Val -> Val -> Interpreter Val
(&&.) v1 v2 = do
  b1 <- evalCon v1
  b2 <- evalCon v2
  return (BOOL $ b1 && b2)

(==.) :: Val -> Val -> Interpreter Val
(==.) v1 v2 = case (v1, v2) of
  (INT i1, INT i2) -> return (BOOL $ i1 == i2)
  (STR s1, STR s2) -> return (BOOL $ s1 == s2)
  _ -> throwError $ "Equal operation to wrong types: " ++ show v1 ++ " " ++ show v2

(!=.) :: Val -> Val -> Interpreter Val
(!=.) v1 v2 = case (v1, v2) of
  (INT i1, INT i2) -> return (BOOL $ i1 /= i2)
  (STR s1, STR s2) -> return (BOOL $ s1 /= s2)
  _ -> throwError $ "Not equal operation to wrong types: " ++ show v1 ++ " " ++ show v2

(<.) :: Val -> Val -> Interpreter Val
(<.) v1 v2 = case (v1, v2) of
  (INT i1, INT i2) -> return (BOOL $ i1 < i2)
  (STR s1, STR s2) -> return (BOOL $ s1 < s2)
  _ -> throwError $ "Less then operation to wrong types: " ++ show v1 ++ " " ++ show v2

(>.) :: Val -> Val -> Interpreter Val
(>.) v1 v2 = case (v1, v2) of
  (INT i1, INT i2) -> return (BOOL $ i1 > i2)
  (STR s1, STR s2) -> return (BOOL $ s1 > s2)
  _ -> throwError $ "Greater then operation to wrong types: " ++ show v1 ++ " " ++ show v2
