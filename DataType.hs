module DataType where
import AbsJC
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import Data.Maybe

type Function = ([Var], Exp)
type Var = String
type Loc = Integer
data Val = 
    NIL
  | BOOL Bool
  | INT Integer
  | STR String 
  | ARR (Map Integer Val)
  | FUN Function
  | RET Val deriving (Eq, Ord)

instance Show Val where
  show NIL = "nil"
  show (BOOL b) = show b
  show (INT n) = show n
  show (STR s) = show s
  show (ARR m) = show $ elems m
  show (FUN (var, exp)) = "fun " ++ show var ++ " " ++ show exp
  show (RET v) = show v

type Env = Map Var Loc
type Store = Map Loc Val

type Interpreter a = StateT Store (ReaderT Env (ErrorT String IO)) a
