module Imperative (
  def, var, lit, while, (+=), (-=), (*=)
) where

import Control.Monad.State
import Data.Vector as V hiding (update)
import Data.Vector.Mutable as VM

type Environment = Vector Integer
type Computation = State Environment
type Expression = Computation Value
type Statement = Computation ()
data Value = Var Int | Lit Integer

def :: Expression -> Integer
def r         = let (val, list) = runState r V.empty
                in fromEnv val list

fromEnv :: Value -> Environment -> Integer
fromEnv val list = 
  case val of
    Lit l -> l
    Var idx -> list ! idx
    
var :: Integer -> Expression
var v = state $ \s -> (Var (V.length s), snoc s v)

lit :: Integer -> Value
lit l         = (Lit l)

while :: Value -> (Integer -> Bool) -> Computation a -> Statement
while r f act = state loop
  where loop :: Environment -> ((), Environment) 
        loop env = if f (fromEnv r env) then loop (execState act env) else ((), env)

update :: Integer -> Value -> Environment -> ((), Environment)
update i v e = case v of
  Var v' -> let env = V.modify (\mv -> VM.write mv v' i) e in ((), env)
  Lit _ -> ((), e)
  
mkOperator :: (Integer -> Integer -> Integer) -> Value -> Value -> Statement
mkOperator f a b= state $ \env -> let a' = fromEnv a env 
                                      b' = fromEnv b env
                                  in update (f a' b') a env

(+=), (-=), (*=) :: Value -> Value -> Statement
(+=) = mkOperator (+)
(-=) = mkOperator (-)
(*=) = mkOperator (*)