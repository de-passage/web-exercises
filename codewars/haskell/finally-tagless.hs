{-# LANGUAGE RankNTypes #-}

module Tagless where

import Prelude hiding (and, or)
import Data.Tuple (swap)
import qualified Data.Bool as B (bool)
import Control.Applicative
import Control.Lens.Combinators (Profunctor(..))

class Language r where
  here   :: r (a, h) a
  before :: r h a -> r (any, h) a
  lambda :: r (a, h) b -> r h (a -> b)
  apply  :: r h (a -> b) -> (r h a -> r h b)
  
  loop   :: r h (a -> a) -> r h a
  
  int    :: Int -> r h Int
  add    :: r h Int -> r h Int -> r h Int
  down   :: r h Int -> r h Int    -- \x -> x - 1
  up     :: r h Int -> r h Int    -- \x -> x + 1
  mult   :: r h Int -> r h Int -> r h Int
  gte    :: r h Int -> r h Int -> r h Bool -- greater than or equal
  
  bool   :: Bool -> r h Bool
  and    :: r h Bool -> r h Bool -> r h Bool
  or     :: r h Bool -> r h Bool -> r h Bool
  neg    :: r h Bool -> r h Bool
  
  ifte   :: r h Bool -> r h a -> r h a -> r h a -- if true then return left term, else return right term

type Term a = forall r h . Language r => r h a

data Interpreter r h = Interpreter { run :: r -> h }

instance Profunctor Interpreter where 
  dimap f g i = Interpreter $ g . run i . f

instance Functor (Interpreter r) where
  fmap = rmap

instance Applicative (Interpreter r) where
  pure = Interpreter . const
  i <*> j = Interpreter $ (run i) <*> (run j)

instance Language Interpreter where
  here = Interpreter fst
  before = lmap snd
  lambda i = Interpreter (\h a -> run i (a, h))
  int = pure
  bool = pure
  apply = (<*>) 
  
  loop l = Interpreter $ run l <*> run (loop l)
  
  add = liftA2 (+)
  down = rmap pred
  up  = rmap succ
  mult = liftA2 (*)
  gte = liftA2 (>=)
  
  and = liftA2 (&&)
  or = liftA2 (||)
  neg = rmap not
  
  ifte c a b = liftA3 B.bool b a c
  
interpret :: Term a -> a
interpret t = run t ()