{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return = (Identity)
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = (Just)
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  (State g) >>= f = State $ \s -> 
    let (a, s') = g s in
    let (State g') = f a in
      g' s'

instance Monad (Reader s) where
  return = Reader . const 
  (Reader g) >>= f = Reader $ \s -> runReader (f $ g s) $ s

instance Monoid w => Monad (Writer w) where
  return a = Writer (mempty, a)
  (Writer (s, v)) >>= f = Writer $
    let (Writer (l, r)) = f v in
      (mappend s l, r)