{-# LANGUAGE LambdaCase #-}

module Coroutine where

import Control.Monad (ap, forever)
import Preloaded

-- Preloaded contains the following:
-- {-# LANGUAGE DeriveFunctor #-}
--
-- newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)
--
-- data Command r u d a =
--   Done a
-- | Out d (Coroutine r u d a)
-- | In (u -> Coroutine r u d a) deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

-- Makes a coroutine that always produces the given command 
toC :: Command r u d a -> Coroutine r u d a
toC c = Coroutine ($ c)

-- Makes a coroutine that only produces Done commands
doneC :: a -> Coroutine r u d a
doneC = toC . Done

-- Makes a coroutine that only produces In commands
inC :: (u -> Coroutine r u d a) -> Coroutine r u d a
inC = toC . In

-- Makes a coroutine that only produce Out commands
outC :: d -> Coroutine r u d a -> Coroutine r u d a
outC = (toC .) . Out

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
  return x = doneC x
  f >>= g  = Coroutine (\k -> apply f (\case 
                                        Done a -> apply (g a) k
                                        Out a c -> k $ Out a (c >>= g)
                                        In h -> k $ In (\u -> (h u) >>= g)))

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
p1 >>> p2 = Coroutine (\k -> 
  apply p2 (\case
              Done a -> k $ Done a
              Out a c -> k $ Out a (p1 >>> c)
              In f -> apply (pipe2 p1 f) k))

-- It might be useful to define the following function 
pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a
pipe2 c f = Coroutine (\k ->
  apply c (\case 
           Done a -> k $ Done a
           Out m cruma -> apply (cruma >>> (f m)) k
           In g -> k $ In (\u -> pipe2 (g u) f)))

-- Library functions

output :: a -> Coroutine r u a ()
output v = outC v (return ())

input :: Coroutine r v d v
input = inC return

produce :: [a] -> Coroutine r u a ()
produce [] = return ()
produce (x:xs) = outC x (produce xs)

consume :: Coroutine [t] u t a -> [t]
consume c = apply c (\case 
                      Done a -> []
                      Out a c1 -> a : consume c1
                      In f -> [])

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = inC $ \v -> if p v then outC v (filterC p) else filterC p

limit :: Int -> Coroutine r v v ()
limit n = inC $ \v -> if n <= 0 then doneC () else outC v $ limit (pred n)

passthrough :: Coroutine r v v a
passthrough = inC $ \v -> outC v passthrough

suppress :: Int -> Coroutine r v v ()
suppress n = inC $ \v -> if n <= 0 then outC v passthrough else suppress (pred n)

add :: Coroutine r Int Int ()
add = inC $ \v -> inC $ \u -> outC (v + u) add

duplicate :: Coroutine r v v ()
duplicate = inC $ \v -> outC v (outC v duplicate)

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers 
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filterC even >>> limit 5
p2 = triangular 0 1 
  where triangular acc i = output (acc + i) >> triangular (acc + i) (i + 1)
p3 = duplicate >>> add 
p4 = duplicate >>> suppress 1 >>> add