{-# LANGUAGE FlexibleInstances, UndecidableInstances, GADTs, FunctionalDependencies #-}
module PolyvariadicFunctions where

class PolyAddable r where 
  polyAdd' :: Int -> r

instance PolyAddable Int where
  polyAdd' = id

instance (a ~ Int, PolyAddable r) => PolyAddable (a -> r) where
  polyAdd' acc = \i -> polyAdd' (i + acc)

polyAdd :: PolyAddable r => r  
polyAdd = polyAdd' 0

class PolyListable a r | r -> a where
  polyList' :: [a] -> r
  
instance PolyListable a [a] where
  polyList' = reverse
  
instance (PolyListable a r) => PolyListable a (a -> r) where
  polyList' l = \e -> polyList' (e:l)

-- `polyList` turns its arguments into a list, polymorphically.
polyList :: PolyListable a b => b
polyList = polyList' []


class PolyWordable r where
  polyWords' :: [String] -> r
  
instance PolyWordable String where
  polyWords' = unwords . reverse
  
instance (a ~ String, PolyWordable r) => PolyWordable ((->) a r) where
  polyWords' l = \s -> polyWords' (s:l)
  
-- `polyWords` turns its arguments into a spaced string.
polyWords :: PolyWordable r => r
polyWords = polyWords' []


{-- 
This is not my solution, but I'll keep it here for future reference. This is a lot more elegant

{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             FunctionalDependencies,
             TypeFamilies #-}
module PolyvariadicFunctions where

class Variadic a r t | t -> r where
  liftVariadic :: ([a] -> r) -> t

instance Variadic a r r where
  liftVariadic f = f []

instance (a ~ a', Variadic a r t) => Variadic a r (a' -> t) where
  liftVariadic f h = liftVariadic (f . (h:))

polyAdd :: Variadic Int Int t => t
polyAdd = liftVariadic (sum :: [Int] -> Int)

polyWords :: Variadic String String t => t
polyWords = liftVariadic unwords

polyList :: Variadic a [a] t => t
polyList = liftVariadic id
--}