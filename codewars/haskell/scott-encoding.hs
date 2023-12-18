{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

pair :: a -> b -> SPair a b
pair a b = SPair $ \f -> f a b
toPair :: SPair a b -> (a,b)
toPair = uncurry (,)
fromPair :: (a,b) -> SPair a b
fromPair (a, b) = pair a b
fst :: SPair a b -> a
fst p = runPair p const
snd :: SPair a b -> b
snd p = runPair p $ flip const
swap :: SPair a b -> SPair b a
swap p = pair (snd p) (fst p)
curry :: (SPair a b -> c) -> (a -> b -> c)
curry f a b = f $ pair a b
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f p = f (fst p) (snd p)

nothing' :: SMaybe a
nothing' = SMaybe const
just' :: a -> SMaybe a
just' a = SMaybe $ \_ f -> f a
toMaybe :: SMaybe a -> Maybe a
toMaybe m = runMaybe m Nothing (Just)
fromMaybe :: Maybe a -> SMaybe a
fromMaybe = maybe nothing' just'
isJust :: SMaybe a -> Bool
isJust m = runMaybe m False (const True)
isNothing :: SMaybe a -> Bool
isNothing = not . isJust
catMaybes :: SList (SMaybe a) -> SList a
catMaybes = foldr (\m l -> runMaybe m l (flip cons l)) empty

left' :: a -> SEither a b
left' a = SEither $ \f _ -> f a
right' :: b -> SEither a b
right' b = SEither $ \_ f -> f b
toEither :: SEither a b -> Either a b
toEither e = runEither e (Left) (Right)
fromEither :: Either a b -> SEither a b
fromEither = either left' right'
isLeft :: SEither a b -> Bool
isLeft e = runEither e (const True) (const False)
isRight :: SEither a b -> Bool
isRight = not . isLeft
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = foldr (\e p -> runEither e (\a -> pair (cons a $ fst p) (snd p)) (\b -> pair (fst p) (cons b $ snd p))) (pair empty empty)

empty :: SList a
empty = SList const
toList :: SList a -> [a]
toList l = runList l [] (\h t -> h : toList t)
fromList :: [a] -> SList a
fromList [] = empty
fromList (x:xs) = SList $ \_ f -> f x (fromList xs)
cons :: a -> SList a -> SList a
cons a l = SList $ \_ f -> f a l
concat :: SList a -> SList a -> SList a
concat a b = runList a b (\h t -> cons h (concat t b))
null :: SList a -> Bool
null l = runList l True (\_ _ -> False)
length :: SList a -> Int
length l = runList l 0 (\_ t -> succ . length $ t)
map :: (a -> b) -> SList a -> SList b
map f = foldr (\e l -> cons (f e) l) empty
zip :: SList a -> SList b -> SList (SPair a b)
zip l r = runList l empty (\h t -> runList r empty (\h' t' -> cons (pair h h') (zip t t')))
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f a l = runList l a (\h t -> foldl f (f a h) t)
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f a l = runList l a (\h t -> f h (foldr f a t))
take :: Int -> SList a -> SList a
take 0 _ = empty
take n l = runList l empty (\h t -> cons h $ take (pred n) t)