module ISO where

import Data.Maybe
import Data.Void
import Control.Arrow ((***))
import Data.Bool
import Data.Tuple

-- A type of `Void` have no value.
-- So it is impossible to construct `Void`,
-- unless using undefined, error, unsafeCoerce, infinite recursion, etc
-- And there is a function
-- absurd :: Void -> a
-- That get any value out of `Void`
-- We can do this becuase we can never have void in the zeroth place.

-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = 
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (fmap ab, fmap ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (left, right)
  where
    left  (Left  a) = Left  (ab a)
    left  (Right c) = Right (cd c)
    right (Left  b) = Left  (ba b)
    right (Right d) = Right (dc d)

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (\f -> cd . f . ba, \g -> dc . g . ab)

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe m@(mamb, mbma) = 
  (\a -> get $ mamb $ Just a, substL $ isoUnMaybe $ symm m)
  where
    get (Just b) = b
    get Nothing = fromJust (mamb Nothing)
    -- Suppose mamb return Nothing
    -- Since mamb (Just a) is Nothing, mbma Nothing is Just a.
    -- Since mamb Nothing, mbma Nothing is Nothing
    -- mbma Nothing can only be Just a, or Nothing, but cannot be both!
    -- So there is a contraidction, this case is impossible.

-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Note that we have
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (left, right)
  where
    left  (Left     l)  = Left  (():l)
    left  (Right    _)  = Left  []
    right (Left    [])  = Right ()
    right (Left (_:l))  = Left  l
    right (Right    v)  = absurd v -- absurd :: Void -> a
-- where (), the empty tuple, has 1 value, and Void has 0 value
-- If we have isoUnEither,
-- We have ISO () Void by calling isoUnEither isoEU
-- That is impossible, since we can get a Void by substL on ISO () Void
-- So it is impossible to have isoUnEither

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (symm, symm)

-- Sometimes, we can treat a Type as a Number:
-- if a Type t has n distinct value, it's Number is n.
-- This is formally called cardinality.
-- See https://en.wikipedia.org/wiki/Cardinality

-- Void has cardinality of 0 (we will abbreviate it Void is 0).
-- () is 1.
-- Bool is 2.
-- Maybe a is 1 + a.
-- We will be using peano arithmetic so we will write it as S a.
-- https://en.wikipedia.org/wiki/Peano_axioms
-- Either a b is a + b.
-- (a, b) is a * b.
-- a -> b is b ^ a. Try counting (() -> Bool) and (Bool -> ())

-- Algebraic data type got the name because
-- it satisfies a lot of algebraic rules under isomorphism

-- a = b -> c = d -> a * c = b * d
isoProd :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoProd = isoTuple

-- a = b -> c = d -> a + c = b + d
isoPlus :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoPlus = isoEither

-- a = b -> S a = S b
isoS :: ISO a b -> ISO (Maybe a) (Maybe b)
isoS = isoMaybe

-- a = b -> c = d -> c ^ a = d ^ b
isoPow :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoPow = isoFunc

right = (Right)
left = (Left)

-- a + b = b + a
plusComm :: ISO (Either a b) (Either b a)
plusComm = (either right left, either right left)

-- a + b + c = a + (b + c)
plusAssoc :: ISO (Either (Either a b) c) (Either a (Either b c))
plusAssoc = ( either (either left (right.left)) (right.right)
            , either (left.left) (either (left.right) right)) 

-- a * b = b * a
multComm :: ISO (a, b) (b, a)
multComm = (swap, swap)

-- a * b * c = a * (b * c)
multAssoc :: ISO ((a, b), c) (a, (b, c))
multAssoc = (\((a,b),c) -> (a,(b,c)), \(a,(b,c)) -> ((a,b),c))

-- dist :: a * (b + c) = a * b + a * c
dist :: ISO (a, (Either b c)) (Either (a, b) (a, c))
dist = (l, r)
  where 
    l (a, e) = either (\b -> left (a, b)) (\c -> right (a, c)) e
    r = either (id *** left) (id *** right)

-- (c ^ b) ^ a = c ^ (a * b)
curryISO :: ISO (a -> b -> c) ((a, b) -> c)
curryISO = (uncurry, curry)

-- 1 = S O (we are using peano arithmetic)
-- https://en.wikipedia.org/wiki/Peano_axioms
one :: ISO () (Maybe Void)
one = (const Nothing, const ())

-- 2 = S (S O)
two :: ISO Bool (Maybe (Maybe Void))
two = (bool Nothing (Just Nothing), isJust)

-- O + b = b
plusO :: ISO (Either Void b) b
plusO = (left, Right)
  where
    left (Left  x) = absurd x -- absurd :: Void -> a
    left (Right x) = x

-- S a + b = S (a + b)
plusS :: ISO (Either (Maybe a) b) (Maybe (Either a b))
plusS = (l, r)
  where 
    l = either (>>= return.left) ((Just).right)
    r = (fromMaybe (Left Nothing)) . (>>= return . (either (left.return) right))

-- 1 + b
-- = S 0 + b    (one)
-- = S (0 + b)  (plusS)
-- = S b        (plus0)
plusSO :: ISO (Either () b) (Maybe b)
plusSO = 
  isoPlus one refl `trans` 
    plusS `trans`
    isoS plusO

-- O * a = O
multO :: ISO (Void, a) Void
multO = (undefined, absurd)

-- S a * b = b + a * b
multS :: ISO (Maybe a, b) (Either b (a, b))
multS = (l, r)
  where 
    l (ma, b) = 
      case ma of
        Nothing -> Left b
        Just a  -> Right (a, b)
    r = either ((,) Nothing) (return *** id)

-- 1 * b
-- = S 0 * b    (one)
-- = b + 0 * b  (multS)
-- = b + 0      (mult0)
-- = 0 + b      (plusComm)
-- = b          (plus0)
multSO :: ISO ((), b) b
multSO =
  isoProd one refl `trans`
    multS `trans`
    isoPlus refl multO `trans` 
    plusComm `trans`
    plusO

-- a ^ O = 1
powO :: ISO (Void -> a) ()
powO = (const (), const absurd)

-- a ^ (S b) = a * (a ^ b)
powS :: ISO (Maybe b -> a) (a, b -> a)
powS = (l, r)
  where
    l f = (f Nothing, f.(Just))
    r (a, f) mb =
      case mb of
        Nothing -> a
        Just b -> f b
    

-- a ^ 1
-- = a ^ (S 0)    (one)
-- = a * (a ^ 0)  (powS)
-- = a * 1        (pow0)
-- = 1 * a        (multComm)
-- = a            (multS0)
-- Go the hard way (like multSO, plusSO)
-- to prove that you really get what is going on!
powSO :: ISO (() -> a) a
powSO = 
  isoPow one refl `trans`     -- isoPow one refl :: ISO (() -> a) (Maybe Void -> a) , ie a ^ 1 = a ^ S 0. 
                              -- 'refl' represents the variable, we have no other way to represent one than 'one',
                              -- which by definition is S 0
    powS `trans`              -- each application of `trans` is a step of the proof
    isoProd refl powO `trans` -- here the expected type is ISO (a, Void -> a) (a, ()), but powO is ISO (Void -> a) ()
                              -- so we rebind the variable by making a new multiplication with isoProd 
    multComm `trans`          -- ofc we need to prove commutativity as well
    multSO                    -- last substitution of the proof, it typechecks meaning that it's correct
    
-- Here's a trick: 
-- replace undefined with the rhs of the comment on previous line
-- When you're not sure what to fill in for a value,
-- Have it as a _
-- GHC will goes like
-- "Found hole `_' with type: ISO (() -> a) (Maybe b0 -> a0)"
-- So you can immediately see value of what type are needed
-- This process can be repeat indefinitely -
-- For example you might replace `_` with `isoFunc _ _`
-- So GHC hint you on more specific type.
-- This is especially usefull if you have complex type.
-- See https://wiki.haskell.org/GHC/Typed_holes
-- And "stepwise refinement" for more details.