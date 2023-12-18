module PolynomField where

import Data.List (sortOn, intersperse)
import Data.Ord (Down(..))

data BinaryPolynom = BPoly [Int] deriving (Eq)

zero, one :: BinaryPolynom
zero = BPoly []
one  = BPoly [0]

lift :: ([Int] -> a) -> BinaryPolynom -> a
lift f (BPoly l) = f l

lift' :: ([Int] -> [Int]) -> BinaryPolynom -> BinaryPolynom
lift' f p = BPoly $ lift f p

isZero = lift null

deg :: BinaryPolynom -> Int
deg l = if isZero l then -1 else lift head l

-- | Constructs a monom with the given degree.
polyFromDeg :: Int -> BinaryPolynom
polyFromDeg = BPoly . return

polyFromPowers :: [Int] -> BinaryPolynom
polyFromPowers = BPoly . sortOn Down

instance Show BinaryPolynom where
    show = isZero >>= (\c -> 
            if c then const "0"
            else concat . intersperse " + " . lift (map $ ("x^" ++).show))

-- | Multiplication in the polynom ring.
multiply :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
multiply _ (BPoly []) = BPoly []
multiply bx (BPoly (y:ys)) = 
  lift (foldl sumPoly zero) bx
    where
      sumPoly acc p = acc .+. (lift' (p + y:) $ multiply (polyFromDeg p) (BPoly ys))

-- | Addition and multiplication in the polynom field.
(.+.), (.*.) :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
(BPoly []) .+. y = y
x .+. (BPoly []) = x
bx@(BPoly (x:xs)) .+. by@(BPoly (y:ys))
  | x == y = (BPoly xs) .+. (BPoly ys)
  | x > y = lift' (x:) (BPoly xs .+. by)
  | x < y = lift' (y:) (bx .+. BPoly ys)
  
x .*. y = multiply x y `polyMod` polyFromPowers [8, 4, 3, 1, 0]

polyMod = (snd.) . polyDivMod

polyDivMod :: BinaryPolynom -> BinaryPolynom -> (BinaryPolynom, BinaryPolynom)
polyDivMod x y
  | dx < dy = (zero, x)
  | otherwise = (qu .+. diffP, re)
  where dx = deg x
        dy = deg y
        diffP = polyFromDeg $ dx - dy
        adjustedX = (diffP `multiply` y) .+. x
        (qu, re) = adjustedX `polyDivMod` y