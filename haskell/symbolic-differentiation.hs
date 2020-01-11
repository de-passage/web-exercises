module SymbolicDifferentiationOfPrefixExpressions (diff, derive, parseExpr, simplify) where

import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative ((<|>))
import Numeric
import GHC.Float

import Debug.Trace

data Expr = Constant (Double)
          | Var
          | Unary UnaryOp Expr
          | Binary BinaryOp Expr Expr deriving (Eq)

data BinaryOp = Plus | Minus | Mult | Div | Pow deriving (Eq)
data UnaryOp = Cos | Sin | Tan | Ln | Exp deriving (Eq)

prettify :: Double -> String
prettify v
  | v - fromIntegral (floor v) == 0 = formatRealFloat FFGeneric (Just 0) v
  | otherwise                       = formatRealFloat FFGeneric Nothing v

instance Show Expr where
  show (Constant x) = prettify x
  show Var = "x"
  show (Unary op expr) = "(" ++ show op ++ " " ++ show expr ++ ")"
  show (Binary op expr1 expr2) = "(" ++ show op ++ " " ++ show expr1 ++ " " ++ show expr2 ++ ")"

instance Show BinaryOp where
  show Plus = "+"
  show Minus = "-"
  show Mult = "*"
  show Div = "/"
  show Pow = "^"

instance Show UnaryOp where
  show Cos = "cos"
  show Sin = "sin"
  show Tan = "tan"
  show Ln = "ln"
  show Exp = "exp"

(.:) = ((.).(.)) 
infixr 9 .:

whitespace :: ReadP Char
whitespace = satisfy isSpace

var :: ReadP Expr
var = Var <$ char 'x'

integer :: ReadP String
integer = many1 (satisfy isDigit)

number :: ReadP String
number = do sign    <- option "" $ string "-"
            intPart <- integer
            decPart <- option "" $ do dot <- string "."
                                      nbs <- integer
                                      return $ dot ++ nbs
            return $ sign ++ intPart ++ decPart

constant :: ReadP Expr
constant = (Constant) <$> read <$> number

binOp :: ReadP Expr
binOp = do op <- operation
           many1 whitespace
           left <- expr 
           many1 whitespace
           right <- expr
           return $ Binary op left right
  where 
    operation :: ReadP BinaryOp
    operation = Plus <$ char '+'
            <|> Minus <$ char '-'
            <|> Mult <$ char '*'
            <|> Div <$ char '/'
            <|> Pow <$ char '^'
      
unOp :: ReadP Expr
unOp = do op <- operation
          many whitespace
          val <- expr
          return $ Unary op val
  where 
    operation = Sin <$ string "sin"
            <|> Cos <$ string "cos"
            <|> Tan <$ string "tan"
            <|> Ln <$ string "ln"
            <|> Exp <$ string "exp"
    
operation :: ReadP Expr 
operation = between (char '(' <* many whitespace) (many whitespace *> char ')') (binOp <|> unOp)
  
expr :: ReadP Expr
expr = var <|> constant <|> operation

mult :: Expr -> Expr -> Expr
mult = Binary Mult

plus :: Expr -> Expr -> Expr
plus = Binary Plus

minus :: Expr -> Expr -> Expr
minus = Binary Minus

divi :: Expr -> Expr -> Expr
divi = Binary Div

pow :: Expr -> Expr -> Expr
pow = Binary Pow

cosi :: Expr -> Expr
cosi = Unary Cos

sine :: Expr -> Expr
sine = Unary Sin

tang :: Expr -> Expr
tang = Unary Tan

logn :: Expr -> Expr
logn = Unary Ln

expo :: Expr -> Expr
expo = Unary Exp

derive :: Expr -> Expr
derive (Constant _) = Constant 0
derive Var = Constant 1
derive (Binary Pow expr1 expr2) = expr2 `mult` (expr1 `pow` (expr2 `minus` Constant 1))
derive (Binary Mult (Constant v) expr2) = (Constant v) `mult` derive expr2
derive (Binary Mult expr1 (Constant v)) = derive expr1 `mult` (Constant v)
derive (Binary Div expr1 expr2) = ((d1 `mult` expr2) `minus` (expr1 `mult` d2)) `divi` (expr2 `pow` Constant 2)
  where d1 = derive expr1
        d2 = derive expr2
-- derive (Binary Div (Constant v) expr2) = (Constant v) `divi` derive expr2
-- derive (Binary Div expr1 (Constant v)) = derive expr1 `divi` (Constant v)
derive (Binary op expr1 expr2) = Binary op (derive expr1) (derive expr2)
derive (Unary Sin expr) = derive expr `mult` cosi expr
derive (Unary Cos expr) = (Constant (-1) `mult` derive expr) `mult` (sine expr)
derive (Unary Tan expr) = derive expr `mult` ((Constant 1) `plus` ((tang expr) `pow` (Constant 2)))
derive (Unary Exp expr) = derive expr `mult` expo expr
derive (Unary Ln expr) = (Constant 1) `divi` expr

apply :: BinaryOp -> Double -> Double -> Double
apply Plus = (+)
apply Minus = (-)
apply Mult = (*)
apply Div = (/)
apply Pow = (**)

data Simplified = Simplifiable Expr | Terminal Expr

zero = Constant 0
one = Constant 1

simplify :: Expr -> Expr
simplify (Unary op expr) = Unary op (simplify expr)
simplify (Binary op expr1 expr2) = simplifyBin op expr1 expr2
  where 
    simplifyBin :: BinaryOp -> Expr -> Expr -> Expr
    simplifyBin Plus a b =
      case simplify a of 
        (Constant 0) -> simplify b
        (Constant x) -> case simplify b of 
          (Constant y) -> Constant (x + y)
          y -> (Constant x) `plus` y
        x -> case simplify b of
          (Constant 0) -> x
          y -> x `plus` y
    simplifyBin Minus a b =
      case simplify a of 
        (Constant 0) -> case simplify b of 
          (Constant y) -> Constant (-y)
          y -> Constant (-1) `mult` y
        (Constant x) -> case simplify b of 
          (Constant y) -> Constant (x - y)
          y -> (Constant x) `minus` y
        x -> case simplify b of
          (Constant 0) -> x
          y -> x `minus` y
    simplifyBin Mult a b =
      case simplify a of 
        (Constant 0) -> zero
        (Constant 1) -> simplify b
        (Constant x) -> case simplify b of 
          (Constant y) -> Constant (x * y)
          y -> (Constant x) `mult` y
        x -> case simplify b of
          (Constant 0) -> zero
          (Constant 1) -> x
          y -> x `mult` y
    simplifyBin Div a b =
      case simplify a of 
        (Constant 0) -> zero
        (Constant x) -> case simplify b of 
          (Constant y) -> Constant (x / y)
          y -> (Constant x) `divi` y
        x -> case simplify b of
          (Constant 0) -> error $ "dividing " ++ show x ++ " by 0"
          (Constant 1) -> x
          y -> x `divi` y
    simplifyBin Pow a b =
      case simplify a of 
        (Constant 0) -> zero
        (Constant 1) -> one
        (Constant x) -> case simplify b of 
          (Constant y) -> Constant (x ** y)
          y -> (Constant x) `pow` y
        x -> case simplify b of
          (Constant 0) -> one
          (Constant 1) -> x
          y -> x `pow` y
simplify other = other
      

evaluate :: Expr -> Expr
evaluate = simplify . derive

parse :: ReadP a -> String -> [a]
parse = map fst .: filter (null . snd) .: readP_to_S 

parseUnique :: ReadP a -> String -> a
parseUnique p s = if length result == 1 then head result else error $ "ill-formed input : " ++ s
  where result = parse p s

parseExpr :: String -> Expr
parseExpr = parseUnique expr

diff :: String -> String
diff = show . evaluate . parseExpr