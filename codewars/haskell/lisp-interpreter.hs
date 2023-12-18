module LispLovesMe where

import Control.Applicative ((<|>))
import Control.Monad (liftM)
import Data.Char
import Data.List (intersperse, foldl1')
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.ReadP

-- composition of functions with 2 arguments
(.:) = ((.).(.))
infixr 9 .:

-- left to right association of 2-arity functions
(|-->) = flip (.:)
infixl 9 |-->

-- left to right association of 1-arity functions
(|->) = flip (.)
infixl 9 |->

-- left to right function application
(|>>) = flip ($)
infixl 1 |>>

-- flipped Data.Bool.bool
if' a b c = if c then a else b

-- monadic application to reader: if'' c a b x = if c x then a x else b x
if'' c a b = c >>= if' a b

-- Returns the list of successful parses that consumed the entire input 
parse :: ReadP a -> String -> [a]
parse = readP_to_S |--> filter (null.snd) |--> map fst

-- Returns a result if the parser consumed the entire input and returned a single result
parseUnique :: ReadP a -> String -> Maybe a
parseUnique = parse |--> if'' (length |-> (== 1))
                              (return.head)
                              (const Nothing)

data AST = I32 Int
         | Sym String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq, Show)

-- Prelude functions & their evaluation
preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
  [ ("+", arithmetic (+))
  , ("*", arithmetic (*))
  , ("-", arithmetic (-))
  , ("/", arithmetic div)
  , ("^", arithmetic (^))
  , (">", comparison (>))
  , ("<", comparison (<))
  , ("!", neg)
  , ("size", size)
  , ("reverse", rev)
  , ("==", comparison (==))
  , (">=", comparison (>=))
  , ("<=", comparison (<=))
  , ("!=", comparison (/=))
  , ("if", cond)
  ]

-- Negate a boolean value, error if not exactly 1 Bool
neg :: [AST] -> AST
neg [(Boo b)] = Boo $ not b
neg _ = Err

-- if then else construct, error if not 1 Bool followed by 1 or 2 values
cond :: [AST] -> AST
cond [(Boo c), a, b] = if c then a else b
cond [(Boo c), a] = if c then a else Nul
cond _ = Err
  
-- Lift a binary operation on ints to AST context
liftI32 :: (Int -> Int -> Int) -> AST -> AST -> AST
liftI32 f (I32 x) (I32 y) = I32 $ f x y
liftI32 _ _ _ = Err

-- Apply sequentially the given operation to the list if its length >= 1, error otherwise
arithmetic :: (Int -> Int -> Int) -> [AST] -> AST
arithmetic op = if'' (length |-> (< 1))
                     (const Err)
                     (foldl1' (liftI32 op))
                     
-- Comparison lifted to AST, error if not exatly two Int argumetns
comparison :: (Int -> Int -> Bool) -> [AST] -> AST
comparison x [(I32 a), (I32 b)] = Boo $ x a b
comparison _ _ = Err

-- Size of a Lst AST
size :: [AST] -> AST
size [Nul] = I32 0
size [Lst l] = I32 (length l)
size _ = Err

-- Reverse of a Lst AST
rev :: [AST] -> AST
rev [Nul] = Nul
rev [Lst l] = Lst (reverse l)
rev _ = Err

-- Parsers

nullE :: ReadP AST
nullE = Nul <$ (string "()" <|> string "null")

boolE :: ReadP AST
boolE =   Boo True <$ string "true"
      <|> Boo False <$ string "false"
      
symbol :: ReadP String
symbol = pure (:) <*> satisfy (`notElem` " ,\t\n\r()" ++ ['0'..'9']) <*> munch (`notElem` " ,\n\t\r()")

symbolE :: ReadP AST
symbolE = Sym <$> (symbol >>= ignoreBuiltIn)
  where 
    ignoreBuiltIn :: String -> ReadP String
    ignoreBuiltIn = if'' (`notElem` ["null", "true", "false", "list", ".."])
                          (return.id)
                          (const pfail)

number :: ReadP Int
number = read <$> many1 (satisfy isDigit)

numberE :: ReadP AST
numberE = I32 <$> number

rangeE :: ReadP [AST]
rangeE = betweenParen $ do  string ".."
                            many separators
                            beg <- number
                            many1 separators
                            end <- number
                            return $ map I32 $ enumFromTo beg end

litListE :: ReadP [AST]
litListE = betweenParen $ do  string "list"
                              many1 (many1 separators *> expr)
                      

listE :: ReadP AST
listE = Lst <$> (rangeE <|> litListE)

expr :: ReadP AST
expr = (nullE <|> boolE <|> symbolE <|> numberE <|> listE <|> node)

separators :: ReadP ()
separators = () <$ (satisfy isSpace <|> char ',')

betweenParen = between (char '(' <* many separators) (many separators *> char ')') 

node :: ReadP AST
node = betweenParen
     $ pure Nod <*> expr <*> many ((many1 separators *> expr) <|> node)
     
prettyPrint :: AST -> String
prettyPrint (I32 i) = show i
prettyPrint (Sym sym) = sym
prettyPrint Nul = "null"
prettyPrint Err = "!error!"
prettyPrint (Lst list) = "(list " ++ (concat $ intersperse " " $ map prettyPrint list) ++ ")"
prettyPrint (Boo True) = "true"
prettyPrint (Boo False) = "false"
prettyPrint (Nod ast list) = "(" ++ (concat . intersperse " " . map prettyPrint $ (ast:list)) ++ ")"

-- parse to AST
lispParser :: ReadP AST
lispParser = many separators *> expr <* many separators

-- Parse then pretty print
lispPretty :: String -> Maybe String
lispPretty = parseUnique lispParser
           |-> (prettyPrint <$>)
           
-- Function application in AST context
apply :: AST -> [AST] -> AST
apply (Sym s) l = (lookup s preludeFunctions) <*> pure l
                  |>> fromMaybe Err
apply x [] = x
apply _ _ = Err

-- Evaluate the value of an AST
eval :: AST -> AST
eval (Nod ast list) = apply (eval ast) (map eval list)
eval x = x

lispEval :: String -> Maybe AST
lispEval = parseUnique lispParser |-> liftM eval