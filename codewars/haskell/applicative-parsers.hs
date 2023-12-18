module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)
import Data.List (isPrefixOf)
import Data.Char (isDigit)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P (\s -> map (\(rest, val) -> (rest, f val)) $ unP p s)

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) a b = (const a) <#> b

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P (\s -> if null s then [] else if p (head s) then [(tail s, head s)] else [])

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = predP (== c)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P (\s -> [(s, x)])

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P (\s -> let r = unP pf s in
                       concatMap (\(rest, fun) -> let r' = unP px rest in
                                                    map (\(rest', val) -> (rest', fun val)) r') r)

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = P (\s -> let r = unP pa s in
                      concatMap (\(rest, val) -> let r' = unP pb rest in
                                                   map (\(rest', _) -> (rest', val)) r') r)

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = P (\s -> let r = unP pa s in
                      concatMap (\(rest, _) -> unP pb rest) r)

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP pref = P (\s -> if isPrefixOf pref s then
                          [(drop (length pref) s, pref)]
                        else
                          [])

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P (\_ -> [])

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
p <<>> p' = P (\s -> (unP p s) ++ (unP p' s))

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = P (\s -> let result = unP p s in
                    if null result then
                      [(s, [])]
                    else
                      concatMap (\(rest, val) -> map (\(rest', val') -> (rest', (val:val'))) $ unP (many p) rest) result)

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = P (\s -> let result = unP p s in
                      concatMap (\(rest, val) -> map (\(rest', val') -> (rest', (val:val'))) $ unP (many p) rest) result)


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = map snd $ filter (null . fst) $ unP p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = let result = runParser p cs in
                          if length result == 1 then
                            Just (head result)
                          else
                            Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE x) = x
evalExpr ZeroE = 0
evalExpr (NegE expr) = negate $ evalExpr expr
evalExpr (BinOpE op expr1 expr2) = evalOp op (evalExpr expr1) (evalExpr expr2)
  where 
    evalOp :: BinOp -> Int -> Int -> Int
    evalOp AddBO = (+)
    evalOp MulBO = (*)

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 

digit :: Parser Char
digit = predP isDigit

constant :: Parser Expr
constant = ((ConstE) . read) <#> some digit

zeroE :: Parser Expr
zeroE = ZeroE <# charP 'z'

binOpExpr :: Parser Expr
binOpExpr = charP '(' @> ((flip (BinOpE)) <#> expr <@> (charP ' ' @>  binOp <@ charP ' ') <@> expr ) <@ charP ')'

binOp :: Parser BinOp
binOp = (AddBO <# charP '+') <<>> (MulBO <# charP '*')

negE :: Parser Expr
negE = (NegE) <#> (charP '-' @> expr) 

expr :: Parser Expr
expr = constant <<>> binOpExpr <<>> negE <<>> zeroE

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique expr