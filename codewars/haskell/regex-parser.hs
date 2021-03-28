module RegExpParser
       ( RegExp(..)
       , parseRegExp
       ) where
       
import Text.ParserCombinators.ReadP
import Data.Bool (bool)
import Control.Applicative

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any charater
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
  deriving (Show, Eq)

(.:) = (.).(.)

pipe :: ReadP Char
pipe = char '|'

star :: ReadP Char
star = char '*'

period :: ReadP Char
period = char '.'

openParen :: ReadP Char
openParen = char '('

closeParen :: ReadP Char
closeParen = char ')'

normalChar :: ReadP RegExp
normalChar = (Normal) <$> satisfy (`notElem` ".()*|")

betweenParen :: ReadP a -> ReadP a
betweenParen = between openParen closeParen

normalOrAny :: ReadP RegExp
normalOrAny = ((const Any) <$> period) <|> normalChar
            
starQualified :: ReadP RegExp -> ReadP RegExp
starQualified p = do c <- p
                     star 
                     return (ZeroOrMore c)
        
starChar :: ReadP RegExp
starChar = starQualified normalOrAny 

qualifiedChar :: ReadP RegExp
qualifiedChar = normalOrAny <|> starChar <|> betweenParen qualifiedChar

subString :: ReadP RegExp
subString = raise <$> many1 qualifiedChar
  where
    raise (x:[]) = x
    raise xs = Str xs

qualifiedString :: ReadP RegExp
qualifiedString = subString <|> starQualified (betweenParen subString) <|> betweenParen qualifiedString

orExpression :: ReadP RegExp
orExpression = do left <- qualifiedString <|> orExpressionBetweenParen
                  pipe
                  right <- stringOfExpression <++ validExpression
                  return (Or left right)

orExpressionBetweenParen :: ReadP RegExp
orExpressionBetweenParen = betweenParen orExpression <|> starQualified (betweenParen orExpression) <|> betweenParen orExpressionBetweenParen
                  
qualifiedOrExpression :: ReadP RegExp
qualifiedOrExpression = orExpression <++ starQualified (betweenParen orExpression) <++ betweenParen qualifiedOrExpression


validExpression :: ReadP RegExp
validExpression = qualifiedOrExpression <++ qualifiedString <++ betweenParen validExpression

stringOfExpression :: ReadP RegExp
stringOfExpression = (Str) <$> do first <- orExpressionBetweenParen <++ qualifiedChar
                                  rest <- exprList
                                  return (first:rest)
  where
    exprList = do first <- orExpressionBetweenParen <++ qualifiedChar
                  rest <- exprList <|> return []
                  return (first:rest)

regex :: ReadP RegExp
regex = validExpression <|> stringOfExpression

readPToMaybe :: ReadP a -> String -> Maybe a
readPToMaybe = (null >>= bool 
                    ((null.snd >>= bool 
                               (const Nothing)
                               (return.fst))
                               .last)
                    (const Nothing)) 
               .: readP_to_S

parseRegExp :: String -> Maybe RegExp
parseRegExp = readPToMaybe regex