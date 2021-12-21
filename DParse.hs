module DParser
(
    item,
    oneOf1,
    satisfy,
    combine,
    failure,
    option,
    char,
    string,
    token,
    reserved,
    spaces,
    upper,
    lower,
    alphanum,
    digit,
    letter,
    seqStr,
    sepBy1,
    coma,
    parens,
    Parser,
    parse,
    taketoken,
    takerest
)
where
    
import Data.Char
import Control.Monad
import Control.Applicative
import Data.List.Split
import Data.List
--Basic parser lib with primitive definitions

{-The main type Parser a / a:(arbitrary type)
*A function working on an input string 
*Returns [] when parsing fails
*Successful parses generate a single pair with (parsedElem,remainingString)
-}
newtype Parser a = Parser { parse :: String -> [(a,String)] }
--Basic bind for parsers
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser (\s -> concatMap (\(a, s') -> parse (f a) s')  (parse p s))
--Unit mainly used for type convert (a->Parser a)
unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])
--taketoken , useful to apply parser to a string and just return the parsed value
taketoken::Parser a->String->a
taketoken p str = (fst (head (parse p str)))
--takerest , ignores the token and just returns whats left of a string after parsing
takerest::Parser a->String ->String
takerest p str = (snd (head (parse p str)))

{-
Parsers are instances of certain typeclasses
-}
instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

--Single Char parsing
item :: Parser Char
item = Parser (\s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)])

--Name not to be confused with Data.List.Split functions
--oneOf1 takes a string [Char] and parses a single char that is from the string
oneOf1 :: [Char] -> Parser Char
oneOf1 s = satisfy (flip elem s)
--Satisfy creates parser of char, only if that char satisfies the predicate p
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = bind item (\c ->
  if p c
  then unit c
  else (Parser (\cs -> [])))
--combines two parsers
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)
--fails on any input
failure :: Parser a
failure = Parser (\cs -> [])
-- if can parse p then parse p if can parse q parse q
option :: Parser a -> Parser a -> Parser a
option  p q = Parser ( \s ->
  case parse p s of
    []     -> parse q s
    res    -> res )
--like item, but can isolate char as param
char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

--token space eater
token :: Parser a -> Parser a
token p = do 
  { 
    spaces ;
    a <- p; 
    spaces ;
    return a}

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many (oneOf1 " \n\r")

digit :: Parser Char
digit = satisfy (\x->x<='9' && x>='0')

lower::Parser Char
lower = satisfy (\x -> x<='z' && x>='a')

upper::Parser Char
upper = satisfy (\x -> x<='Z' && x>='A')

letter::Parser Char
letter = combine lower upper

alphanum::Parser Char
alphanum =  letter <|> digit

seqStr::Parser String -> Parser String-> Parser String
seqStr p q = bind p (\x-> bind q (\xs-> unit (x++xs)))

sepBy1::Parser a->Parser b->Parser [b]
sepBy1 sep element = (:) <$> element <*> many (sep *> element) <|> pure []

coma::Parser String
coma = reserved ","
 
parens :: Parser String -> Parser String
parens m = do
  par1<-reserved "("
  n <- m
  par2<-reserved ")"
  return (par1++n++par2)