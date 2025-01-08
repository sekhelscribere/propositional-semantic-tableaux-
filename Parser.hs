module PropParser where

import Control.Applicative
import Learn

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser x) = Parser $ \s -> do
        (x', s') <- x s
        return (f x', s') 
    (<$) a (Parser b) = Parser $ \s ->  Just (a, " ")

instance Applicative Parser where
    pure a = Parser $ \x -> Just (a, x)

    (Parser f) <*> (Parser x) = Parser $ \s -> do
        (f', s') <- f s 
        (x', s'') <- x s'
        return  (f' x', s'') 

instance Monad Parser where
    (Parser a) >>= f = Parser $ \s -> do
        (x, y) <- a s
        runParser (f x) y
        
instance MonadFail Parser where
    fail s = Parser $ \x -> Nothing 

instance Alternative Parser where
    empty  = fail ""
    (Parser x) <|> (Parser y) = Parser $ \s ->
        case x s of
            Just x -> Just x
            Nothing -> y s


allowedProps = ['p', 'q', 'r', 's', 't']

propsToInt :: Char -> Integer
propsToInt 'p' = 1
propsToInt 'q' = 2
propsToInt 'r' = 3
propsToInt 's' = 4
propsToInt 't' = 5

char :: Char -> Parser Char
char c = Parser $ \x -> 
    case x of
        (x:xs) | x==c -> Just (x, xs)
        _             -> Nothing

string :: String -> Parser String
string "" = Parser $ \s -> Just ("",s)
string (x:xs) = Parser $ \s -> do
    (s, ys) <- runParser (char x) s
    (s', ys') <- runParser (string xs) ys
    return (x:xs, ys')

prop :: Parser Form
prop = Parser $ \s -> 
    case s of
        (x:xs) | x `elem` allowedProps -> Just ((P (propsToInt x)), xs)
        _                              -> Nothing

space :: Parser Char
space = char ' '

deleteWS = many space


token :: Parser a -> Parser a
token p = Parser $ \s -> do
     (x, xs) <- runParser deleteWS s
     (x', xs') <- runParser p xs
     (y, ys) <- runParser deleteWS xs'
     return (x', ys)



atom :: Parser Form
atom = (token prop) <|>  (token neg) <|> (token parens) 

neg :: Parser Form
neg = Parser $ \s -> 
    case s of
        (x:xs) | x=='~' -> (runParser atom xs) >>= (\(y, ys) ->  Just ((Neg y), ys))
        _               -> Nothing

parens :: Parser Form
parens = do
    token $ char '('
    p <- token expr
    token $ char ')'
    return p


conj :: Parser (Form -> Form -> Form) 
conj = Parser $ \s ->
     case s of
        (x:xs) | x=='&' ->  Just (Conj, xs)
        _               -> Nothing
        

disj :: Parser (Form-> Form -> Form)
disj = Parser $ \s ->
    case s of
        (x:xs)|x=='|' -> Just (Disj, xs)
        _             -> Nothing



binaryOp :: Parser (Form -> Form -> Form) -> Parser Form -> Parser Form
binaryOp op next =  do
    p1 <- next 
    rest p1
  where
    rest p1 = (do
        f <- token op
        p2 <-  next 
        rest (f p1 p2))
        <|> return p1

 
    

expr :: Parser Form
expr = binaryOp disj (binaryOp conj atom)

parsing :: String -> Maybe Form
parsing s = runParser expr s >>= \(s, xs) -> Just s

