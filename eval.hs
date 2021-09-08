module Eval where
import Lexer
import TabelaSimbolos
import Tokens
import Text.Parsec

eval :: Token -> Token -> Token -> Token
eval (Int _ x) (Sym p "%") (Int _ y) = Int p (x `mod` y)
eval (String p a) (Sym _ "+") (String _ b) = String p (a ++ b)
eval (Int _ x) (Sym p "+") (Int _ y) = Int p (x + y)
eval (Int _ x) (Sym p "+") (Double _ y) = Double p (fromIntegral x + y)
eval (Double _ y) (Sym p "+") (Int _ x) = Double p (fromIntegral x + y)
eval (Double _ y) (Sym p "+") (Double _ x) = Double p (x + y)
-- eval (Array _ y) (Sym p "+") (Array _ x) = Array p ((zipWith (+)) x y)
-- eval (Matrix _ y) (Sym p "+") (Matrix _ x) = Matrix p (zipWith (zipWith (+)) x y)
eval (Int _ x) (Sym p "*") (Int _ y) = Int p (x * y)
eval (Int _ x) (Sym p "*") (Double _ y) = Double p (fromIntegral x * y)
eval (Double _ y) (Sym p "*") (Int _ x) = Double p (fromIntegral x * y)
eval (Double _ y) (Sym p "*") (Double _ x) = Double p (x * y)
eval (Int _ x) (Sym p "/") (Int _ y) = Int p (x `div` y)
eval (Int _ x) (Sym p "/") (Double _ y) = Double p (fromIntegral x / y)
eval (Double _ y) (Sym p "/") (Int _ x) = Double p (y / fromIntegral x)
eval (Double _ y) (Sym p "/") (Double _ x) = Double p (x / y)
eval (Int _ x) (Sym p "**") (Int _ y) = Int p (x ^ y)
eval (Int _ x) (Sym p "**") (Double _ y) = Double p (fromIntegral x ** y)
eval (Double _ y) (Sym p "**") (Int _ x) = Double p (y ** fromIntegral x)
eval (Double _ y) (Sym p "**") (Double _ x) = Double p (x ** y)
eval (Int _ x) (Sym p "-") (Int _ y) = Int p (x - y)
eval (Int _ x) (Sym p "-") (Double _ y) = Double p (fromIntegral x - y)
eval (Double _ x) (Sym p "-") (Int _ y) = Double p (x - fromIntegral y)
eval (Double _ x) (Sym p "-") (Double _ y) = Double p (x - y)
eval (TrueSym p) (Sym _ "and") (FalseSym _) = FalseSym p
eval (TrueSym p) (Sym _ "and") (TrueSym _) = TrueSym p
eval (FalseSym p) (Sym _ "and") _ = FalseSym p
eval (TrueSym p) (Sym _ "or") (TrueSym _) = TrueSym p
eval (TrueSym p) (Sym _ "or") (FalseSym _) = TrueSym p
eval (FalseSym p) (Sym _ "or") (TrueSym _) = TrueSym p
eval (FalseSym p) (Sym _ "or") (FalseSym _) = FalseSym p
eval (Int _ x)    (Sym p "<=") (Int _ y) = if x <= y then TrueSym p else FalseSym p
eval (Int _ x)    (Sym p ">=") (Int _ y) = if x >= y then TrueSym p else FalseSym p
eval (Int _ x)    (Sym p "==") (Int _ y) = if x == y then TrueSym p else FalseSym p
eval (Int _ x)    (Sym p "<") (Int _ y) = if x < y then TrueSym p else FalseSym p
eval (Int _ x)    (Sym p ">") (Int _ y) = if x > y then TrueSym p else FalseSym p
eval (Int _ x)    (Sym p "!=") (Int _ y) = if x /= y then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p "<=") (Double _ y) = if x <= y then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p ">=") (Double _ y) = if x >= y then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p "==") (Double _ y) = if x == y then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p ">") (Double _ y) = if x > y then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p "<") (Double _ y) = if x < y then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p "!=") (Double _ y) = if x /= y then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p "<=") (Int _ y) = if x <= (fromIntegral y) then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p ">=") (Int _ y) = if x >= (fromIntegral y) then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p "==") (Int _ y) = if x == (fromIntegral y) then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p ">") (Int _ y) = if x > (fromIntegral y) then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p "<") (Int _ y) = if x < (fromIntegral y) then TrueSym p else FalseSym p
eval (Double _ x)    (Sym p "!=") (Int _ y) = if x /= (fromIntegral y) then TrueSym p else FalseSym p
eval (Int _ x)    (Sym p "<=") (Double _ y) = if (fromIntegral x) <= (y) then TrueSym p else FalseSym p
eval (Int _ x)    (Sym p ">=") (Double _ y) = if (fromIntegral x) >= (y) then TrueSym p else FalseSym p
eval (Int _ x)    (Sym p "==") (Double _ y) = if (fromIntegral x) == (y) then TrueSym p else FalseSym p
eval (Int _ x)    (Sym p "<") (Double _ y) = if (fromIntegral x) < (y) then TrueSym p else FalseSym p
eval (Int _ x)    (Sym p ">") (Double _ y) = if (fromIntegral x) > (y) then TrueSym p else FalseSym p
eval (Int _ x)    (Sym p "!=") (Double _ y) = if (fromIntegral x) /= (y) then TrueSym p else FalseSym p
eval a _ b = error $ "type missmatch betwen values: "++  show a ++ " and "++ show b 

cast :: String -> Token -> Token
cast s (Int p _) = Int p (read s)
cast s (Double p _) = Double p (read s)
cast s (String p _) = String p s


precedence :: Token -> Int
precedence (Sym p "+") = 6
precedence (Sym p "*") = 7
precedence (Sym p "/") = 7
precedence (Sym p "-") = 6
precedence (Sym p "**") = 9
precedence (Sym p "and") = 3
precedence (Sym p "or") = 3
precedence (Sym p "<") = 5
precedence (Sym p ">") = 5
precedence (Sym p ">=") = 5
precedence (Sym p "<=") = 5
precedence (Sym p "==") = 5
precedence (Sym p "!=") = 5
precedence (Sym p "%") = 7

-- retorna se a precedencia de x é maior que a de y
(#<) :: Token -> Token -> Bool
x #< y = precedence x <= precedence y

isLeftAssociativeOperator :: Token -> Bool
isLeftAssociativeOperator (Sym p "**") = False
isLeftAssociativeOperator _ = True

isRightAssociativeOperator :: Token -> Bool
isRightAssociativeOperator (Sym p "**") = True
isRightAssociativeOperator _ = False


infixToPostFix :: [Token] -> [Token]
infixToPostFix tokens = shuntingYard tokens [] []

shuntingYard :: [Token] -> [Token] -> [Token] -> [Token]
shuntingYard [] [] outQueue = reverse outQueue
shuntingYard [] (op:ops) outQueue = shuntingYard [] ops (op:outQueue)
shuntingYard (token:tokens) opStack outQueue =
    case token of 
        (Sym p x) -> case opStack of
                [] -> shuntingYard tokens (token:opStack) outQueue
                (op2:ops) -> if ((isLeftAssociativeOperator token)
                                        && ((precedence token) <= (precedence op2)))
                                        || ((isRightAssociativeOperator token)
                                        && ((precedence token) < (precedence op2)))
                            then shuntingYard (token:tokens) ops (op2:outQueue)
                            else shuntingYard tokens (token:opStack) outQueue
        x -> shuntingYard tokens opStack (x:outQueue)

evalPostfix :: [Token] -> Token
evalPostfix ex = head (foldl func [] ex)
    where
          func (x:y:xs) (Sym p op) = (eval y (Sym p op) x):xs
          func xs digs = digs:xs

{-
foldl (b -> a -> b) -> b -> [a] -> b
foldl f b [] = b
foldl f b (a:as) = foldl f (f b a) as

foldl func [] [INT 0,INT 0,>=,INT 0,INT 25,<=,and] = 
foldl func (func [] INT 0 ) [INT 0,>=,INT 0,INT 25,<=,and] = 
foldl func (func [INT 0] INT 0 ) [>=,INT 0,INT 25,<=,and] = 
foldl func (func [INT 0,INT 0, >=] ) [INT 0,INT 25,<=,and] = 
foldl func (func [True] ) [INT 0,INT 25,<=,and] =
foldl func (func [True, INT 0] ) [INT 25,<=,and] =
foldl func (func [True, INT 0, INT 25] ) [<=,and] =
-}