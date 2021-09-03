module Eval where
import Lexer
import TabelaSimbolos
import Tokens
import Text.Parsec



eval :: Token -> Token -> Token -> Token
eval (String p a) (Sym _ "+") (String _ b) = String p (a ++ b)
eval (Int _ x) (Sym p "+") (Int _ y) = Int p (x + y)
eval (Int _ x) (Sym p "+") (Double _ y) = Double p (fromIntegral x + y)
eval (Double _ y) (Sym p "+") (Int _ x) = Double p (fromIntegral x + y)
eval (Double _ y) (Sym p "+") (Double _ x) = Double p (x + y)
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
eval (Double _ y) (Sym p "-") (Int _ x) = Double p (fromIntegral x - y)
eval (Double _ y) (Sym p "-") (Double _ x) = Double p (x - y)
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