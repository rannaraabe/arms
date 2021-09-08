module TabelaSimbolos where

import Lexer
import Text.Parsec
import GHC.Float

type Escopo = String
type Valor = Token
type Nome = Token
type Retorno = Token
type Variavel = (Escopo, Nome, Valor)
type SubPrograma = (Nome, [Variavel], Retorno, [Token])
type Estado = ([Escopo], [Variavel], [SubPrograma], Bool)


cellStr :: Variavel -> String
cellStr (escopo, name, val ) = escopo ++ " " ++ show name ++ (show val)

symTableInsertVariable :: Variavel -> Estado -> Estado
symTableInsertVariable c (es, vs, s, ex) = (es, insertVariable c vs, s, ex)

findSubProgram :: Nome -> [SubPrograma] -> [Token]
findSubProgram n ((name, v, r, pc):xs) 
    | n == name = pc
    | otherwise = findSubProgram n xs
findSubProgram n [] = error "Erro - Função não declarada."

findSubProgramArgs :: Nome -> [SubPrograma] -> [Variavel]
findSubProgramArgs n ((name, v, r, pc):xs) 
    | n == name = v
    | otherwise = findSubProgramArgs n xs
findSubProgramArgs n [] = error "Erro - Função não declarada."

getFunctionStart :: Nome -> Estado -> [Token]
getFunctionStart name (e, v, s, _) = findSubProgram name s 

getFunctionArgs :: Nome -> Estado -> [Variavel]
getFunctionArgs name (e, v, s, _) = findSubProgramArgs name s

insertMultVariables :: [Variavel] -> Estado -> Estado
insertMultVariables vs (e, v, s, ex) =  (e , vs ++ v, s, ex)

pushScope :: Escopo -> Estado -> Estado
pushScope e (es, v, s, ex) = (e:es, v, s, ex)

popScope :: Estado -> Estado
popScope (e:es, v, s, ex) = (es, dropEsc e v, s, ex)

dropEsc :: Escopo -> [Variavel] -> [Variavel]
dropEsc _ [] = []
dropEsc e ((es, name, value):vs)
    | e == es = dropEsc e vs
    | otherwise = (es, name, value) : dropEsc e vs

isExecuting :: Estado -> Bool
isExecuting (es, vs, s, ex) = ex

symTableInsertSubProgram :: SubPrograma -> Estado -> Estado
symTableInsertSubProgram sp (es, vs, s, f) = (es, vs, insertSub sp s, f)

symTableUpdateVariable :: Variavel -> Estado -> Estado
symTableUpdateVariable (_, name,value) (es, vs, s, ex) = (es, updateVariable es (name, value) vs, s, ex)

turnOnExecution :: Estado -> Estado
turnOnExecution (es, vs, s, _) = (es, vs, s, True)

turnOffExecution :: Estado -> Estado
turnOffExecution (es, vs, s, _) = (es, vs, s, False)

updateVariable :: [Escopo] -> (Nome, Valor) -> [Variavel] -> [Variavel]
updateVariable es (name, value) [] = error ("Erro - Variável " ++ show name ++ " não declarada.")
updateVariable es (Identifier p n, v) ((esc1, Identifier p1 n1, v1):symbs)
    | esc1 `elem` es && n == n1 = ((esc1, Identifier p n, v):symbs)
    | otherwise = (esc1, Identifier p1 n1, v1): updateVariable es (Identifier p n, v) symbs

insertVariable :: Variavel -> [Variavel] -> [Variavel]
insertVariable c [] = [c]
insertVariable (esc, Identifier p n, v) ((esc1, Identifier p1 n1, v1):symbs)
    | esc == esc1 && n == n1 = error $ "Erro - Variável " ++ cellStr (esc, Identifier p n, v) ++ " já existe."
    | otherwise = (esc1, Identifier p1 n1, v1): insertVariable (esc, Identifier p n, v) symbs

symTableGetValue :: Token -> [Variavel] -> Token
symTableGetValue t [] = error $ "Erro - Variável "++ show t ++" não declarada."
symTableGetValue (x) ((_, n, v ):xs) =
    if n == x
     then v
     else symTableGetValue (x) xs

symTableGetValueArray :: Token -> Int -> [Variavel] -> Token
symTableGetValueArray t _ [] = error $ "Erro - Variável "++ show t ++" não declarada."
symTableGetValueArray (x) (index) ((_, n, v ):xs) =
    if n == x
     then case v of 
        Array p x -> 
          if index >= length x || index < 0
            then error $ "Erro - Índice " ++ show index ++ " fora dos limites."
          else x !! (fromIntegral index)
        _ -> symTableGetValueArray (x) (index) xs
    else symTableGetValueArray (x) (index) xs

symTableGetValueMatrix :: Token -> Int -> Int -> [Variavel] -> Token
symTableGetValueMatrix t _ _ [] = error $ "Erro - Variável "++ show t ++" não declarada."
symTableGetValueMatrix (x) (index1) (index2) ((_, n, v ):xs) =
    if n == x
     then case v of 
        Matrix p x -> 
          if index1 >= length x || index1 < 0
            then error $ "Erro - Índice " ++ show index1 ++ " fora dos limites."
          else 
            if index2 >= length (x !! (fromIntegral index1)) || index2 < 0
              then error $ "Erro - Índice " ++ show index2 ++ " fora dos limites."
            else (x !! (fromIntegral index1)) !! index2
        _ -> symTableGetValueMatrix (x) (index1) (index2) xs
    else symTableGetValueMatrix (x) (index1) (index2) xs

insertSub :: SubPrograma -> [SubPrograma] -> [SubPrograma]
insertSub (n, vs, r, pc) [] = [(n, vs, r, pc)]
insertSub (n, vs, r, pc) ((n1, vs1, r1, pc1):sps)
    | n == n1 && r1 == r = error "Erro - Função já foi declarada."
    | otherwise = (n1, vs1, r1, pc1) : (insertSub (n, vs, r, pc) sps)
    

getDefaulValue :: Token -> Valor
getDefaulValue (IntType p) = Int (AlexPn 1 1 1) 0
getDefaulValue (DoubleType p) = Double (AlexPn 1 1 1) 0
getDefaulValue (ComplexType p) = Complex (AlexPn 1 1 1) (Double (AlexPn 1 1 1) 0, Double (AlexPn 1 1 1) 0)
getDefaulValue (ArrayType p) = Array (AlexPn 1 1 1) []
getDefaulValue (MatrixType p) = Array (AlexPn 1 1 1) []
getDefaulValue (BooleanType p) = TrueSym (AlexPn 1 1 1)
getDefaulValue (CharType p) = String (AlexPn 1 1 1) ""
getDefaulValue (StringType p) = String (AlexPn 1 1 1) ""

typeTokenCompatible :: Token -> Token -> Bool
typeTokenCompatible (IntType _) (Int _ _)  = True
typeTokenCompatible (DoubleType _) (Double _ _)  = True
typeTokenCompatible (StringType _) (String _ _)  = True
typeTokenCompatible (BooleanType _) (Boolean _ _)  = True
typeTokenCompatible (CharType _) (Char _ _)  = True
typeTokenCompatible a b  = error $ "Erro - Valor " ++ show b ++ " imcompatível com o tipo " ++ show a ++ "." 


typeCompatible :: Valor -> Valor -> Valor
typeCompatible (Int p x )        (Int _ y)        = Int p x
typeCompatible (Double p x )     (Double _ y)        = Double p x
typeCompatible (Int p x )        (Double _ y)        = Double p (fromIntegral x)
typeCompatible (Double p x )     (Int _ y)      = Int p (double2Int x)
typeCompatible (TrueSym p   )    (TrueSym _  )  = TrueSym p
typeCompatible (FalseSym p   )   _  = FalseSym p 
typeCompatible (Complex p x )    (Complex _ _)   = Complex p x
typeCompatible (String p x )        (String _ _)       = String p x
typeCompatible (Array p x)    (Array _ _)  = Array p x
typeCompatible (Matrix p x)   (Matrix _ _) = Matrix p x
typeCompatible b a = error $ "Erro - Tipos incompatíves: " ++ show b ++ " e " ++ show a


instance Eq Token where
  IntType _            == IntType _           = True
  DoubleType _         == DoubleType _        = True
  ComplexType _        == ComplexType _       = True
  ArrayType _          == ArrayType _      = True
  MatrixType _         == MatrixType _     = True
  BooleanType _        == BooleanType _       = True
  CharType _           == CharType _          = True
  In  _                == In  _               = True
  For _                == For _               = True
  If _                 == If _                = True
  Else _               == Else _              = True
  Return _             == Return _            = True
  Break _              == Break _             = True
  Continue _           == Continue _          = True
  SemiColon _          == SemiColon _         = True
  OpenParenthese _     == OpenParenthese _    = True
  CloseParenthese _    == CloseParenthese _   = True
  OpenBracket _        == OpenBracket _       = True
  CloseBracket _       == CloseBracket _      = True
  OpenBracer _         == OpenBracer _        = True
  CloseBracer _        == CloseBracer _       = True
  Colon _              == Colon _             = True
  While _              == While _             = True
  Main _               == Main _              = True
  Print _              == Print _             = True
  Read _               == Read _              = True
  Comma _              == Comma _             = True
  Extraction _         == Extraction _        = True
  Insertion _          == Insertion _         = True
  Assign _             == Assign _            = True
  Int _ x              == Int _ y             = x == y
  Double _ x           == Double _ y          = x == y
  SymArith _ x         == SymArith _ y        = x == y
  Sym _ s              == Sym _ c             = s == c
  TrueSym _            == TrueSym _           = True
  FalseSym _           == FalseSym _          = True
  BoolOP _ g           == BoolOP _ s     = g == s
  RelOP _ g            == RelOP _ s      = g == s
  Complex _ x          == Complex _ y = x == y
  String _ g                == String _ s          = g == s
  StringType _           == StringType _          = True
  Array _ xs     == Array _ ys    = xs == ys
  Matrix _ xs  == Matrix _ ys = xs == ys
  Identifier _ g == Identifier _ s= g == s
  _ == _ = False