module TabelaSimbolos where

import Lexer
import Text.Parsec
import GHC.Float

type Escopo = String
type Valor = Token
type Nome = Token
type Retorno = Token
type Variavel = (Escopo, Nome, Valor)
type Tipo = (String, [(Token, Token)])
type SubPrograma = (Nome, [Variavel], Retorno, [Token])
type Estado = ([Escopo], [Variavel], [SubPrograma], [Tipo], Bool)


cellStr :: Variavel -> String
cellStr (escopo, name, val ) = escopo ++ " " ++ show name ++ (show val)

symTableInsertVariable :: Variavel -> Estado -> Estado
symTableInsertVariable c (es, vs, s, ts,ex) = (es, insertVariable c vs, s, ts, ex)

findSubProgram :: Nome -> [SubPrograma] -> [Token]
findSubProgram n ((name, v, r, pc):xs) 
    | n == name = pc
    | otherwise = findSubProgram n xs
findSubProgram n [] = error "funcao não declarada"

findSubProgramArgs :: Nome -> [SubPrograma] -> [Variavel]
findSubProgramArgs n ((name, v, r, pc):xs) 
    | n == name = v
    | otherwise = findSubProgramArgs n xs
findSubProgramArgs n [] = error "funcao não declarada"

getFunctionStart :: Nome -> Estado -> [Token]
getFunctionStart name (e, v, s, ts, _) = findSubProgram name s 

getFunctionArgs :: Nome -> Estado -> [Variavel]
getFunctionArgs name (e, v, s,ts, _) = findSubProgramArgs name s

insertMultVariables :: [Variavel] -> Estado -> Estado
insertMultVariables vs (e, v, s, ts,ex) =  (e , vs ++ v, s,ts, ex)

pushScope :: Escopo -> Estado -> Estado
pushScope e (es, v, s, ts, ex) = (e:es, v, s, ts, ex)

popScope :: Estado -> Estado
popScope (e:es, v, s, ts, ex) = (es, dropEsc e v, s, ts, ex)

topScope :: Estado -> Escopo
topScope (e:es, v, s, ts, ex) = e


dropEsc :: Escopo -> [Variavel] -> [Variavel]
dropEsc _ [] = []
dropEsc e ((es, name, value):vs)
    | e == es = dropEsc e vs
    | otherwise = (es, name, value) : dropEsc e vs

isExecuting :: Estado -> Bool
isExecuting (es, vs, s, ts, ex) = ex

symTableInsertSubProgram :: SubPrograma -> Estado -> Estado
symTableInsertSubProgram sp (es, vs, s, ts, f) = (es, vs, insertSub sp s, ts, f)

symTableUpdateVariable :: Variavel -> Estado -> Estado
symTableUpdateVariable (_, name,value) (es, vs, s, ts, ex) = (es, updateVariable es (name, value) vs, s, ts, ex)

symTableUpdateArrayElem :: Variavel -> Int -> Estado -> Estado
symTableUpdateArrayElem (_, name,value) index (es, vs, s, ts, ex) = (es, updateArrayElem es (name, value, index) vs, s, ts, ex)

symTableUpdateMatrixElem :: Variavel -> Int -> Int -> Estado -> Estado
symTableUpdateMatrixElem (_, name,value) index1 index2 (es, vs, s, ts, ex) = (es, updateMatrixElem es (name, value, index1, index2) vs, s, ts, ex)

turnOnExecution :: Estado -> Estado
turnOnExecution (es, vs, s, ts, _) = (es, vs, s, ts, True)

setExecution :: Bool -> Estado -> Estado
setExecution t (es, vs, s, ts, _) = (es, vs, s, ts, t)

turnOffExecution :: Estado -> Estado
turnOffExecution (es, vs, s, ts, _) = (es, vs, s, ts, False)

updateVariable :: [Escopo] -> (Nome, Valor) -> [Variavel] -> [Variavel]
updateVariable es (name, value) [] = error ("Variavel " ++ show name ++ " nao declarada")
updateVariable es (Identifier p n, v) ((esc1, Identifier p1 n1, v1):symbs)
    | esc1 `elem` es && n == n1 = ((esc1, Identifier p n, v):symbs)
    | otherwise = (esc1, Identifier p1 n1, v1): updateVariable es (Identifier p n, v) symbs

updateArrayElem :: [Escopo] -> (Nome, Valor, Int) -> [Variavel] -> [Variavel]
updateArrayElem es (name, value, index) [] = error ("Array " ++ show name ++ " nao declarado")
updateArrayElem es (Identifier p n, v, index)           ((esc1, Identifier p1 n1, (Array _ v1)):symbs)
    | index >= length v1 = error $ "Indice " ++ show index ++ " fora dos limites"
    | esc1 `elem` es && n == n1 = ((esc1, Identifier p n, Array (AlexPn 1 1 1) (take index v1 ++ v : drop (index+1) v1)):symbs)
    | otherwise = (esc1, Identifier p1 n1, (Array (AlexPn 1 1 1) v1)): updateArrayElem es (Identifier p n, v, index) symbs

updateMatrixElem :: [Escopo] -> (Nome, Valor, Int, Int) -> [Variavel] -> [Variavel]
updateMatrixElem es (name, value, index1, index2) [] = error ("Matrix " ++ show name ++ " nao declarada")
updateMatrixElem es (Identifier p n, v, index1, index2) ((esc1, Identifier p1 n1, (Array _ v1)):symbs) = symbs


    -- | index1 < 0 || index2 < 0 = error $ "Indices nao podem ser negativos"
    -- | index1 >= length v1 = error $ "Indice " ++ show index1 ++ " fora dos limites"
    -- | index2 >= length (v1 !! index1) = error $ "Indice " ++ show index2 ++ " fora dos limites"
    -- | esc1 `elem` es && n == n1 = ((esc1, Identifier p n, Matrix (AlexPn 1 1 1) ((take index1 v1) ++ [take index2 (v1 !! index1) ++ v : drop (index2 + 1) (v1 !! index1)] ++ (drop (index1 + 1) v1))):symbs)
    -- | otherwise = (esc1, Identifier p1 n1, (Matrix (AlexPn 1 1 1) v1)): updateMatrixElem es (Identifier p n, v, index1, index2) symbs


insertVariable :: Variavel -> [Variavel] -> [Variavel]
insertVariable c [] = [c]
insertVariable (esc, Identifier p n, v) ((esc1, Identifier p1 n1, v1):symbs)
    | esc == esc1 && n == n1 = error $ "Variavel " ++ cellStr (esc, Identifier p n, v) ++ " já exite"
    | otherwise = (esc1, Identifier p1 n1, v1): insertVariable (esc, Identifier p n, v) symbs

symTableGetValue :: Token -> [Variavel] -> (Maybe Token)
symTableGetValue t [] = Nothing
symTableGetValue (x) ((_, n, v ):xs) =
    if n == x
     then Just  v
     else symTableGetValue (x) xs

symTableGetValueArray :: Token -> Int -> [Variavel] -> Token
symTableGetValueArray t _ [] = error $ "Variavel "++ show t ++" nao declarada"
symTableGetValueArray (x) (index) ((_, n, v ):xs) =
    if n == x
     then case v of 
        Array p x -> 
          if index >= length x || index < 0
            then error $ "Indice " ++ show index ++ " fora dos limites"
          else x !! (fromIntegral index)
        _ -> symTableGetValueArray (x) (index) xs
    else symTableGetValueArray (x) (index) xs

symTableGetValueMatrix :: Token -> Int -> Int -> [Variavel] -> Token
symTableGetValueMatrix t _ _ [] = error $ "Variavel "++ show t ++" nao declarada"
symTableGetValueMatrix (x) (index1) (index2) ((_, n, v ):xs) =
    if n == x
     then case v of 
        Matrix p x -> 
          if index1 >= length x || index1 < 0
            then error $ "Indice " ++ show index1 ++ " fora dos limites"
          else 
            if index2 >= length (x !! (fromIntegral index1)) || index2 < 0
              then error $ "Indice " ++ show index2 ++ " fora dos limites"
            else (x !! (fromIntegral index1)) !! index2
        _ -> symTableGetValueMatrix (x) (index1) (index2) xs
    else symTableGetValueMatrix (x) (index1) (index2) xs

symTableInsertType :: Tipo -> Estado -> Estado
symTableInsertType t (es, vs, sps, ts, ex) =
  (es, vs, sps, insertType ts t, ex)

getType :: String -> [Tipo] -> Tipo
getType s ((nome, atb):xs) 
  | s == nome = (nome, atb)
  | otherwise = getType s xs
getType s [] = error $ "Tipo "++ s ++ " nao declarado"

insertType :: [Tipo] -> Tipo -> [Tipo]
insertType ts t = if foldr (&&) True $ map (==t) ts then
                      t:ts
                  else error $ "Struct " ++ fst t ++ " ja existe"

findType :: String -> [(String, [(Token, Token)])] -> Bool
findType s = (foldr (&&) True) . map ((==s) . fst)


insertSub :: SubPrograma -> [SubPrograma] -> [SubPrograma]
insertSub (n, vs, r, pc) [] = [(n, vs, r, pc)]
insertSub (n, vs, r, pc) ((n1, vs1, r1, pc1):sps)
    | n == n1 && r1 == r = error "Funcao ja declarada"
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

getDefaultArray :: Token -> Int -> [Valor]
getDefaultArray (IntType p) size = map (const (Int (AlexPn 1 1 1) 0)) [1 .. size]
getDefaultArray (DoubleType p) size = map (const (Double (AlexPn 1 1 1) 0)) [1 .. size]
getDefaultArray (BooleanType p) size = map (const (TrueSym (AlexPn 1 1 1))) [1 .. size] 
getDefaultArray (CharType p) size = map (const (String (AlexPn 1 1 1) "")) [1 .. size] 
getDefaultArray (StringType p) size = map (const (String (AlexPn 1 1 1) "")) [1 .. size] 

getDefaultMatrix :: Token -> Int -> Int -> [[Valor]]
getDefaultMatrix (IntType p) size1 size2 = map (const (map (const (Int (AlexPn 1 1 1) 0)) [1 .. size2])) [1 .. size1]
getDefaultMatrix (DoubleType p) size1 size2 = map (const (map (const (Double (AlexPn 1 1 1) 0)) [1 .. size2])) [1 .. size1]
getDefaultMatrix (BooleanType p) size1 size2 = map (const (map (const (TrueSym (AlexPn 1 1 1))) [1 .. size2])) [1 .. size1]
getDefaultMatrix (CharType p) size1 size2 = map (const (map (const (String (AlexPn 1 1 1) "")) [1 .. size2])) [1 .. size1]
getDefaultMatrix (StringType p) size1 size2 = map (const (map (const (String (AlexPn 1 1 1) "")) [1 .. size2])) [1 .. size1]


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
typeCompatible b a = error $ "tipos incompatives: " ++ show b ++ " e " ++ show a


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