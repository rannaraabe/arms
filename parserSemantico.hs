import Lexer
import TabelaSimbolos
import Text.Parsec
import ParserSintatico
import Tokens
import Eval
import Control.Monad.IO.Class
import System.IO.Unsafe
import System.Environment
import Text.Read (readMaybe)


initialState = (["global"], [], [], [], False)


parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program initialState "Error message" tokens

main :: IO ()
main = do
    xs <- getArgs
    case xs of
      [] -> print "nem um programa fornecido"
      x:_ -> do
        case unsafePerformIO (parser (getTokens x)) of
                Left err -> print err
                Right ans -> return ()

program :: ParsecT [Token] Estado IO [Token]
program = do
            a <- begin <|> return []
            b <- mainParser
            eof
            return $ a ++ b

begin :: ParsecT [Token] Estado IO [Token]
begin = do
          a <- try arrayDeclAss <|>
               try matrixDeclAss <|>
               try arrayDecl <|>
               try varDecl <|>
               structParser <|>
               funcDecl
          c <- semiColonToken
          b <- begin <|> return []
          return (a ++ [c] ++ b)

funcDecl :: ParsecT [Token] Estado IO [Token]
funcDecl  = do
            name <- identifierToken
            b <- openParentheseToken
            arg <- args name
            d <- closeParentheseToken
            e <- colonToken
            ret <- head <$> typeToken 
            inp <- getInput
            g <- subProgram
            updateState(symTableInsertSubProgram (name, arg, ret, inp))
            s <- getState
            --liftIO $ print s
            return g

structParser :: ParsecT [Token] Estado IO [Token]
structParser = do
  structToken
  Identifier p nome <- identifierToken
  openBracerToken
  atrb <- atribs
  closeBracerToken
  updateState $ symTableInsertType (nome, atrb)
  s <- getState
  return []

atribs :: ParsecT [Token] Estado IO [(Token, Token)]
atribs = do
  nome <- identifierToken
  colonToken
  typ <- head <$> typeToken
  let v = getDefaulValue typ
  semiColonToken
  rest <- atribs <|> return []
  return $ (nome, v):rest

varAss :: ParsecT [Token] Estado IO [Token]
varAss = do
    name <- identifierToken
    a <- assignToken
    val <- try typeConstructor <|> expression
    (e:es,_,_,_,_) <- getState
    updateState(symTableUpdateVariable (e, name, val))
    s <- getState
    -- liftIO (print s)
    return (name:a:[val])

varDecl :: ParsecT [Token] Estado IO [Token]
varDecl = do
          name <- identifierToken
          b <- assignToken
          expr <- try typeConstructor <|> expression
          s <- getState
          (e:_, _, _, _,_) <- getState
          o <- remainingDecls
          d <- colonToken
          g <- typeToken
          updateState(symTableInsertVariable (e, name, expr))
          return ((name:b:[expr]) ++ (d:g))

remainingDecls :: ParsecT [Token] Estado IO [Token]
remainingDecls = (do
          c <- commaToken
          name <- identifierToken
          a <- assignToken
          expr <- expression
          (e:_, _, _, _, _) <- getState
          updateState(symTableInsertVariable (e, name, expr))
          r <- remainingDecls
          return (c:name:a:expr:r)) <|> return []

expression :: ParsecT [Token] Estado IO Token
expression = do
    l <- literalIdentifier
    resto <- remainingExpressions
    let xs = infixToPostFix $ l:resto
    let res = evalPostfix xs
    return res


remainingExpressions :: ParsecT [Token] Estado IO [Token]
remainingExpressions = f <|> return []
  where f = do
              s <- symToken
              x <- literalIdentifier
              r <- remainingExpressions <|> return []
              return $ s:x:r

expressionPrev :: ParsecT [Token] Estado IO Token
expressionPrev = do
          a <- literalIdentifier
          result <- remainingExpressionsPrev a
          return (result)

remainingExpressionsPrev :: Token -> ParsecT [Token] Estado IO Token
remainingExpressionsPrev n1 = (do
                a <- symToken
                case a of
                  (Sym p "and") -> do
                          e <- expression
                          return $ eval n1 (Sym p "and") e
                  (Sym p "or") -> do
                          e <- expression
                          return $ eval n1 (Sym p "or") e
                  _ -> do
                    b <- literalIdentifier
                    c <- remainingExpressionsPrev $ eval n1 a b
                    return $ c) <|> return n1

arrayDecl :: ParsecT [Token] Estado IO [Token]
arrayDecl = do
            name <- identifierToken
            ob <- openBracketToken
            size <- expression
            let (Int p v) = typeCompatible size $ getDefaulValue (IntType (AlexPn 1 1 1))
            cb <- closeBracketToken
            cl <- colonToken
            ar <- arrayToken
            ap <- openParentheseToken
            t:_ <- typeToken
            cp <- closeParentheseToken
            (e:es, vs, _, _, _) <- getState
            updateState(symTableInsertVariable (e, name, Array (AlexPn 1 1 1) (getDefaultArray t v)))
            return ([name])

arrayDeclAss :: ParsecT [Token] Estado IO [Token]
arrayDeclAss = do
          name <- identifierToken
          ob <- openBracketToken
          size <- expression
          let (Int p v) = typeCompatible size $ getDefaulValue (IntType (AlexPn 1 1 1))
          cb <- closeBracketToken
          assignToken
          l <- literalArray
          if v /= length l then do
            error $ "O array " ++ show name ++ " tem tamanho " ++ show v ++ " e foram atribuidos " ++ show (length l) ++ " elementos"
            return []
          else do
            (e:es, vs, _, _, _) <- getState
            updateState(symTableInsertVariable (e, name, Array (AlexPn 1 1 1) l))
            cl <- colonToken
            ar <- arrayToken
            ap <- openParentheseToken
            t <- typeToken
            -- liftIO (print ("tipo " ++ (show t)))
            cp <- closeParentheseToken
            -- liftIO (print ("teste array" ++ (show l)))
            return (name:l ++ ar:ap:t ++ [cp])

arrayAss :: ParsecT [Token] Estado IO [Token]
arrayAss = do
    i <- identifierToken
    a <- assignToken
    l <- literalArray
    (e:es, vs, _, _, _) <- getState
    updateState(symTableUpdateVariable (e, i, Array (AlexPn 1 1 1) l))

    return (i:a:l)

literalArray ::ParsecT [Token] Estado IO [Token]
literalArray = do 
          ob <- openBracketToken
          first <- expression
          rest <- remainingArrays
          return (first:rest)

remainingArrays :: ParsecT [Token] Estado IO [Token]
remainingArrays = f <|> g
      where f = do
              comma <- commaToken
              i <- expression
              rest <- f <|> g
              return (i:rest)
            g = do
              c <- closeBracketToken
              return []

arrayAssElem :: ParsecT [Token] Estado IO [Token]
arrayAssElem = do
    i <- identifierToken
    ob <- openBracketToken
    index <- expression
    cb <- closeBracketToken

    let (Int p v_index) = typeCompatible index $ getDefaulValue (IntType (AlexPn 1 1 1))

    a <- assignToken
    exp <- expression


    (e:es, vs, _, _, _) <- getState

    updateState(symTableUpdateArrayElem (e, i, exp) v_index)
    s <- getState

    return ([])

matrixDecl :: ParsecT [Token] Estado IO [Token]
matrixDecl = do
          name <- identifierToken
          ob1 <- openBracketToken
          size1 <- expression
          let (Int p1 v1) = typeCompatible size1 $ getDefaulValue (IntType (AlexPn 1 1 1))
          cb1 <- closeBracketToken

          ob2 <- openBracketToken
          size2 <- expression
          let (Int p2 v2) = typeCompatible size2 $ getDefaulValue (IntType (AlexPn 1 1 1))
          cb2 <- closeBracketToken

          cl <- colonToken
          ar <- matrixToken
          ap <- openParentheseToken

          t:_ <- typeToken
          cp <- closeParentheseToken

          (e:es, vs, _, _, _) <- getState

          updateState(symTableInsertVariable (e, name, Matrix (AlexPn 1 1 1) (getDefaultMatrix t v1 v2)))

          -- liftIO $ print ("a")
          
          return ([name])

matrixDeclAss :: ParsecT [Token] Estado IO [Token]
matrixDeclAss = do

          name <- identifierToken
          ob1 <- openBracketToken
          size1 <- expression
          let (Int p1 v1) = typeCompatible size1 $ getDefaulValue (IntType (AlexPn 1 1 1))
          cb1 <- closeBracketToken

          ob2 <- openBracketToken
          size2 <- expression
          let (Int p2 v2) = typeCompatible size2 $ getDefaulValue (IntType (AlexPn 1 1 1))
          cb2 <- closeBracketToken

          assignToken

          l <- literalMatrix
          
          if v1 /= length l then do
            error $ "A matrix " ++ show name ++ " tem " ++ show v1 ++ " linhas e foram atribuidas " ++ show (length l) ++ " linhas"
            return []
          
          else do

          (e:es, vs, _, _, _) <- getState
          updateState(symTableInsertVariable (e, name, Matrix (AlexPn 1 1 1) l))
          cl <- colonToken
          ar <- matrixToken
          ap <- openParentheseToken
          t <- typeToken
          cp <- closeParentheseToken
          return (name:ar:ap:t ++ [cp])

matrixAss :: ParsecT [Token] Estado IO [Token]
matrixAss = do
    i <- identifierToken
    a <- assignToken
    l <- literalMatrix
    (e:es, vs, _, _, _) <- getState
    updateState(symTableUpdateVariable (e, i, Matrix (AlexPn 1 1 1) l))

    return (i:[a])

literalMatrix ::ParsecT [Token] Estado IO [[Token]]
literalMatrix = do 
          ob <- openBracketToken
          first <- literalArray
          rest <- remainingMatrixes

          return (first:rest)

remainingMatrixes :: ParsecT [Token] Estado IO [[Token]]
remainingMatrixes = f <|> g
      where f = do
              comma <- commaToken
              i <- literalArray
              rest <- f <|> g
              return (i:rest)
            g = do
              c <- closeBracketToken
              return []

matrixAssElem :: ParsecT [Token] Estado IO [Token]
matrixAssElem = do
    i <- identifierToken
    ob1 <- openBracketToken
    index1 <- expression
    cb1 <- closeBracketToken

    let (Int p v_index1) = typeCompatible index1 $ getDefaulValue (IntType (AlexPn 1 1 1))

    ob2 <- openBracketToken
    index2 <- expression
    cb2 <- closeBracketToken

    let (Int p v_index2) = typeCompatible index2 $ getDefaulValue (IntType (AlexPn 1 1 1))

    a <- assignToken
    exp <- expression


    (e:es, vs, _, _, _) <- getState

    updateState(symTableUpdateMatrixElem (e, i, exp) v_index1 v_index2)
    s <- getState

    return ([])

conditionalParser :: ParsecT [Token] Estado IO [Token]
conditionalParser = do
      ifToken
      openParentheseToken
      e <- expression
      closeParentheseToken
      updateState(pushScope "if")
      if e == getDefaulValue(BooleanType (AlexPn 1 1 1)) then do
        updateState(turnOnExecution)
        res <- subProgram
        updateState(popScope)
        updateState(turnOffExecution)
        remainingConditionalSint
        updateState(turnOnExecution)
        return res
      else do
        updateState(turnOffExecution)
        res <- subProgram
        updateState $ turnOnExecution
        remainingConditional
        return res

remainingConditional :: ParsecT [Token] Estado IO [Token]
remainingConditional = try elseIf <|> endConditional <|> return []
    where elseIf = do
          elseToken
          ifToken
          openParentheseToken
          e <- expression
          closeParentheseToken
          if e == getDefaulValue(BooleanType (AlexPn 1 1 1)) then do
            updateState(pushScope "elseif")
            updateState(turnOnExecution)
            res <- subProgram
            updateState(turnOffExecution)
            updateState(popScope)
            remainingConditionalSint
            updateState(turnOnExecution)
            return res
          else do
            updateState(turnOffExecution)
            res <- subProgram
            updateState(turnOnExecution)
            try remainingConditional <|> endConditional <|> return []
            return res

endConditional :: ParsecT [Token] Estado IO [Token]
endConditional = do
  elseToken
  updateState(turnOnExecution)
  updateState(pushScope "else")
  res <- subProgram
  updateState(popScope)
  return res


subProgram :: ParsecT [Token] Estado IO [Token]
subProgram = do
          (_,_,_,_,t) <- getState
          ob <- openBracerToken
          first <- if t then
                      command
                     else
                      commandSint
          if t then do
            res <- case first of
                        [Return p, x] -> do
                          updateState turnOffExecution
                          semiColonToken
                          res <- remainingStmts
                          (e:_,_,_,_,_) <- getState
                          updateState $ setExecution t
                          closeBracerToken
                          return [x]
                        _ -> do
                          semiColonToken
                          res <- remainingStmts
                          (e:_,_,_,_,_) <- getState 
                          updateState $ setExecution t
                          closeBracerToken
                          return res
            return res
          else do
            semiColonToken
            remainingStmts
            closeBracerToken
            return [VoidType $ AlexPn 1 1 1]

remainingStmts :: ParsecT [Token] Estado IO [Token]
remainingStmts = f <|> return []
  where f = do
              s <- getState
              b <- if isExecuting s then
                        command
                      else
                        commandSint
              if isExecuting s then do
                res <- case b of
                        [Return p, x] -> do
                          updateState turnOffExecution
                          semiColonToken
                          remainingStmts
                          updateState turnOnExecution
                          return [x]
                        _ -> do
                          semiColonToken
                          remainingStmts <|> return [VoidType $ AlexPn 1 1 1]
                return res
              else do
                semiColonToken
                remainingStmts <|> return []

commandSint :: ParsecT [Token] Estado IO [Token]
commandSint = try arrayDeclSint <|>
              try varDeclSint <|>
              try arrayAssSint <|>
              returnRuleSint<|>
              varAssSint <|>
              outputSint <|>
              inputSint <|>
              whileSint <|>
              functionCallSint <|>
              conditionalSint


command :: ParsecT [Token] Estado IO [Token]
command = try arrayAssElem <|>
          try matrixAssElem <|>
          try arrayDecl <|>
          try matrixDecl <|>
          try varDecl <|>
          try varAss <|>
          try arrayAss <|>
          returnRule <|>
          outputParser<|>
          inputParser<|>
          whileParser <|>
          conditionalParser <|>
          ((:[]) <$> functionCall)

whileSint :: ParsecT [Token] Estado IO [Token]
whileSint = do
            a <- whileToken
            b <- openParentheseToken
            c <- expressionSint
            d <- closeParentheseToken
            e <- subProgram
            return (a:b:c ++ d:e)


args :: Token -> ParsecT [Token] Estado IO [Variavel]
args (Identifier _ funcName) = do
       a <- identifierToken
       b <- colonToken
       c:_ <- typeToken
       d <- remainingArgs funcName <|> return []
       return ((funcName, a, getDefaulValue c): d)

remainingArgs :: String -> ParsecT [Token] Estado IO [Variavel]
remainingArgs funcName = do
                  a <- commaToken
                  b <- identifierToken
                  c <- colonToken
                  d:_ <- typeToken
                  e <- remainingArgs funcName <|> (return [])
                  return ((funcName, b, getDefaulValue d): e)

mainParser :: ParsecT [Token] Estado IO [Token]
mainParser = do
            -- liftIO $ print "parsing main"
            a <- mainToken
            b <- openParentheseToken
            -- argumentos
            c <- closeParentheseToken
            updateState(turnOnExecution)
            updateState(pushScope "main")
            s <- getState
            e <- subProgram
            return (a:b:c:e)

returnRule :: ParsecT [Token] Estado IO [Token]
returnRule = do
    ret <- returnToken
    e <- expression
    return [ret,e]


conditionalSint :: ParsecT [Token] Estado IO [Token]
conditionalSint = do
                a <- ifToken
                b <- openParentheseToken
                c <- expressionSint -- <expr>
                d <- closeParentheseToken
                f <- subProgram  -- <command>
                h <- remainingConditionalSint
                return (a:b:c ++ d:f ++h)



remainingConditionalSint :: ParsecT [Token] Estado IO [Token]
remainingConditionalSint = try f <|> endConditionalSint <|> return []
  where f = do
              a <- elseToken
              b <- ifToken
              c <- openParentheseToken
              d <- expressionSint
              e <- closeParentheseToken
              g <- subProgram
              i <- try remainingConditionalSint <|> endConditionalSint
              return (a:b:c:d ++ e:g ++i)

endConditionalSint :: ParsecT [Token] Estado IO [Token]
endConditionalSint = do
                    a <- elseToken
                    b <- subProgram
                    return (a:b)

iterable :: ParsecT [Token] Estado IO [Token]
iterable = do
    a <- ( (:[]) <$> f) <|> g
    return a
  where f = identifierToken
        g =  listParser

listParser :: ParsecT [Token] Estado IO [Token]
listParser = do
          ob <- openBracerToken
          mem <- members 
          cb <- closeBracerToken
          return (ob : mem ++ [cb])
      where
          members = do 
          i <- identifierToken
          c <- commaToken
          g <- members <|> return []
          return (i:c:g)

-- regra <entrada>

inputParser:: ParsecT [Token] Estado IO [Token]
inputParser= do
            a <- readToken
            vars <- remainingInputs
            ent <- words <$> liftIO getLine
            updateVariables vars ent
            return ([a]++vars)

updateVariables :: [Token] -> [String] -> ParsecT [Token] Estado IO [Token]
updateVariables [] [] = return []
updateVariables x [] = fail "not enouth values"
updateVariables [] x = fail "too many values"
updateVariables (x:xs) (s:vs) = do
    (es,vr,_ ,_,_)<- getState
    let mv = symTableGetValue x vr
    t <- case mv of
          Just v -> do 
            let nv = cast s v
            (e:_,_,_,_,_) <- getState
            updateState(symTableUpdateVariable(e, x, nv))
            updateVariables xs vs
            return []
          Nothing -> fail "Variavel nao declarada"
    return t


remainingInputs :: ParsecT [Token] Estado IO [Token]
remainingInputs = do
                    a <- extractionToken
                    b <- identifierToken
                    c <- remainingInputs <|> (return []) -- ver se isso ta certo
                    return (b:c)

-- regra <saida>
-- depende da regra <expr>

outputParser:: ParsecT [Token] Estado IO [Token]
outputParser= do
            a <- printToken
            b <- remainingOutputs
            liftIO (putStrLn $ foldr (++) "" (map show b))
            return (a:b)



remainingOutputs :: ParsecT [Token] Estado IO [Token]
remainingOutputs = do
                    a <- insertionToken
                    b <- expression -- <expr>
                    c <- remainingOutputs <|> (return []) -- ver se isso ta certo
                    return (b:(String (AlexPn 1 2 3) " "):c)

-- regra <funcao_chamada>
-- depende da regra <literal_id>

functionCall :: ParsecT [Token] Estado IO Token
functionCall = do
                (Identifier p nome) <- identifierToken
                b <- openParentheseToken
                par <- functionCallParams
                d <- closeParentheseToken
                ret <- getInput
                s <- getState
                let inp = getFunctionStart (Identifier p nome) s
                let fargs = getFunctionArgs (Identifier p nome) s
                l <- getLevel nome
                let vars = matchArgs par fargs l
                updateState(insertMultVariables vars)
                updateState(pushScope $ nome ++ show l)
                setInput inp
                s <- getState
                res <- subProgram
                updateState(popScope)
                setInput ret
                return $ head res


isPrefix :: String -> Escopo -> Bool
isPrefix [] _ = True
isPrefix (x:xs) [] = False
isPrefix (x:xs) (y:ys)
  | x == y = isPrefix xs ys
  | otherwise = False
getLevel :: String -> ParsecT [Token] Estado IO Int
getLevel nome = do
    (es,_,_,_,_) <- getState
    let v = map (\x -> if nome `isPrefix` x then 1 else 0) es
    return $ foldr (+) 0 v

fixScope :: [Variavel] -> Int -> [Variavel]
fixScope [] _ = []
fixScope ((e,n,v):xs) d = (e ++ show d, n,v):(fixScope xs d)

qeq :: String -> String -> (Bool, Int)
qeq [] [] = (True, 0)
qeq [] xs = case readMaybe xs of
              Just x -> (True, x)
              Nothing -> (False, 0)
qeq (s1:ss1) (s2:ss2)
  | s1 == s2 = qeq ss1 ss2
  | otherwise = (False, 0)

whileParser :: ParsecT [Token] Estado IO [Token]
whileParser = do
    whileToken
    inp <- getInput
    loopWhile inp

loopWhile :: [Token] -> ParsecT [Token] Estado IO [Token]
loopWhile pc = do
  setInput pc
  openParentheseToken
  e <- expression
  closeParentheseToken
  updateState(pushScope "while")
  if e == getDefaulValue(BooleanType (AlexPn 1 1 1)) then
    do
      res <- subProgram
      updateState(popScope)
      loopWhile pc
      return (e: res)
  else do
    updateState(turnOffExecution)
    res <- subProgram
    updateState(popScope)
    updateState(turnOnExecution)
    return $ e:res

--                      ARGS          
matchArgs :: [Token] -> [Variavel] -> Int  -> [Variavel]
matchArgs (t:ts) ((escopo, name, valor): ars) l
          = (escopo ++ show l, name, typeCompatible t valor): (matchArgs ts ars l)
matchArgs [] [] _ = []

functionCallParams :: ParsecT [Token] Estado IO [Token]
functionCallParams = (do
                        a <- expression -- <literal_id>
                        b <- remainingFunctionCallParams
                        return (a:b))
                        <|> (return [])

remainingFunctionCallParams :: ParsecT [Token] Estado IO [Token]
remainingFunctionCallParams = do f <|> return []
  where f = do
              a <- commaToken
              b <- expression -- <literal_id>
              c <- remainingFunctionCallParams <|> (return []) -- ver se isso ta certo
              return (b:c)

literalIdentifier :: ParsecT [Token] Estado IO Token
literalIdentifier = do
  a <- doubleToken <|>
       intToken <|>
       trueToken <|>
       falseToken <|>
       try matrixAcc <|>
       try arrayAcc <|>
       try variable <|>
       try functionCall <|>
       stringToken
  return a

accessStruct :: ParsecT [Token] Estado IO Token
accessStruct = do
  nome <- literalIdentifier
  setaToken
  atr <- literalIdentifier
  (_, vs, _, _, _) <- getState
  case symTableGetValue nome vs of
    Just (Struct nome atrb) -> return $ getValueAtr atr atrb
    Nothing -> fail "Tipo inexistente"

structAss :: ParsecT [Token] Estado IO [Token]
structAss = do
    (e:es,ts,_,_,_) <- getState
    nome <- identifierToken
    setaToken
    atr <- identifierToken
    expr <- expression
    semiColonToken
    case symTableGetValue nome ts of
      Just (Struct name valor) -> do 
        let nv = updateAtr valor atr expr
        updateState $ symTableUpdateVariable (e, nome, Struct name nv)
        return []
      Nothing -> fail "variavel nao declarada"

updateAtr :: [(Token, Token)] -> Token -> Token -> [(Token, Token)]
updateAtr ((nome, valor):ts) atr nv
  | nome == atr = (nome, nv):ts
  | otherwise = (nome, valor): updateAtr ts atr nv
updateAtr [] _ _ = error "nem tem esse atributo"


getValueAtr :: Token -> [(Token, Token)] -> Token
getValueAtr t ((nome, v):xs)
  | t == nome = v
  | otherwise = getValueAtr t xs
getValueAtr t [] = error $ "atributo " ++ show t ++ "nao existe"

typeConstructor :: ParsecT [Token] Estado IO Token
typeConstructor = do
  (e:es,_, _, ts, _) <- getState
  Identifier p nome <- identifierToken
  openParentheseToken
  closeParentheseToken
  let (nome, atrb) = getType nome ts
  return $ Struct nome atrb


variable :: ParsecT [Token] Estado IO Token
variable = do
  v <- identifierToken
  (_,s,_,_,_) <- getState
  let x = symTableGetValue v s
  case x of
    Just y -> return y
    Nothing -> fail "Variavel não declarada"

arrayAcc :: ParsecT [Token] Estado IO Token
arrayAcc = do
  id <- identifierToken
  ob <- openBracketToken
  value <- expression
  cb <- closeBracketToken
  (_,s,_,_,_) <- getState
  let (Int p v) = typeCompatible value $ getDefaulValue (IntType (AlexPn 1 1 1))
  let x = symTableGetValueArray id v s
  return x

matrixAcc :: ParsecT [Token] Estado IO Token
matrixAcc = do
  id <- identifierToken
  ob <- openBracketToken
  value1 <- expression
  cb <- closeBracketToken
  ob2 <- openBracketToken
  value2 <- expression
  cb2 <- closeBracketToken
  (_,s,_,_,_) <- getState
  let (Int p v1) = typeCompatible value1 $ getDefaulValue (IntType (AlexPn 1 1 1))
  let (Int p v2) = typeCompatible value2 $ getDefaulValue (IntType (AlexPn 1 1 1))
  let x = symTableGetValueMatrix id v1 v2 s
  return x


toStr :: [Token] -> String
toStr (x : xs) = show x ++ ' ': toStr xs
toStr [] = ""
