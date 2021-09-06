import Lexer
import TabelaSimbolos
import Text.Parsec
import ParserSintatico
import Tokens
import Eval
import Control.Monad.IO.Class
import System.IO.Unsafe
import System.Environment

-- parsers para os não-terminais

initialState = (["global"], [], [], False)


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
          a <- try arrayDecl <|>
               try varDecl <|>
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
            s <- getState
            g <- subProgram
            updateState(symTableInsertSubProgram (name, arg, ret, inp))
            return (g)

arrayDecl :: ParsecT [Token] Estado IO [Token]
arrayDecl = do
          name <- identifierToken
          assignToken
          vec <- literalArray
          cl <- colonToken
          ar <- arrayToken
          ap <- openParentheseToken
          t <- typeToken
          cp <- closeParentheseToken
          return (name:vec ++ ar:ap:t ++ [cp])


literalArray ::ParsecT [Token] Estado IO [Token]
literalArray = do 
          ob <- openBracketToken
          first <- literalIdentifier
          rest <- remainingArrays
          return (first:rest)

remainingArrays :: ParsecT [Token] Estado IO [Token]
remainingArrays = f <|> g
      where f = do
              comma <- commaToken
              i <- literalIdentifier
              rest <- f <|> g
              return (i:rest)
            g = do
              c <- closeBracketToken
              return []

varAss :: ParsecT [Token] Estado IO [Token]
varAss = do
    name <- identifierToken
    a <- assignToken
    val <- expression
    (e:es,_,_,_) <- getState
    updateState(symTableUpdateVariable (e, name, val))
    s <- getState
    return (name:a:[val])

varDecl :: ParsecT [Token] Estado IO [Token]
varDecl = do
          name <- identifierToken
          b <- assignToken
          expr <- expression
          (e:_, _, _, _) <- getState
          updateState(symTableInsertVariable (e, name, expr))
          s <- getState
          --liftIO $ print s
          o <- remainingDecls
          d <- colonToken
          g <- typeToken
          return ((name:b:[expr]) ++ (d:g) )

arrayAss :: ParsecT [Token] Estado IO [Token]
arrayAss = do
    i <- identifierToken
    a <- assignToken
    l <- literalArray
    (e:es, vs, _, _) <- getState
    updateState(symTableUpdateVariable (e, i, Array (AlexPn 1 1 1) l))
    return (i:a:l)

remainingDecls:: ParsecT [Token] Estado IO [Token]
remainingDecls = (do
          c <- commaToken
          name <- identifierToken
          a <- assignToken
          expr <- expression
          (e:_, _, _, _) <- getState
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

conditionalParser :: ParsecT [Token] Estado IO [Token]
conditionalParser = do
      ifToken
      openParentheseToken
      e <- expression
      closeParentheseToken
      updateState(pushScope "if")
      if e == getDefaulValue(BooleanType (AlexPn 1 1 1)) then do
        updateState(turnOnExecution)
        subProgram
        updateState(popScope)
        updateState(turnOffExecution)
        remainingConditionalSint
        updateState(turnOnExecution)
        return []
      else do
        updateState(turnOffExecution)
        subProgram
        remainingConditional
        return []

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
            subProgram
            updateState(turnOffExecution)
            updateState(popScope)
            remainingConditionalSint
            updateState(turnOnExecution)
            return []
          else do
            updateState(turnOffExecution)
            subProgram
            try remainingConditional <|> endConditional <|> return []
            return []

endConditional :: ParsecT [Token] Estado IO [Token]
endConditional = do
  elseToken
  updateState(turnOnExecution)
  updateState(pushScope "else")
  subProgram
  updateState(popScope)
  return []


subProgram :: ParsecT [Token] Estado IO [Token]
subProgram = do
          s <- getState
          ob <- openBracerToken
          first <- if isExecuting s then
                      command
                     else
                      commandSint
          cm <- semiColonToken
          rest <- remainingStmts
          ret <- if isExecuting s then
                     returnRule <|> return [(VoidType $ AlexPn 1 1 1)]
                    else
                      returnRuleSint <|> return [(VoidType $ AlexPn 1 1 1)]
          cb <- closeBracerToken
          return ret

commandSint :: ParsecT [Token] Estado IO [Token]
commandSint = try arrayDeclSint <|>
              try varDeclSint <|>
              try arrayAssSint <|>
              varAssSint <|>
              outputSint <|>
              inputSint <|>
              whileSint <|>
              conditionalSint


command :: ParsecT [Token] Estado IO [Token]
command = try arrayDecl <|>
          try varDecl <|>
          try varAss <|>
          try arrayAss <|>
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
    return [e]



remainingStmts :: ParsecT [Token] Estado IO [Token]
remainingStmts = (do
                        s <- getState
                        b <- if isExecuting s then
                                  command
                                else
                                  commandSint
                        sm <- semiColonToken
                        rs <- remainingStmts <|> return []
                        return (b ++ sm:rs)) <|> return []


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

-- regras <repeticao>
-- depende das regras <iteravel>, <expr> e <command>

-- loop :: ParsecT [Token] Estado IO [Token] -- talvez possa ser removido
-- loop = do
--         a <- loopFor <|> loopWhile
--         return (a)

-- loopFor :: ParsecT [Token] Estado IO [Token]
-- loopFor = do
--             a <- forToken
--             b <- identifierToken
--             c <- inToken
--             d <- iterable -- <iteravel>
--             e <- openBracerToken
--             f <- return [] -- <command>
--             g <- closeBracerToken
--             return (a:b:[c]++d++[e]++f++[g])

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

         {-  remainigTokens = do
            a <- f <|> return []
            return a 
          f = do
            i <- identifierToken
            c <- commaToken
            r <- remainigTokens
            return (i:c:r)  -}
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
    (es,vr,_ ,_)<- getState
    let v = symTableGetValue x vr
    let nv = cast s v
    (e:_,_,_,_) <- getState
    updateState(symTableUpdateVariable(e, x, nv))
    updateVariables xs vs
    return []


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
                (Identifier p a) <- identifierToken
                b <- openParentheseToken
                par <- functionCallParams
                d <- closeParentheseToken
                ret <- getInput
                s <- getState
                let inp = getFunctionStart (Identifier p a) s
                let arg = getFunctionArgs (Identifier p a) s
                let vars = matchArgs par arg
                updateState(insertMultVariables vars)
                updateState(pushScope a)
                setInput inp
                res:_ <- subProgram
                updateState(popScope)
                setInput ret
                return res

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
matchArgs :: [Token] -> [Variavel] -> [Variavel]
matchArgs (t:ts) ((escopo, name, valor): ars)
          = (escopo, name, typeCompatible t valor): (matchArgs ts ars)
matchArgs [] [] = []

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
       try variable <|>
       functionCall <|>
       stringToken
  return a

variable :: ParsecT [Token] Estado IO Token
variable = do
  v <- identifierToken
  (_,s,_,_) <- getState
  let x = symTableGetValue v s
  return x


toStr :: [Token] -> String
toStr (x : xs) = show x ++ ' ': toStr xs
toStr [] = ""
