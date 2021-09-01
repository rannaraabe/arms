import Lexer
import TabelaSimbolos
import Text.Parsec
import ParserSemantico
import Tokens
import Eval
import Control.Monad.IO.Class
import System.IO.Unsafe
-- parsers para os não-terminais

initialState = (["global"], [], [], False)


parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program initialState "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "./program_posn.pe")) of
            Left err -> print err
            Right ans -> print ans

program :: ParsecT [Token] Estado IO [Token]
program = do
            a <- begin
            b <- mainParser
            eof
            return (a ++ b)

begin :: ParsecT [Token] Estado IO [Token]
begin = do
          a <- try arrayDecl <|>
               try varDecl <|>
               try (funcDecl) -- add struct? 
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
            ret:_ <- typeToken -- tipo do retorno
            inp <- getInput
            s <- getState
            g <- subProgram
            --liftIO(print "ei eiei eiei")
           -- liftIO (print $ "inserindo" ++ show (name, arg, ret, inp))
            updateState(symTableInsertSubProgram (name, arg, ret, inp))
            s <- getState
            return (g)

arrayDecl :: ParsecT [Token] Estado IO [Token]
arrayDecl = do 
          name <- identifierToken
          a <- assignToken
          vec <- literalArray
          cl <- colonToken
          ar <- arrayToken
          ap <- openParentheseToken
          t <- typeToken
          cp <- closeParentheseToken
          return (name:a:vec ++ ar:ap:t ++ [cp])


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
    updateState(symTableUpdataVal (e, name, val))
    s <- getState
    return (name:a:[val])

varDecl :: ParsecT [Token] Estado IO [Token]
varDecl = do
          name <- identifierToken
          b <- assignToken
          expr <- expression
          (e:_, _, _, _) <- getState
          updateState(symTableInsertVal (e, name, expr))
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
    updateState(symTableUpdataVal (e, i, Array (AlexPn 1 1 1) l))
    return (i:a:l)

remainingDecls:: ParsecT [Token] Estado IO [Token]
remainingDecls = (do
          c <- commaToken
          name <- identifierToken
          a <- assignToken
          expr <- expression
          (e:_, _, _, _) <- getState
          updateState(symTableInsertVal (e, name, expr))
          r <- remainingDecls
          return (c:name:a:expr:r)) <|> return []

expression :: ParsecT [Token] Estado IO Token
expression = do
          a <- literalIdentifier
          result <- remainingExpressions a
          return (result)

remainingExpressions :: Token -> ParsecT [Token] Estado IO Token
remainingExpressions n1 = (do
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
                    c <- remainingExpressions $ eval n1 a b
                    return $ c) <|> return n1


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

command :: ParsecT [Token] Estado IO [Token]
command = try arrayDecl <|>
          try varDecl <|>
          try varAss <|>
          try output <|>
          try whileParser <|>
          try ((:[]) <$> functionCall) <|>
          try arrayAss

returnRule :: ParsecT [Token] Estado IO [Token]
returnRule = do
    returnToken
    e <- expression
    semiColonToken
    return [e]

subProgram :: ParsecT [Token] Estado IO [Token]
subProgram = do
          s <- getState
          ob <- openBracerToken
          first <- if isExecuting s then
                      command
                     else
                      commandSinc
          cm <- semiColonToken
          rest <- remainingStmts
          ret <- if isExecuting s then
                     returnRule <|> return []
                    else
                      returnRuleSint <|> return []

          cb <- closeBracerToken
          return ret

remainingStmts :: ParsecT [Token] Estado IO [Token]
remainingStmts = (do
                        s <- getState
                        b <- if isExecuting s then
                                  command
                                else
                                  commandSinc
                        sm <- semiColonToken
                        rs <- remainingStmts <|> return []
                        return (b ++ sm:rs)) <|> return []


conditional :: ParsecT [Token] Estado IO [Token]
conditional = do
                a <- ifToken
                b <- openParentheseToken
                c <- expression -- <expr>
                d <- closeParentheseToken
                e <- openBracerToken
                f <- return []  -- <command>
                g <- closeBracerToken
                h <- remainingConditionals
                i <- endConditional
                return (a:b:c:d:[e]++f++[g]++h++i)



remainingConditionals :: ParsecT [Token] Estado IO [Token]
remainingConditionals = (do
                            a <- elseToken
                            b <- ifToken
                            c <- openParentheseToken
                            d <- expression -- <expr>
                            e <- closeParentheseToken
                            f <- openBracerToken
                            g <- return [] -- <command>
                            h <- closeBracerToken
                            i <- remainingConditionals
                            return (a:b:c:d:e:f:g++[h]++i))
                        <|> (return [])

endConditional :: ParsecT [Token] Estado IO [Token]
endConditional = (do
                    a <- elseToken
                    b <- openBracerToken
                    c <- return [] -- <command>
                    d <- closeBracerToken
                    return (a:[b]++c++[d]))
                <|> (return [])

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



-- loopWhile :: ParsecT [Token] Estado IO [Token]
-- loopWhile = do
--                 a <- whileToken
--                 b <- openParentheseToken
--                 c <- expression -- <expr>
--                 d <- closeParentheseToken
--                 e <- openBracerToken
--                 f <- return [] -- <command>
--                 g <- closeBracerToken
--                 return (a:b:c:d:e:f++[g])

-- regra <entrada>

input :: ParsecT [Token] Estado IO [Token]
input = do
            a <- readToken
            vars <- remainingInputs
            ent <- words <$> liftIO getLine
            return ([a]++vars)

updateVariables :: [Token] -> [String] -> ParsecT [Token] Estado IO [Token]
updateVariables [] [] = error "algo de errado não esta certo"
updateVariables (x:xs) (v:vs) = do
    (es,vr,_ ,_)<- getState
    let v = symTableGetVal x vr
    return []


remainingInputs :: ParsecT [Token] Estado IO [Token]
remainingInputs = do
                    a <- extractionToken
                    b <- identifierToken
                    c <- remainingInputs <|> (return []) -- ver se isso ta certo
                    return (b:c)

-- regra <saida>
-- depende da regra <expr>

output :: ParsecT [Token] Estado IO [Token]
output = do
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
                inp <- getInput
                s <- getState
                let inp = getFunctionStart (Identifier p a) s
                let arg = getFunctionArgs (Identifier p a) s
                let vars = matchArgs par arg
                updateState(insertMultVariables vars)
                updateState(pushScope a)
                setInput inp
                res:_ <- subProgram
                updateState(popScope)
                s <- getState
                --liftIO $ print s
                setInput inp
                return (res)

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
    s <- getState
    updateState(turnOnExecution)
    return $ e:res



matchArgs :: [Token] -> [Variavel] -> [Variavel]
matchArgs (t:ts) ((sta, name, valor): ars)
          = (sta, name, typeCompatible t valor): (matchArgs ts ars)
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

-- regra <retorno>
-- depende da regra <expr>

literalIdentifier :: ParsecT [Token] Estado IO Token
literalIdentifier = do
  a <- doubleToken <|> 
       intToken <|>
       trueToken <|>
       falseToken <|>
       try functionCall <|>
       variable <|>
       stringToken
  return a

variable :: ParsecT [Token] Estado IO Token
variable = do
  v <- identifierToken
  (_,s,_,_) <- getState
  let x = symTableGetVal v s
  return x


toStr :: [Token] -> String
toStr (x : xs) = show x ++ ' ': toStr xs
toStr [] = ""
