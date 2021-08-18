module Main where

import Lexer
import Text.Parsec
import Tokens
import Text.Parsec.Expr
-- parsers para os não-terminais


typeToken :: Parsec [Token] st [Token]
typeToken = do a <- intTypeToken <|> doubleTypeToken <|> boolTypeToken
               return ([a])

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "./program_posn.pe") of
            Left err -> print err 
            Right ans -> print ans

program :: Parsec [Token] st [Token]
program = do
            a <- begin
            b <- mainParser
            eof
            return (a ++ b)

begin :: Parsec [Token] st [Token]
begin = do
          a <- try var_decl <|> try func_decl <|> try single_import -- add var global depois 
          b <- begin <|> return []
          return (a ++ b)

var_decl :: Parsec [Token] st [Token]
var_decl = do
          a <- identifierToken
          b <- assignToken
          c <- expression
          d <- colonToken
          g <- typeToken
          h <- semiColonToken
          return ((a:b:c) ++ (d:g ++ [h]) )

single_import :: Parsec [Token] st [Token]
single_import = do
          a <- importToken
          b <- identifierToken
          return (a : [b])


func_decl :: Parsec [Token] st [Token]
func_decl = do
            a <- identifierToken
            b <- openParentheseToken
            c <- args
            d <- closeParentheseToken
            e <- colonToken
            f <- typeToken -- tipo do retorno
            g <- openBracerToken
            h <- return []
            i <- closeBracerToken
            return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ [g] ++ h ++ [i])


args :: Parsec [Token] st [Token]
args = (do
       a <- identifierToken
       b <- colonToken
       c <- typeToken
       d <- remaining_args
       return (a : [b] ++ c ++ d)) <|> (return [])

remaining_args :: Parsec [Token] st [Token]
remaining_args = (do
                  a <- commaToken
                  b <- identifierToken
                  c <- colonToken
                  d <- typeToken
                  e <- remaining_args
                  return (a : b : [c] ++ d ++ e)) <|> (return [])

mainParser :: Parsec [Token] st [Token]
mainParser = do
            a <- mainToken
            b <- openParentheseToken
            -- argumentos
            c <- closeParentheseToken
            d <- openBracerToken
            e <- stmts
            f <- closeBracerToken
            return (a:b:c:[d] ++ e ++ [f])


stmts :: Parsec [Token] st [Token]
stmts = do
          first <- var_decl
          return first

expression :: Parsec [Token] st [Token]
expression = do
          a <- expr_bool <|> expr_arith
          return a


conditional :: Parsec [Token] st [Token]
conditional = do
                a <- ifToken
                b <- openParentheseToken
                c <- expression -- <expr>
                d <- closeParentheseToken
                e <- openBracerToken
                f <- stmts -- <comando>
                g <- closeBracerToken
                h <- remaining_conditionals
                i <- end_conditional
                return (a:[b]++c++d:[e]++f++[g]++h++i)

expr_arith :: Parsec [Token] st [Token]
expr_arith = do
      a <- ((:[]) <$> (f <|> g <|> h)) <|> j
      return a
      where 
        f = intToken
        g = doubleToken
        h = identifierToken
        j = do
          expr1 <- f <|> g <|> h
          operation <- symArithToken <|> symToken
          expr2 <- expr_arith
          return ([expr1] ++ [operation] ++ expr2)

-- a and b and c
expr_bool :: Parsec [Token] st [Token]
expr_bool = do
      a <- try expressao <|> valor_simples
      return a
    where
          valor_simples = do
            d <- identifierToken <|> trueToken <|> falseToken
            return [d]
          expressao = do
              expr1 <- valor_simples
              operation <- try boolOPToken
              expr2 <- try expressao
              return (expr1 ++ [operation] ++ expr2) <|> do return []
          {- j = do
              expr1 <- f <|> intToken <|> doubleToken
              operation <- relOPToken
              expr2 <- expr_arith <|> return []
              return ([expr1] ++ [operation] ++ expr2) -}
    

remaining_conditionals :: Parsec [Token] st [Token]
remaining_conditionals = (do
                            a <- elseToken
                            b <- ifToken
                            c <- openParentheseToken
                            d <- expression -- <expr>
                            e <- closeParentheseToken
                            f <- openBracerToken
                            g <- stmts -- <comando>
                            h <- closeBracerToken
                            i <- remaining_conditionals
                            return (a:b:[c]++d++e:[f]++g++[h]++i))
                        <|> (return [])

end_conditional :: Parsec [Token] st [Token]
end_conditional = (do
                    a <- elseToken
                    b <- openBracerToken
                    c <- stmts -- <comando>
                    d <- closeBracerToken
                    return (a:[b]++c++[d]))
                <|> (return [])

-- regras <repeticao>
-- depende das regras <iteravel>, <expr> e <comando>

loop :: Parsec [Token] st [Token] -- talvez possa ser removido
loop = do
        a <- loop_for <|> loop_while
        return (a)

loop_for :: Parsec [Token] st [Token]
loop_for = do
            a <- forToken
            b <- identifierToken
            c <- inToken
            d <- iterable -- <iteravel>
            e <- openBracerToken
            f <- stmts -- <comando>
            g <- closeBracerToken
            return (a:b:[c]++d++[e]++f++[g])

iterable :: Parsec [Token] st [Token]
iterable = do
    a <- ( (:[]) <$> f) <|> g
    return a
  where f = identifierToken
        g =  listParser

listParser :: Parsec [Token] st [Token]
listParser = do
          abre <- openBracerToken
          mem <- members 
          fecha <- closeBracerToken
          return (abre : mem ++ [fecha])
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



loop_while :: Parsec [Token] st [Token]
loop_while = do
                a <- whileToken
                b <- openParentheseToken
                c <- expression -- <expr>
                d <- closeParentheseToken
                e <- openBracerToken
                f <- stmts -- <comando>
                g <- closeBracerToken
                return (a:[b]++c++d:[e]++f++[g])

-- regra <entrada>

input :: Parsec [Token] st [Token]
input = do
            a <- readToken
            b <- remaining_input
            return ([a]++b)

remaining_input :: Parsec [Token] st [Token]
remaining_input = do
                    a <- extractionToken
                    b <- identifierToken
                    c <- remaining_input <|> (return []) -- ver se isso ta certo
                    return (a:[b]++c)

-- regra <saida>
-- depende da regra <expr>

output :: Parsec [Token] st [Token]
output = do
            a <- printToken
            b <- remaining_output
            return (a:b)

remaining_output :: Parsec [Token] st [Token]
remaining_output = do
                    a <- insersionToken
                    b <- expression -- <expr>
                    c <- remaining_output <|> (return []) -- ver se isso ta certo
                    return ((a:b) ++ c)

-- regra <funcao_chamada>
-- depende da regra <literal_id>

function_call :: Parsec [Token] st [Token]
function_call = do
                a <- identifierToken
                b <- openParentheseToken
                c <- function_call_params
                d <- closeParentheseToken
                return (a:[b]++c++[d]) 

function_call_params :: Parsec [Token] st [Token]
function_call_params = (do
                        a <- literal_identifier -- <literal_id>
                        b <- remaining_function_call_params
                        return (a++b))
                        <|> (return [])

remaining_function_call_params :: Parsec [Token] st [Token]
remaining_function_call_params = do f <|> return []
  where f = do
              a <- commaToken
              b <- literal_identifier -- <literal_id>
              c <- remaining_function_call_params <|> (return []) -- ver se isso ta certo
              return ([a] ++ b ++ c)

-- regra <retorno>
-- depende da regra <expr>

literal_identifier :: Parsec [Token] st [Token]
literal_identifier = do
  a <- doubleToken <|> intToken <|> trueToken <|> falseToken
  return [a]

return_rule :: Parsec [Token] st [Token]
return_rule = do
                a <- returnToken
                b <- expression -- <expr>
                return (a:b)
 
-- command :: Parsec [Token] st [Token]
-- command = do
--           a <- assign <|> conditional <|> loop <|> read <|> print <|> funcCall <|> return <|>
--           b <- semiColonToken 
--           return (a:b)


-- vars :: Parsec [Token] st [Token]
-- vars = ()

-- remaining_vars :: Parsec [Token] st [Token]
-- remaining_vars = (do
--                   a <- commaToken
--                   b <- args)

-- mainParser :: Parsec [Token] st [Token]
-- mainParser = do
--             a <- mainToken
--             b <- openParentheseToken
--             -- argumentos
--             c <- closeParentheseToken
--             d <- openBracerToken
--             e <- stmts
--             f <- closeBracerToken
--             return (a:b:c:[d] ++ e ++ [f])



-- stmts :: Parsec [Token] st [Token]
-- stmts = do
--           first <- assign
--           next <- assign <|> return []
--           return (first ++ next)



-- assign :: Parsec [Token] st [Token]
-- assign = do
--           a <- identifierToken
--           b <- assignToken
--           c <- intToken <|> doubleToken
--           f <- symToken
--           g <- intToken <|> doubleToken
--           d <- colonToken
--           e <- intTypeToken <|> doubleTypeToken
--           s <- semiColonToken
--           return (a:b:c:f:g:d:[e])



-- var_array :: Parsec [Token] st [Token]
-- var_array = do
--           a <- var_array_size <|> var_array_assing -- vai bugar por causa da igualdade?
--           b <- remaining_var_array
--           c <- colonToken
--           d <- intTypeToken <|> doubleTypeToken -- ter um símbolo não terminal para os tipos
--           return (a + b + c:[d])

-- -- a[5] : int
-- -- a[3] = [1,2,3] : int
-- -- a[5] = [1,2,3,4,5], b[2] = [4,3] : int

-- remaining_var_array :: Parsec [Token] st [Token]
-- remaining_var_array = (do
--           a <- commaToken
--           b <- var_array_size <|> var_array_assing
--           c <- remaining_var_array
--           return (a:b:c)) <|> return []

-- var_array_size :: Parsec [Token] st [Token] -- array_name[size]
-- var_array_size = do
--           a <- identifierToken
--           b <- openBracket
--           c <- intToken
--           d <- closeBracket
--           return (a:b:c:[d])

-- var_array_assing :: Parsec [Token] st [Token] -- array_name[size] = [1,2,...]
-- var_array_assing = do
--           a <- var_array_size
--           b <- assignToken
--           c <- array_expr
--           return (a + b:c)



-- var_matrix :: Parsec [Token] st [Token]
-- var_array = do
--           a <- var_matrix_size <|> var_matrix_assing
--           b <- remaining_var_matrix
--           c <- colonToken
--           d <- intTypeToken <|> doubleTypeToken -- ter um símbolo não terminal para os tipos
--           return (a + b + c:[d])

-- remaining_var_matrix :: Parsec [Token] st [Token]
-- remaining_var_matrix = (do
--           a <- commaToken
--           b <- var_matrix_size <|> var_matrix_assing
--           c <- remaining_var_matrix
--           return (a + b + c)) <|> return []

-- var_matrix_size :: Parsec [Token] st [Token] -- matrix_name[size][size]
-- var_matrix_size = do
--           a <- identifierToken
--           b <- openBracket
--           c <- intToken
--           d <- closeBracket
--           e <- openBracket
--           f <- intToken
--           g <- closeBracket
--           return (a:b:c:d:e:f:[g])

-- var_matrix_assing :: Parsec [Token] st [Token] -- array_name[size][size] = [[1,2,...],[],...]
-- var_matrix_assing = do
--           a <- var_matrix_size
--           b <- assignToken
--           c <- matrix_expr
--           return (a + b:c)


-- command :: Parsec [Token] st [Token]
-- command = do
--           a <- assign <|> conditional <|> loop <|> read <|> print <|> funcCall <|> return <|>
--           b <- semiColonToken 
--           return (a:b)






-- parsers para os não-terminais

-- program :: Parsec [Token] st [Token]
-- program = do
--             a <- imports -- tornar funcao
--             b <- functions
--             -- principal
--             c <- mainToken
--             o <- openParentheseToken
--             -- argumentos
--             j <- closeParentheseToken
--             k <- openBracerToken
--             l <- stmts
--             m <- closeBracerToken
--             eof
--             return ()

-- imports :: Parsec [Token] st [Token]
-- imports = do
--           first <- single_import
--           next <- remaining_imports
--           return (first ++ next)

-- single_import :: Parsec [Token] st [Token]
-- single_import = do
--             a <- importToken -- criar esse token
--             b <- identifierToken
--             return (a:[b])

-- remaining_imports :: Parsec [Token] st [Token]
-- remaining_imports = (do a <- single_import return (a)) 
--                     <|> (return [])

-- functions :: Parsec [Token] st [Token]
-- functions = do
--             first <- function
--             next <- remaining_functions
--             return (first ++ next)

-- function :: Parsec [Token] st [Token]
-- function = do
--             a <- identifierToken
--             b <- openParentheseToken -- sem parametros
--             c <- args
--             c <- closeParentheseToken
--             d <- colonToken
--             e <- identifierToken -- tipo do retorno
--             f <- openBracerToken
--             g <- stmts
--             h <- closeBracerToken
--             return ()

-- remaining_functions :: Parsec [Token] st [Token]
-- remaining_functions = (do a <- function
--                           return (a)) <|> (return [])

-- stmts :: Parsec [Token] st [Token]
-- stmts = do
--           first <- stmt
--           next <- remaining_stmts
--           return (first ++ next)

-- stmt :: Parsec [Token] st [Token]
-- stmt = do
--           a <- assign
--           b <- print
--           c <- read
--           d <- chamada_funcao
--           e <- return
--           f <- for
--           g <- while
--           h <- if
--           i <- operacao
--           j <- declaracao
--           return ()



-- assign :: Parsec [Token] st [Token]
-- assign = do
--           a <- idToken
--           b <- symToken
--           c <- type
--           return ()

-- chamada_funcao :: Parsec [Token] st [Token]
-- chamada_funcao = do
--                   a <- IdentifierToken
--                   b <- openParentheseToken -- colocar parametros
--                   c <- closeParentheseToken
--                   return ()

-- remaining_stmts :: Parsec [Token] st [Token]
-- remaining_stmts = (do a <- semiColonToken
--                       b <- assign
--                       return (a:b)) <|> (return [])

-- invocação do parser para o símbolo de partida 
