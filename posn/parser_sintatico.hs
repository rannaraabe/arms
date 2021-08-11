module Main where

import Lexer
import Text.Parsec

-- parsers para os tokens

intTypeToken = tokenPrim show update_pos get_token where
  get_token (IntType position) = Just $ IntType position
  get_token _    = Nothing

doubleTypeToken = tokenPrim show update_pos get_token where
  get_token (DoubleType position) = Just $ DoubleType position
  get_token _    = Nothing

importToken :: Parsec [Token] st Token
importToken = tokenPrim show update_pos get_token where
  get_token (Import position) = Just $ Import position
  get_token _    = Nothing

symArithToken :: Parsec [Token] st Token
symArithToken = tokenPrim show update_pos get_token where
  get_token (SymArith position name) = Just $ SymArith position name
  get_token _    = Nothing  

mainToken = tokenPrim show update_pos get_token where
  get_token (Main position) = Just $ Main position
  get_token _    = Nothing

openParentheseToken = tokenPrim show update_pos get_token where
  get_token (OpenParenthese position) = Just $ OpenParenthese position
  get_token _              = Nothing

closeParentheseToken = tokenPrim show update_pos get_token where
  get_token (CloseParenthese position) = Just $ CloseParenthese position
  get_token _               = Nothing

openBracerToken = tokenPrim show update_pos get_token where
  get_token (OpenBracer position) = Just $ OpenBracer position
  get_token _          = Nothing

closeBracerToken = tokenPrim show update_pos get_token where
  get_token (CloseBracer position) = Just $ CloseBracer position
  get_token _           = Nothing

colonToken = tokenPrim show update_pos get_token where
  get_token (Colon position)  = Just $ Colon position
  get_token _      = Nothing

identifierToken = tokenPrim show update_pos get_token where
  get_token (Identifier position name)  = Just $ Identifier position name
  get_token _          = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token (Int position name) = Just $ Int position name
  get_token _        = Nothing

doubleToken = tokenPrim show update_pos get_token where
  get_token (Double position name) = Just $ Double position name
  get_token _    = Nothing

semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon position)  = Just $ SemiColon position
  get_token _          = Nothing

assignToken = tokenPrim show update_pos get_token where
  get_token (Assign position) = Just $ Assign position
  get_token _          = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos

-- parsers para os não-terminais

program :: Parsec [Token] st [Token]
program = do
            a <- mainToken
            b <- openParentheseToken
            -- argumentos
            c <- closeParentheseToken
            d <- openBracerToken
            e <- stmts
            f <- closeBracerToken
            eof
            return (a:b:c:[d] ++ e ++ [f])



stmts :: Parsec [Token] st [Token]
stmts = do
          first <- assign
          next <- assign <|> return []
          return (first ++ next)



assign :: Parsec [Token] st [Token]
assign = do
          a <- identifierToken
          b <- assignToken
          c <- intToken <|> doubleToken
          d <- colonToken
          e <- intTypeToken <|> doubleTypeToken
          s <- semiColonToken
          return (a:b:c:d:[e])





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

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "./program_posn.pe") of
            Left err -> print err 
            Right ans -> print ans