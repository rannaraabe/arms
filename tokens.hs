module Tokens where

import Lexer
import TabelaSimbolos
import Text.Parsec

-- parsers para os tokens
intTypeToken ::ParsecT [Token] st IO (Token)
intTypeToken = tokenPrim show update_pos get_token where
  get_token (IntType position) = Just $ IntType position
  get_token _    = Nothing

stringToken ::ParsecT [Token] st IO (Token)
stringToken = tokenPrim show update_pos get_token where
  get_token (String position val) = Just $ String position $ fixStr val
  get_token _    = Nothing

fixStr :: String -> String
fixStr (x:xs) 
  | '"' == x = takeWhile (/='"') xs
  |otherwise = x:xs
fixStr _ = ""

stringTypeToken :: ParsecT [Token] st IO (Token)
stringTypeToken = tokenPrim show update_pos get_token where
  get_token (StringType position) = Just $ StringType position
  get_token _    = Nothing

boolTypeToken :: ParsecT [Token] st IO (Token)
boolTypeToken = tokenPrim show update_pos get_token where
  get_token (BooleanType position) = Just $ BooleanType position
  get_token _    = Nothing

voidTypeToken :: ParsecT [Token] st IO (Token)
voidTypeToken = tokenPrim show update_pos get_token where
  get_token (VoidType position) = Just $ VoidType position
  get_token _    = Nothing

arrayToken :: ParsecT [Token] st IO (Token)
arrayToken = tokenPrim show update_pos get_token where
  get_token (ArrayType position) = Just $ BooleanType position
  get_token _    = Nothing

doubleTypeToken :: ParsecT [Token] st IO (Token)
doubleTypeToken = tokenPrim show update_pos get_token where
  get_token (DoubleType position) = Just $ DoubleType position
  get_token _    = Nothing

symArithToken :: ParsecT [Token] st IO (Token)
symArithToken = tokenPrim show update_pos get_token where
  get_token (SymArith position name) = Just $ SymArith position name
  get_token _    = Nothing

symToken :: ParsecT [Token] st IO (Token)
symToken = tokenPrim show update_pos get_token where
   get_token (Sym position name) = Just $ Sym position name
   get_token _    = Nothing

mainToken :: ParsecT [Token] st IO (Token)
mainToken = tokenPrim show update_pos get_token where
  get_token (Main position) = Just $ Main position
  get_token _    = Nothing

commaToken :: ParsecT [Token] st IO (Token)
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma position) = Just $ Comma position
  get_token _    = Nothing

openParentheseToken :: ParsecT [Token] st IO (Token)
openParentheseToken = tokenPrim show update_pos get_token where
  get_token (OpenParenthese position) = Just $ OpenParenthese position
  get_token _              = Nothing
closeParentheseToken :: ParsecT [Token] st IO (Token)
closeParentheseToken = tokenPrim show update_pos get_token where
  get_token (CloseParenthese position) = Just $ CloseParenthese position
  get_token _               = Nothing

openBracerToken :: ParsecT [Token] st IO (Token)
openBracerToken = tokenPrim show update_pos get_token where
  get_token (OpenBracer position) = Just $ OpenBracer position
  get_token _          = Nothing

closeBracerToken :: ParsecT [Token] st IO (Token)
closeBracerToken = tokenPrim show update_pos get_token where
  get_token (CloseBracer position) = Just $ CloseBracer position
  get_token _           = Nothing

colonToken :: ParsecT [Token] st IO (Token)
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon position)  = Just $ Colon position
  get_token _      = Nothing

identifierToken :: ParsecT [Token] st IO (Token)
identifierToken = tokenPrim show update_pos get_token where
  get_token (Identifier position name)  = Just $ Identifier position name
  get_token _          = Nothing


closeBracketToken :: ParsecT [Token] st IO (Token)
closeBracketToken = tokenPrim show update_pos get_token where
  get_token (CloseBracket position) = Just $ CloseBracket position
  get_token _        = Nothing

openBracketToken :: ParsecT [Token] st IO (Token)
openBracketToken = tokenPrim show update_pos get_token where
  get_token (OpenBracket position) = Just $ OpenBracket position
  get_token _        = Nothing

trueToken ::ParsecT [Token] st IO (Token)
trueToken = tokenPrim show update_pos get_token where
  get_token (TrueSym position) = Just $ TrueSym position
  get_token _        = Nothing

falseToken :: ParsecT [Token] st IO (Token)
falseToken = tokenPrim show update_pos get_token where
  get_token (FalseSym position) = Just $ FalseSym position
  get_token _        = Nothing

boolOPToken :: ParsecT [Token] st IO (Token)
boolOPToken = tokenPrim show update_pos get_token where
  get_token (BoolOP position name) = Just $ BoolOP position name 
  get_token _        = Nothing

relOPToken :: ParsecT [Token] st IO (Token)
relOPToken = tokenPrim show update_pos get_token where
  get_token (RelOP position name) = Just $ RelOP position name 
  get_token _        = Nothing

intToken :: ParsecT [Token] st IO (Token)
intToken = tokenPrim show update_pos get_token where
  get_token (Int position name) = Just $ Int position name
  get_token _        = Nothing

doubleToken :: ParsecT [Token] st IO (Token)
doubleToken = tokenPrim show update_pos get_token where
  get_token (Double position name) = Just $ Double position name
  get_token _    = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon position)  = Just $ SemiColon position
  get_token _          = Nothing

assignToken :: ParsecT [Token] st IO (Token)
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign position) = Just $ Assign position
  get_token _          = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos


ifToken :: ParsecT [Token] st IO (Token)
ifToken = tokenPrim show update_pos get_token where
    get_token (If position) = Just $ If position
    get_token _             = Nothing

elseToken :: ParsecT [Token] st IO (Token)
elseToken = tokenPrim show update_pos get_token where
    get_token (Else position) = Just $ Else position
    get_token _             = Nothing

forToken :: ParsecT [Token] st IO (Token)
forToken = tokenPrim show update_pos get_token where
    get_token (For position) = Just $ For position
    get_token _             = Nothing

inToken :: ParsecT [Token] st IO (Token)
inToken = tokenPrim show update_pos get_token where
    get_token (In position) = Just $ In position
    get_token _             = Nothing

whileToken :: ParsecT [Token] st IO (Token)
whileToken = tokenPrim show update_pos get_token where
    get_token (While position) = Just $ While position
    get_token _             = Nothing

breakToken :: ParsecT [Token] st IO (Token)
breakToken = tokenPrim show update_pos get_token where
    get_token (Break position) = Just $ Break position
    get_token _             = Nothing

continueToken :: ParsecT [Token] st IO (Token)
continueToken = tokenPrim show update_pos get_token where
    get_token (Continue position) = Just $ Continue position
    get_token _             = Nothing

returnToken :: ParsecT [Token] st IO (Token)
returnToken = tokenPrim show update_pos get_token where
    get_token (Return position) = Just $ Return position
    get_token _             = Nothing

readToken :: ParsecT [Token] st IO (Token)
readToken = tokenPrim show update_pos get_token where
    get_token (Read position) = Just $ Read position
    get_token _             = Nothing

extractionToken :: ParsecT [Token] st IO (Token)
extractionToken = tokenPrim show update_pos get_token where
    get_token (Extraction position) = Just $ Extraction position
    get_token _             = Nothing

printToken :: ParsecT [Token] st IO (Token)
printToken = tokenPrim show update_pos get_token where
    get_token (Print position) = Just $ Print position
    get_token _             = Nothing

insertionToken :: ParsecT [Token] st IO (Token)
insertionToken = tokenPrim show update_pos get_token where
    get_token (Insertion position) = Just $ Insertion position
    get_token _             = Nothing

