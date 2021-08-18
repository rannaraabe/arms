module Tokens where

import Lexer
import Text.Parsec

-- parsers para os tokens
intTypeToken :: Parsec [Token] st Token
intTypeToken = tokenPrim show update_pos get_token where
  get_token (IntType position) = Just $ IntType position
  get_token _    = Nothing

boolTypeToken :: Parsec [Token] st Token
boolTypeToken = tokenPrim show update_pos get_token where
  get_token (BooleanType position) = Just $ BooleanType position
  get_token _    = Nothing

doubleTypeToken :: Parsec [Token] st Token
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

symToken :: Parsec [Token] st Token
symToken = tokenPrim show update_pos get_token where
   get_token (Sym position name) = Just $ Sym position name
   get_token _    = Nothing

mainToken :: Parsec [Token] st Token
mainToken = tokenPrim show update_pos get_token where
  get_token (Main position) = Just $ Main position
  get_token _    = Nothing

commaToken :: Parsec [Token] st Token
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma position) = Just $ Comma position
  get_token _    = Nothing


openParentheseToken ::Parsec [Token] st Token
openParentheseToken = tokenPrim show update_pos get_token where
  get_token (OpenParenthese position) = Just $ OpenParenthese position
  get_token _              = Nothing

closeParentheseToken ::Parsec [Token] st Token
closeParentheseToken = tokenPrim show update_pos get_token where
  get_token (CloseParenthese position) = Just $ CloseParenthese position
  get_token _               = Nothing

openBracerToken :: Parsec [Token] st Token
openBracerToken = tokenPrim show update_pos get_token where
  get_token (OpenBracer position) = Just $ OpenBracer position
  get_token _          = Nothing

closeBracerToken :: Parsec [Token] st Token
closeBracerToken = tokenPrim show update_pos get_token where
  get_token (CloseBracer position) = Just $ CloseBracer position
  get_token _           = Nothing

colonToken :: Parsec [Token] st Token
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon position)  = Just $ Colon position
  get_token _      = Nothing

identifierToken :: Parsec [Token] st Token
identifierToken = tokenPrim show update_pos get_token where
  get_token (Identifier position name)  = Just $ Identifier position name
  get_token _          = Nothing



-- symToken = tokenPrim show update_pos get_token where
--   get_token (Sym position name)  = Just $ Sym position name
--   get_token _          = Nothing

trueToken ::Parsec [Token] st Token
trueToken = tokenPrim show update_pos get_token where
  get_token (TrueSym position) = Just $ TrueSym position
  get_token _        = Nothing

falseToken :: Parsec [Token] st Token
falseToken = tokenPrim show update_pos get_token where
  get_token (FalseSym position) = Just $ FalseSym position
  get_token _        = Nothing

boolOPToken :: Parsec [Token] st Token
boolOPToken = tokenPrim show update_pos get_token where
  get_token (BoolOP position name) = Just $ BoolOP position name 
  get_token _        = Nothing

relOPToken :: Parsec [Token] st Token
relOPToken = tokenPrim show update_pos get_token where
  get_token (RelOP position name) = Just $ RelOP position name 
  get_token _        = Nothing

intToken :: Parsec [Token] st Token
intToken = tokenPrim show update_pos get_token where
  get_token (Int position name) = Just $ Int position name
  get_token _        = Nothing

doubleToken :: Parsec [Token] st Token
doubleToken = tokenPrim show update_pos get_token where
  get_token (Double position name) = Just $ Double position name
  get_token _    = Nothing

semiColonToken :: Parsec [Token] st Token
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon position)  = Just $ SemiColon position
  get_token _          = Nothing

assignToken :: Parsec [Token] st Token
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign position) = Just $ Assign position
  get_token _          = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos


ifToken :: Parsec [Token] st Token
ifToken = tokenPrim show update_pos get_token where
    get_token (If position) = Just $ If position
    get_token _             = Nothing

elseToken :: Parsec [Token] st Token
elseToken = tokenPrim show update_pos get_token where
    get_token (Else position) = Just $ Else position
    get_token _             = Nothing

forToken :: Parsec [Token] st Token
forToken = tokenPrim show update_pos get_token where
    get_token (For position) = Just $ For position
    get_token _             = Nothing

inToken :: Parsec [Token] st Token
inToken = tokenPrim show update_pos get_token where
    get_token (In position) = Just $ In position
    get_token _             = Nothing

whileToken :: Parsec [Token] st Token
whileToken = tokenPrim show update_pos get_token where
    get_token (While position) = Just $ While position
    get_token _             = Nothing

breakToken :: Parsec [Token] st Token
breakToken = tokenPrim show update_pos get_token where
    get_token (Break position) = Just $ Break position
    get_token _             = Nothing

continueToken :: Parsec [Token] st Token
continueToken = tokenPrim show update_pos get_token where
    get_token (Continue position) = Just $ Continue position
    get_token _             = Nothing

returnToken :: Parsec [Token] st Token
returnToken = tokenPrim show update_pos get_token where
    get_token (Return position) = Just $ Return position
    get_token _             = Nothing

readToken :: Parsec [Token] st Token
readToken = tokenPrim show update_pos get_token where
    get_token (Read position) = Just $ Read position
    get_token _             = Nothing

extractionToken :: Parsec [Token] st Token
extractionToken = tokenPrim show update_pos get_token where
    get_token (Extraction position) = Just $ Extraction position
    get_token _             = Nothing

printToken :: Parsec [Token] st Token
printToken = tokenPrim show update_pos get_token where
    get_token (Print position) = Just $ Print position
    get_token _             = Nothing

insersionToken :: Parsec [Token] st Token
insersionToken = tokenPrim show update_pos get_token where
    get_token (Insersion position) = Just $ Insersion position
    get_token _             = Nothing