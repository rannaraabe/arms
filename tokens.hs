module Tokens where

import Lexer
import Text.Parsec

-- parsers para os tokens
intTypeToken :: Parsec [Token] st Token
intTypeToken = tokenPrim show update_pos get_token where
  get_token (IntType position) = Just $ IntType position
  get_token _    = Nothing

doubleTypeToken :: Parsec [Token] st Token
doubleTypeToken = tokenPrim show update_pos get_token where
  get_token (DoubleType position) = Just $ DoubleType position
  get_token _    = Nothing

complexTypeToken :: Parsec [Token] st Token
complexTypeToken = tokenPrim show update_pos get_token where
  get_token (ComplexType position) = Just $ ComplexType position
  get_token _    = Nothing

matrixTypeToken :: Parsec [Token] st Token
matrixTypeToken = tokenPrim show update_pos get_token where
  get_token (MatrixType position) = Just $ MatrixType position
  get_token _    = Nothing

arrayTypeToken :: Parsec [Token] st Token
arrayTypeToken = tokenPrim show update_pos get_token where
  get_token (ArrayType position) = Just $ ArrayType position
  get_token _    = Nothing

booleanTypeToken :: Parsec [Token] st Token
booleanTypeToken = tokenPrim show update_pos get_token where
  get_token (BooleanType position) = Just $ BooleanType position
  get_token _    = Nothing

charTypeToken :: Parsec [Token] st Token
charTypeToken = tokenPrim show update_pos get_token where
  get_token (CharType position) = Just $ CharType position
  get_token _    = Nothing

stringTypeToken :: Parsec [Token] st Token
stringTypeToken = tokenPrim show update_pos get_token where
  get_token (StringType position) = Just $ StringType position
  get_token _    = Nothing

voidTypeToken :: Parsec [Token] st Token
voidTypeToken = tokenPrim show update_pos get_token where
  get_token (VoidType position) = Just $ VoidType position
  get_token _    = Nothing

openParentheseToken :: Parsec [Token] st Token
openParentheseToken = tokenPrim show update_pos get_token where
  get_token (OpenParenthese position) = Just $ OpenParenthese position
  get_token _              = Nothing

closeParentheseToken :: Parsec [Token] st Token
closeParentheseToken = tokenPrim show update_pos get_token where
  get_token (CloseParenthese position) = Just $ CloseParenthese position
  get_token _               = Nothing

openBracketToken :: Parsec [Token] st Token
openBracketToken = tokenPrim show update_pos get_token where
  get_token (OpenBracket position) = Just $ OpenBracket position
  get_token _              = Nothing

closeBracketToken :: Parsec [Token] st Token
closeBracketToken = tokenPrim show update_pos get_token where
  get_token (CloseBracket position) = Just $ CloseBracket position
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

semiColonToken :: Parsec [Token] st Token
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon position)  = Just $ SemiColon position
  get_token _      = Nothing

commaToken :: Parsec [Token] st Token
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma position) = Just $ Comma position
  get_token _    = Nothing

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

returnToken :: Parsec [Token] st Token
returnToken = tokenPrim show update_pos get_token where
  get_token (Return position) = Just $ Return position
  get_token _             = Nothing

breakToken :: Parsec [Token] st Token
breakToken = tokenPrim show update_pos get_token where
  get_token (Break position) = Just $ Break position
  get_token _             = Nothing

continueToken :: Parsec [Token] st Token
continueToken = tokenPrim show update_pos get_token where
  get_token (Continue position) = Just $ Continue position
  get_token _             = Nothing

importToken :: Parsec [Token] st Token
importToken = tokenPrim show update_pos get_token where
  get_token (Import position) = Just $ Import position
  get_token _    = Nothing

structToken :: Parsec [Token] st Token
structToken = tokenPrim show update_pos get_token where
  get_token (Struct position) = Just $ Struct position
  get_token _    = Nothing

mainToken :: Parsec [Token] st Token
mainToken = tokenPrim show update_pos get_token where
  get_token (Main position) = Just $ Main position
  get_token _    = Nothing

printToken :: Parsec [Token] st Token
printToken = tokenPrim show update_pos get_token where
    get_token (Print position) = Just $ Print position
    get_token _             = Nothing

insertionToken :: Parsec [Token] st Token
insertionToken = tokenPrim show update_pos get_token where
    get_token (Insertion position) = Just $ Insertion position
    get_token _             = Nothing

readToken :: Parsec [Token] st Token
readToken = tokenPrim show update_pos get_token where
    get_token (Read position) = Just $ Read position
    get_token _             = Nothing

extractionToken :: Parsec [Token] st Token
extractionToken = tokenPrim show update_pos get_token where
    get_token (Extraction position) = Just $ Extraction position
    get_token _             = Nothing

assignToken :: Parsec [Token] st Token
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign position) = Just $ Assign position
  get_token _          = Nothing

intToken :: Parsec [Token] st Token
intToken = tokenPrim show update_pos get_token where
  get_token (Int position name) = Just $ Int position name
  get_token _        = Nothing

doubleToken :: Parsec [Token] st Token
doubleToken = tokenPrim show update_pos get_token where
  get_token (Double position name) = Just $ Double position name
  get_token _    = Nothing

booleanToken :: Parsec [Token] st Token
booleanToken = tokenPrim show update_pos get_token where
  get_token (Boolean position name) = Just $ Boolean position name
  get_token _    = Nothing

charToken :: Parsec [Token] st Token
charToken = tokenPrim show update_pos get_token where
  get_token (Char position name) = Just $ Char position name
  get_token _    = Nothing

stringToken :: Parsec [Token] st Token
stringToken = tokenPrim show update_pos get_token where
  get_token (String position name) = Just $ String position name
  get_token _    = Nothing

symArithLeftToken :: Parsec [Token] st Token
symArithLeftToken = tokenPrim show update_pos get_token where
   get_token (SymArithLeft position name) = Just $ SymArithLeft position name
   get_token _    = Nothing

symArithRightToken :: Parsec [Token] st Token
symArithRightToken = tokenPrim show update_pos get_token where
   get_token (SymArithRight position name) = Just $ SymArithRight position name
   get_token _    = Nothing

symArithUnaryToken :: Parsec [Token] st Token
symArithUnaryToken = tokenPrim show update_pos get_token where
   get_token (SymArithUnary position name) = Just $ SymArithUnary position name
   get_token _    = Nothing

symMatrixToken :: Parsec [Token] st Token
symMatrixToken = tokenPrim show update_pos get_token where
   get_token (SymMatrix position name) = Just $ SymMatrix position name
   get_token _    = Nothing

symRelToken :: Parsec [Token] st Token
symRelToken = tokenPrim show update_pos get_token where
   get_token (SymRel position name) = Just $ SymRel position name
   get_token _    = Nothing

symCompToken :: Parsec [Token] st Token
symCompToken = tokenPrim show update_pos get_token where
   get_token (SymComp position name) = Just $ SymComp position name
   get_token _    = Nothing

symBoolToken :: Parsec [Token] st Token
symBoolToken = tokenPrim show update_pos get_token where
   get_token (SymBool position name) = Just $ SymBool position name
   get_token _    = Nothing

symMatrixArithToken :: Parsec [Token] st Token
symMatrixArithToken = tokenPrim show update_pos get_token where
   get_token (SymMatrixArith position name) = Just $ SymMatrixArith position name
   get_token _    = Nothing

identifierToken :: Parsec [Token] st Token
identifierToken = tokenPrim show update_pos get_token where
  get_token (Identifier position name)  = Just $ Identifier position name
  get_token _          = Nothing