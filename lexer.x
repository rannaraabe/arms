{
module Lexer (Token(..), AlexPosn(..), alexScanTokens, token_posn, getTokens) where
import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$graphic = $printable # $white
@string = \" *($graphic # \")* \"

tokens :-

  $white+                         ;
  "//".*.                         ;
  int                             { \p s -> IntType p }
  double                          { \p s -> DoubleType p }
  complex                         { \p s -> ComplexType p }
  matrix                          { \p s -> MatrixType p }
  array                           { \p s -> ArrayType p }
  boolean                         { \p s -> BooleanType p }
  string                          { \p s -> StringType p }
  void                            { \p s -> VoidType p }
  struct                          { \p s -> Struct p }
  "("                             { \p s -> OpenParenthese p}
  ")"                             { \p s -> CloseParenthese p}
  "["                             { \p s -> OpenBracket p}
  "]"                             { \p s -> CloseBracket p}
  if                              { \p s -> If p }
  else                            { \p s -> Else p }
  "{"                             { \p s -> OpenBracer p}
  "}"                             { \p s -> CloseBracer p}
  for                             { \p s -> For p}
  return                          { \p s -> Return p}
  break                           { \p s -> Break p}
  continue                        { \p s -> Continue p}
  ":"                             { \p s -> Colon p }
  ";"                             { \p s -> SemiColon p }
  while                           { \p s -> While p }
  main                            { \p s -> Main p }
  print                           { \p s -> Print p }
  read                            { \p s -> Read p}
  ","                             { \p s -> Comma p}
  ">>"                            { \p s -> Extraction p}
  "<<"                            { \p s -> Insertion p}
  "="                             { \p s -> Assign p}
  in                              { \p s -> In p }
  [\-]* $digit+                   { \p s -> Int p (read s) }
  [\-]* $digit+ \. $digit+        { \p s -> Double p (read s) }
  ("--" | "++")                   { \p s -> Sym p s }
  [\+ \- \* \/ \%]                { \p s -> Sym p s }
  (and | or | not | xor | "**")   { \p s -> Sym p s}
  ("==" | "!=" | "<=" | ">=" | ">" | "<")  { \p s -> Sym p s}
  true                            { \p s -> TrueSym p}
  false                           { \p s -> FalseSym p}
  $alpha [$alpha $digit \_ ]*     { \p s -> Identifier p s }
  @string                         { \p s -> String p s }
{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:
-- The token type:
data Token =
  IntType AlexPosn           |
  VoidType AlexPosn          |
  DoubleType AlexPosn        |
  ComplexType AlexPosn       |
  MatrixType AlexPosn        |
  ArrayType AlexPosn         |
  BooleanType AlexPosn       |
  CharType AlexPosn          |
  In  AlexPosn               |
  For AlexPosn               |
  If AlexPosn                |
  Else AlexPosn              |
  Return AlexPosn            |
  Break AlexPosn             |
  Continue AlexPosn          |
  SemiColon AlexPosn         |
  OpenParenthese AlexPosn    |
  CloseParenthese AlexPosn   |
  OpenBracket AlexPosn       |
  CloseBracket AlexPosn      |
  OpenBracer AlexPosn        |
  CloseBracer AlexPosn       |
  Colon AlexPosn             |
  While AlexPosn             |
  Main AlexPosn              |
  Print AlexPosn             |
  Read AlexPosn              |
  Comma AlexPosn             |
  Extraction AlexPosn        |
  Insertion AlexPosn         |
  Assign AlexPosn            | 
  Int AlexPosn Int           |
  Double AlexPosn Double     |
  SymArith AlexPosn Char     |
  Sym AlexPosn String        |
  TrueSym AlexPosn           |
  FalseSym AlexPosn          |
  BoolOP AlexPosn String     |
  RelOP AlexPosn String      |
  Struct AlexPosn            |
  Complex AlexPosn (Token, Token)|
  String AlexPosn String          |
  StringType AlexPosn          |
  Array AlexPosn [Token]    |
  Matrix AlexPosn [[Token]] |
  Identifier AlexPosn String


instance Show Token where
  show (IntType p) = "int"
  show (DoubleType p) = "double"
  show (ComplexType p) = "complex"
  show (ArrayType p) = "array"
  show (MatrixType p) = "matrix"
  show (BooleanType _) = "boolean"
  show (CharType p) = "char"
  show (In  p) = "in"
  show (For p) = "for"
  show (If p) = "if"
  show (Else p) = "else"
  show (Return p) = "return"
  show (Break p) = "break"
  show (Continue p) = "continue"
  show (SemiColon p) = ";"
  show (OpenParenthese p) = "( " ++ show p
  show (CloseParenthese p) = ")"
  show (OpenBracket p) =    "["
  show (CloseBracket p) =   "]"
  show (OpenBracer p) =     "{"
  show (CloseBracer p) =    "}"
  show (Colon p) =          ":"
  show (While p) =          "while"
  show (Main p) =           "main" ++ show p
  show (Print p) =          "print"
  show (Read p) =           "read"
  show (Comma p) =          ","
  show (Extraction p) =     ">>"
  show (Insertion p) =      "<<"
  show (Assign p) = "="
  show (Int p x) = "INT " ++ show x
  show (Double p x) = "DOUBLE " ++ show x
  show (SymArith p s) = s:[]
  show (Sym p s) = s
  show (TrueSym p) = "true"
  show (FalseSym p) = "false"
  show (String _ "") = "\"\""
  show (String _ s) = s
  show (StringType _) = "string"
  show (Identifier _ s) = s
  show (Struct _) = "struct"
  show (VoidType p) = "void"


token_posn (IntType p) = p
token_posn (DoubleType p) = p
token_posn (ComplexType p) = p
token_posn (ArrayType p) = p
token_posn (MatrixType p) = p
token_posn (BooleanType p) = p
token_posn (CharType p) = p
token_posn (OpenParenthese p) = p
token_posn (CloseParenthese p) = p
token_posn (OpenBracket p) = p
token_posn (CloseBracket p) = p
token_posn (If p) = p
token_posn (Else p) = p
token_posn (OpenBracer p) = p
token_posn (CloseBracer p) = p
token_posn (For p) = p
token_posn (Return p) = p
token_posn (Break p) = p
token_posn (Continue p) = p
token_posn (Colon p) = p
token_posn (SemiColon p) = p
token_posn (While p) = p
token_posn (Main p) = p
token_posn (Print p) = p
token_posn (Read p) = p
token_posn (Comma p) = p
token_posn (Extraction p) = p
token_posn (Insertion p) = p
token_posn (Assign p) = p
token_posn (In p) = p
token_posn (TrueSym p) = p
token_posn (FalseSym p) = p
token_posn (BoolOP p _) = p
token_posn (RelOP p _) = p
token_posn (Int p _) = p
token_posn (Double p _) = p
token_posn (SymArith p _) = p
token_posn (Sym p _) = p
token_posn (Identifier p _) = p
token_posn (String p _) = p
token_posn (StringType p) = p
token_posn (Struct p) = p
token_posn (VoidType p) = p

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do fh <- openFile fn ReadMode
                     s <- hGetContents fh
                     return (alexScanTokens s)
}