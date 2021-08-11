{
module Lexer (Token(..), AlexPosn(..), alexScanTokens, token_posn, getTokens) where
import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$graphic = $printable # $white


tokens :-

  $white+                         ;
  "--".*.                         ;
  int                             { \p s -> IntType p }
  double                          { \p s -> DoubleType p }
  complex                         { \p s -> ComplexType p }
  num_array                       { \p s -> NumArrayType p }
  num_matrix                      { \p s -> NumMatrixType p }
  array                           { \p s -> ArrayType p }
  boolean                         { \p s -> BooleanType p }
  char                            { \p s -> CharType p }
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
  import                          { \p s -> Import p}
  print                           { \p s -> Print p }
  read                            { \p s -> Read p}
  ","                             { \p s -> Comma p}
  ">>"                            { \p s -> Extraction p}
  "<<"                            { \p s -> Insersion p}
  "="                             { \p s -> Assign p}
  in                              { \p s -> In p }
  [\-]* $digit+                   { \p s -> Int p (read s) }
  [\-]* $digit+ \. $digit+        { \p s -> Double p (read s) }
  [\+ \- \* \/ \%]                { \p s -> SymArith p (head s) }
  $alpha [$alpha $digit \_ ]*     { \p s -> Identifier p s }
{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:
-- The token type:
data Token =
  IntType AlexPosn           |
  DoubleType AlexPosn        |
  ComplexType AlexPosn       |
  NumArrayType AlexPosn      |
  NumMatrixType AlexPosn     |
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
  Import AlexPosn            |
  Print AlexPosn             |
  Read AlexPosn              |
  Comma AlexPosn             |
  Extraction AlexPosn        |
  Insersion AlexPosn         |
  Assign AlexPosn            | 
  Int AlexPosn Int           |
  Double AlexPosn Double     |
  SymArith AlexPosn Char     |
  Identifier AlexPosn String 
  deriving (Eq,Show)

token_posn (IntType p) = p
token_posn (DoubleType p) = p
token_posn (ComplexType p) = p
token_posn (NumArrayType p) = p
token_posn (NumMatrixType p) = p
token_posn (ArrayType p) = p
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
token_posn (Import p) = p
token_posn (Print p) = p
token_posn (Read p) = p
token_posn (Comma p) = p
token_posn (Extraction p) = p
token_posn (Insersion p) = p
token_posn (Assign p) = p
token_posn (In p) = p
token_posn (Int p _) = p
token_posn (Double p _) = p
token_posn (SymArith p _) = p
token_posn (Identifier p _) = p

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do fh <- openFile fn ReadMode
                     s <- hGetContents fh
                     return (alexScanTokens s)
}