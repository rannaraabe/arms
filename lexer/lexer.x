{
    module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}


%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$graphic = $printable # $white
$string = $printable


tokens :-

  $white+                         ;
  "//".*.                         ;
  int                             { \p s -> IntType p }
  double                          { \p s -> DoubleType p }
  complex                         { \p s -> ComplexType p }
  matrix                          { \p s -> MatrixType p }
  array                           { \p s -> ArrayType p }
  boolean                         { \p s -> BooleanType p }
  char                            { \p s -> CharType p }
  void                            { \p s -> VoidType p }
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
  \" ($string # \")* \"           { \p s -> String p ( drop 1 ( take ((length s) - 1) s) ) }
  \' ($graphic # \") \'           { \p s -> Char p ( head ( drop 1 ( take ((length s) - 1) s) ) ) }
  ("true" | "false")              { \p s -> Boolean p s }
  [\+ \- \* \/ \%]                { \p s -> SymArithLeft p (head s) }
  ("**")                          { \p s -> SymArithRight p s }
  ("--" | "++")                   { \p s -> SymArithUnary p s }
  ("rot" | "inv")                       { \p s -> SymMatrix p s }
  struct                        { \p s -> Struct p }
  (\< | \> | "<=" | ">=")         { \p s -> SymRel p s }
  ("==" | "!=")                   { \p s -> SymComp p s }
  ("and" | "or" | "not" | "xor")  { \p s -> SymBool p s }
  ("mod" | "det")                 { \p s -> SymMatrixArith p s }
  $alpha [$alpha $digit \_ ]*     { \p s -> Identifier p s }
{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:
-- The token type:
data Token =
  IntType AlexPosn                |
  DoubleType AlexPosn             |
  ComplexType AlexPosn            |
  MatrixType AlexPosn             |
  ArrayType AlexPosn              |
  BooleanType AlexPosn            |
  CharType AlexPosn               |
  VoidType AlexPosn               |
  In  AlexPosn                    |
  For AlexPosn                    |
  If AlexPosn                     |
  Else AlexPosn                   |
  Return AlexPosn                 |
  Break AlexPosn                  |
  Continue AlexPosn               |
  SemiColon AlexPosn              |
  OpenParenthese AlexPosn         |
  CloseParenthese AlexPosn        |
  OpenBracket AlexPosn            |
  CloseBracket AlexPosn           |
  OpenBracer AlexPosn             |
  CloseBracer AlexPosn            |
  Colon AlexPosn                  |
  While AlexPosn                  |
  Main AlexPosn                   |
  Import AlexPosn                 |
  Print AlexPosn                  |
  Read AlexPosn                   |
  Comma AlexPosn                  |
  Extraction AlexPosn             |
  Insersion AlexPosn              |
  Assign AlexPosn                 | 
  Int AlexPosn Int                |
  Double AlexPosn Double          |
  SymArithLeft AlexPosn Char      |
  SymArithRight AlexPosn String   |
  String AlexPosn String          |
  Char AlexPosn Char              |
  Boolean AlexPosn String         |
  SymArithUnary AlexPosn String   |
  SymMatrix AlexPosn String       |
  Struct AlexPosn                 |
  SymRel AlexPosn String          |
  SymComp AlexPosn String         |
  SymBool AlexPosn String         |
  SymMatrixArith AlexPosn String  |
  Identifier AlexPosn String 
  deriving (Eq,Show)

token_posn (IntType p) = p
token_posn (DoubleType p) = p
token_posn (ComplexType p) = p
token_posn (MatrixType p) = p
token_posn (ArrayType p) = p
token_posn (BooleanType p) = p
token_posn (CharType p) = p
token_posn (VoidType p) = p
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
token_posn (SymArithLeft p _) = p
token_posn (SymArithRight p _) = p
token_posn (String p _) = p
token_posn (Char p _) = p
token_posn (Boolean p _) = p
token_posn (SymArithUnary p _) = p
token_posn (SymMatrix p _) = p
token_posn (Struct p) = p
token_posn (SymRel p _) = p
token_posn (SymComp p _) = p
token_posn (SymBool p _) = p
token_posn (SymMatrixArith p _) = p
token_posn (Identifier p _) = p


main = do
  s <- getContents
  print (alexScanTokens s)

}