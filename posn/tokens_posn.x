{
  module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-

  $white+                         ;
  "--".*.                         ;
  "("                               { \p s -> OpenParenthese p}
  ")"                               { \p s -> CloseParenthese p}
  if                              { \p s -> If p }
  else                            { \p s -> Else p }
  "{"                               { \p s -> OpenBracer p}
  "}"                               { \p s -> CloseBracer p}
  for                             { \p s -> For p}
  return                          {\p s -> Return p}
  ":"                             {\p s -> Colon p }
  ";"                             {\p s -> SemiColon p }
  while                           {\p s -> While p }
  main                            {\p s -> Main p }
  import                          {\p s -> Import p}
  print                           {\p s -> Print p }
  read                            {\p s -> Read p}
  ","                             {\p s -> Comma p}
  ">>"                            {\p s -> Extraction p}
  "<<"                            {\p s -> Insersion p}
  in                              { \p s -> In p }
  $digit+	                        { \p s -> Int p (read s) }
  [\=\+\-\*\/]                    { \p s -> Sym p (head s) }
  $alpha [$alpha $digit \_ \']*   { \p s -> Identfier p s }
{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  In  AlexPosn        |
  Sym AlexPosn Char   |
  Identfier AlexPosn String |
  For AlexPosn        |
  If AlexPosn         |
  Else AlexPosn       |
  Return AlexPosn     |
  SemiColon AlexPosn  |
  OpenParenthese AlexPosn  |
  CloseParenthese AlexPosn  |
  OpenBracer AlexPosn  |
  CloseBracer AlexPosn  |
  Colon AlexPosn |
  While AlexPosn |
  Main AlexPosn |
  Import AlexPosn |
  Print AlexPosn |
  Read AlexPosn |
  Comma AlexPosn |
  Extraction AlexPosn |
  Insersion AlexPosn |
  Int AlexPosn Int
  deriving (Eq,Show)

token_posn (In p) = p
token_posn (Sym p _) = p
token_posn (Identfier p _) = p
token_posn (Int p _) = p
token_posn (SemiColon p) = p
token_posn (OpenParenthese p) = p
token_posn (CloseParenthese p) = p
token_posn (OpenBracer p) = p
token_posn (CloseBracer p) = p
token_posn (Colon p) = p
token_posn (While p) = p
token_posn (Main p) = p
token_posn (Import p) = p
token_posn (Print p) = p
token_posn (Read p) = p
token_posn (Comma p) = p
token_posn (Extraction p) = p
token_posn (Insersion p) = p


main = do
  s <- getContents
  print (alexScanTokens s)
}