module ParserSintatico where

import Lexer
import TabelaSimbolos
import Tokens
import Text.Parsec

typeToken :: ParsecT [Token] Estado IO [Token]
typeToken = do a <- intTypeToken
                <|> doubleTypeToken
                <|> boolTypeToken
                <|> stringTypeToken
                <|> voidTypeToken
               return ([a])

varDeclSint :: ParsecT [Token] Estado IO [Token]
varDeclSint = do
          name <- identifierToken
          b <- assignToken
          expr <- expressionSint
          o <- remainingDeclSint
          d <- colonToken
          g <- typeToken
          return ((name:b:expr) ++ (d:g))

remainingDeclSint:: ParsecT [Token] Estado IO [Token]
remainingDeclSint = (do
          c <- commaToken
          name <- identifierToken
          a <- assignToken
          expr <- expressionSint
          r <- remainingDeclSint
          return (c:name:a:expr ++ r)) <|> return []

varAssSint :: ParsecT [Token] Estado IO [Token]
varAssSint = do
    name <- identifierToken
    a <- assignToken
    val <- expressionSint
    return (name:a:val)

arrayDeclSint :: ParsecT [Token] Estado IO [Token]
arrayDeclSint = do 
          name <- identifierToken
          a <- assignToken
          vec <- literalArraySint
          cl <- colonToken
          ar <- arrayToken
          ap <- openParentheseToken
          t <- typeToken
          cp <- closeParentheseToken
          return (name:a:vec ++ ar:ap:t ++ [cp])

returnRuleSint :: ParsecT [Token] Estado IO [Token]
returnRuleSint = do
                a <- returnToken
                b <- expressionSint
                semiColonToken
                return (a:b)


arrayAssSint :: ParsecT [Token] Estado IO [Token]
arrayAssSint = do
    i <- identifierToken
    a <- assignToken
    l <- expressionSint
    return (i:a:l)

expressionSint :: ParsecT [Token] Estado IO [Token]
expressionSint = do
          a <- literalIdentifierSint
          result <- remainingExpressionsSint
          return (a : result)

remainingExpressionsSint :: ParsecT [Token] Estado IO [Token]
remainingExpressionsSint = (do
                a <- symToken
                b <- literalIdentifierSint
                c <- remainingExpressionsSint
                return $ a:c) <|> return []

literalIdentifierSint :: ParsecT [Token] Estado IO Token
literalIdentifierSint = do
  a <- doubleToken <|> intToken <|> trueToken <|> falseToken <|> identifierToken <|> stringToken
  return a

literalArraySint ::ParsecT [Token] Estado IO [Token]
literalArraySint = do 
          ob <- openBracketToken
          first <- literalIdentifierSint
          rest <- remainingArraysSint
          return (first:rest)

remainingArraysSint:: ParsecT [Token] Estado IO [Token]
remainingArraysSint = f <|> g
      where f = do
              comma <- commaToken
              i <- literalIdentifierSint
              rest <- f <|> g
              return (comma:[i] ++ rest)
            g = do
              c <- closeBracketToken
              return [c]

inputSint :: ParsecT [Token] Estado IO [Token]
inputSint = do
            a <- readToken
            b <- remainingInputSint
            return (a:b)

remainingInputSint :: ParsecT [Token] Estado IO [Token]
remainingInputSint = do
                    a <- extractionToken
                    b <- expressionSint -- <expr>
                    c <- remainingInputSint <|> (return []) -- ver se isso ta certo
                    return (b ++ (String (AlexPn 1 2 3) " "):c)




outputSint :: ParsecT [Token] Estado IO [Token]
outputSint = do
            a <- printToken
            b <- remainingOutputSint
            return (a:b)

remainingOutputSint :: ParsecT [Token] Estado IO [Token]
remainingOutputSint = do
                    a <- insertionToken
                    b <- expressionSint -- <expr>
                    c <- remainingOutputSint <|> (return []) -- ver se isso ta certo
                    return (b ++ (String (AlexPn 1 2 3) " "):c)