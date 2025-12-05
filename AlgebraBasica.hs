module AlgebraBasica
  ( parseYCalcular
  ) where

import Parsing
import Control.Applicative hiding (many, some)
import Data.Char
import Control.Applicative ((<|>))

-- AST
data Expr = Num Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
  deriving (Show, Eq)

-- Parser that returns Expr
expr :: Parser Expr
expr = do t <- term
          rest t
  where
    rest t = (do symbol "+"
                 e <- expr
                 return (Add t e))
             <|> (do symbol "-"
                     e <- expr
                     return (Sub t e))
             <|> return t

term :: Parser Expr
term = do f <- factor
          rest f
  where
    rest f = (do symbol "*"
                 t <- term
                 return (Mul f t))
             <|> (do symbol "/"
                     t <- term
                     return (Div f t))
             <|> return f

factor :: Parser Expr
factor = (do symbol "("
             e <- expr
             symbol ")"
             return e)
         <|> number

number :: Parser Expr
number = token $ do
  s <- sign
  whole <- many1 digit
  frac <- (do char '.'
               dec <- many1 digit
               return ('.':dec)) <|> return ""
  let txt = whole ++ frac
  return (Num (s * read txt))
  where
    sign = (do char '-'; return (-1.0)) <|> (do char '+'; return 1.0) <|> return 1.0

-- Evaluate AST with error handling
eval :: Expr -> Either String Double
eval (Num x) = Right x
eval (Add a b) = do x <- eval a
                    y <- eval b
                    return (x + y)
eval (Sub a b) = do x <- eval a
                    y <- eval b
                    return (x - y)
eval (Mul a b) = do x <- eval a
                    y <- eval b
                    return (x * y)
eval (Div a b) = do x <- eval a
                    y <- eval b
                    if y == 0 then Left "Error: división por cero" else return (x / y)

-- Public: parse string and compute value
parseYCalcular :: String -> Either String Double
parseYCalcular s = case parse (token expr) s of
  [] -> Left "Error de parseo"
  [(e,rest)] -> if all isSpace rest then eval e else Left ("Entrada no consumida: " ++ show rest)
  _ -> Left "Ambigüedad en parseo"
