module AlgebraBasica
  ( parseYCalcular
  ) where

import Parsing
import Control.Applicative hiding (many, some)
import Data.Char
import Control.Applicative ((<|>))

-- Values can be a scalar or a matrix
data Value = VNum Double
           | VMat [[Double]]
  deriving (Show, Eq)

data Expr = Lit Value
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
         <|> matrix
         <|> number

-- Parse a matrix literal like [1,2;3,4]
matrix :: Parser Expr
matrix = token $ do
  symbol "["
  rows <- matrixRows
  symbol "]"
  return (Lit (VMat rows))

matrixRows :: Parser [[Double]]
matrixRows = do
  first <- matrixRow
  rest <- many (do symbol ";"; matrixRow)
  return (first : rest)

matrixRow :: Parser [Double]
matrixRow = do
  first <- element
  rest <- many (do { many (token (char ',')); element })
  return (first : rest)

element :: Parser Double
element = token double

-- Parse a scalar (double) and wrap as Expr
number :: Parser Expr
number = token $ do
  d <- double
  return (Lit (VNum d))

-- Parse a double (with optional sign and fractional part)
double :: Parser Double
double = do
  s <- sign
  whole <- many1 digit
  frac <- (do { char '.'; dec <- many1 digit; return ('.':dec) }) <|> return ""
  let txt = whole ++ frac
  return (s * read txt)

-- Sign parser
sign :: Parser Double
sign = (do char '-'; return (-1.0)) <|> (do char '+'; return 1.0) <|> return 1.0

-- Evaluate AST with error handling, returning a Value
eval :: Expr -> Either String Value
eval (Lit v) = Right v
eval (Add a b) = do va <- eval a
                    vb <- eval b
                    addVals va vb
eval (Sub a b) = do va <- eval a
                    vb <- eval b
                    subVals va vb
eval (Mul a b) = do va <- eval a
                    vb <- eval b
                    mulVals va vb
eval (Div a b) = do va <- eval a
                    vb <- eval b
                    divVals va vb

-- Helpers implementing value arithmetic
elementWise :: (Double -> Double -> Double) -> Value -> Value -> Either String Value
elementWise f (VNum x) (VNum y) = Right (VNum (f x y))
elementWise f (VNum x) (VMat m) = Right (VMat (map (map (f x)) m))
elementWise f (VMat m) (VNum x) = Right (VMat (map (map (\y -> f y x)) m))
elementWise f (VMat a) (VMat b) =
  if dims a /= dims b then Left "Error: dimensiones incompatibles para operación elemento a elemento"
  else Right (VMat (zipWith (zipWith f) a b))

dims :: [[a]] -> (Int, Int)
dims m = (length m, if null m then 0 else length (head m))

addVals :: Value -> Value -> Either String Value
addVals = elementWise (+)

subVals :: Value -> Value -> Either String Value
subVals = elementWise (-)

-- For multiplication: scalar*scalar or scalar*matrix elementwise; matrix*matrix is standard product
mulVals :: Value -> Value -> Either String Value
mulVals (VNum x) (VNum y) = Right (VNum (x * y))
mulVals (VNum x) (VMat m) = Right (VMat (map (map (x *)) m))
mulVals (VMat m) (VNum x) = Right (VMat (map (map (* x)) m))
mulVals (VMat a) (VMat b) = multiplyMatrices a b

-- For division: for scalars and scalar-matrix do elementwise; for matrix/matrix use matrix multiplication per user rules
divVals :: Value -> Value -> Either String Value
divVals (VNum x) (VNum y) = if y == 0 then Left "Error: división por cero" else Right (VNum (x / y))
divVals (VNum x) (VMat m) = Right (VMat (map (map (\y -> if y == 0 then 1/0 else x / y)) m))
divVals (VMat m) (VNum x) = if x == 0 then Left "Error: división por cero" else Right (VMat (map (map (/ x)) m))
divVals (VMat a) (VMat b) = multiplyMatrices a b -- per specified rule: '/' behaves like matrix product

-- Matrix multiplication (standard)
multiplyMatrices :: [[Double]] -> [[Double]] -> Either String Value
multiplyMatrices a b =
  let (ar, ac) = dims a
      (br, bc) = dims b
  in if ac /= br then Left "Error: dimensiones incompatibles para producto matricial"
     else Right $ VMat [[ sum $ zipWith (*) rowA colB | colB <- transpose b ] | rowA <- a ]

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- Convert Value to a printable string (similar format to input)
showValue :: Value -> String
showValue (VNum x) = show x
showValue (VMat m) = "[" ++ rowsStr ++ "]"
  where rowsStr = concat $ intersperse ";" (map rowStr m)
        rowStr r = concat $ intersperse "," (map show r)

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

-- Public: parse string and compute value
parseYCalcular :: String -> Either String String
parseYCalcular s = case parse (token expr) s of
  [] -> Left "Error de parseo"
  [(e,rest)] -> if all isSpace rest
                  then case eval e of
                         Left err -> Left err
                         Right v  -> Right (showValue v)
                  else Left ("Entrada no consumida: " ++ show rest)
  _ -> Left "Ambigüedad en parseo"
