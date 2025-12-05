module ScientificParser where

import Parsing
import Control.Applicative hiding (many)
import Data.Char

-- Data types
data Value = Scalar Float
           | Matrix [[Float]]
           deriving (Show, Eq)

-- Parser for scientific notation: [+|-]0.mantissa E [+|-]exponent
scientificNotation :: Parser Float
scientificNotation = do
    -- Parse optional sign for mantissa
    sign1 <- option '+' (char '+' <|> char '-')
    
    -- Parse mandatory "0."
    char '0'
    char '.'
    
    -- Parse mantissa (at least one digit)
    mantissa <- many1 digit
    
    -- Parse 'E'
    char 'E' <|> char 'e'
    
    -- Parse optional sign for exponent
    sign2 <- option '+' (char '+' <|> char '-')
    
    -- Parse exponent (at least one digit)
    exponent <- many1 digit
    
    -- Build the number
    let mantissaVal = read ("0." ++ mantissa) :: Float
        expVal = read exponent :: Int
        signedMantissa = if sign1 == '-' then -mantissaVal else mantissaVal
        signedExp = if sign2 == '-' then -expVal else expVal
        result = signedMantissa * (10 ** fromIntegral signedExp)
    
    return result

-- Helper: parse optional value
option :: a -> Parser a -> Parser a
option x p = p <|> return x

-- Parser for a scalar value (with token spacing)
scalar :: Parser Float
scalar = token scientificNotation

-- Parser for matrix/vector elements
-- Elements can be separated by spaces or commas
matrixElement :: Parser Float
matrixElement = token scientificNotation

-- Parser for a row of elements
-- Elements separated by spaces and/or commas
matrixRow :: Parser [Float]
matrixRow = do
    first <- matrixElement
    rest <- many (do
        many (token (char ','))  -- optional commas
        matrixElement)
    return (first : rest)

-- Parser for matrix/vector
-- Rows separated by semicolons
matrix :: Parser [[Float]]
matrix = do
    symbol "["
    space
    first <- matrixRow
    rest <- many (do
        symbol ";"
        matrixRow)
    space
    symbol "]"
    return (first : rest)

-- Parser for any value (scalar or matrix)
value :: Parser Value
value = (Matrix <$> matrix) <|> (Scalar <$> scalar)

-- Main parser entry point
parseValue :: String -> [(Value, String)]
parseValue = parse value

-- Convenience function to parse and return just the result
parseValueComplete :: String -> Maybe Value
parseValueComplete input = 
    case parse value input of
        [(v, "")] -> Just v
        _         -> Nothing

