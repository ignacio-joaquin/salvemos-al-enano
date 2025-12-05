module ScientificParser where

import Parsing
import Control.Applicative hiding (many)
import Data.Char

-- Parser for scientific notation: [+|-]0.mantissa E [+|-]exponent
scientificNotation :: Parser Float
scientificNotation = do
    sign1 <- (char '+' >> return 1) <|> (char '-' >> return (-1)) <|> return 1
    char '0'
    char '.'
    mantissa <- many1 digit
    char 'E' <|> char 'e'
    sign2 <- (char '+' >> return 1) <|> (char '-' >> return (-1)) <|> return 1
    exponent <- many1 digit
    let mantissaVal = read ("0." ++ mantissa) :: Float
        expVal = read exponent :: Int
        result = sign1 * mantissaVal * (10 ** fromIntegral (sign2 * expVal))
    return result

-- Parser for a scalar value (with token spacing)
scalar :: Parser Float
scalar = token scientificNotation

-- Parser for matrix/vector elements
matrixElement :: Parser Float
matrixElement = token scientificNotation

-- Parser for a row of elements
matrixRow :: Parser [Float]
matrixRow = do
    first <- matrixElement
    rest <- many (do
        many (token (char ','))
        matrixElement)
    return (first : rest)

-- Parser for matrix/vector
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

-- Parser that tries matrix first, then scalar
value :: Parser [[Float]]
value = matrix <|> (do x <- scalar; return [[x]])

-- Simple parse functions
parseScalar :: String -> [(Float, String)]
parseScalar = parse scalar

parseMatrix :: String -> [([[Float]], String)]
parseMatrix = parse matrix

parseValue :: String -> [([[Float]], String)]
parseValue = parse value