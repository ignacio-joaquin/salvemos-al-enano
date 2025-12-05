module Main where

import System.IO
import System.Exit (exitSuccess)
import AlgebraBasica (parseYCalcular)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Calculadora REPL. Escribí una expresión y presioná Enter. Comando: :quit"
  loop

loop :: IO ()
loop = do
  putStr ">> "
  hFlush stdout
  linea <- getLine
  case linea of
    ":quit" -> putStrLn "Adiós." >> exitSuccess
    "" -> loop
    _ -> do
      case parseYCalcular linea of
        Left err -> putStrLn ("Error: " ++ err)
        Right v  -> putStrLn (show v)
      loop

