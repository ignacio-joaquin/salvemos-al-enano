module Main where

import System.IO
-- no System.Exit: avoid ExitSuccess exception when run inside GHCi
import Algebra (parseYCalcular)

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
    ":quit" -> putStrLn "Adiós." >> return ()
    "" -> loop
    _ -> do
      case parseYCalcular linea of
        Left err -> putStrLn ("Error: " ++ err)
        Right v  -> putStrLn v
      loop

