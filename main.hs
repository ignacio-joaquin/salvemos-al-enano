module Main where

import System.IO
import System.Exit (exitSuccess)

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	putStrLn "REPL simple. Comandos: :show, :clear, :parse, :quit"
	bucle []

bucle :: [String] -> IO ()
bucle almacen = do
	putStr ">> "
	hFlush stdout
	entrada <- getLine
	case entrada of
		":quit" -> putStrLn "Adiós." >> exitSuccess
		":show" -> do
			putStrLn $ "Almacén (" ++ show (length almacen) ++ " líneas):"
			mapM_ putStrLn (reverse almacen)
			bucle almacen
		":clear" -> do
			putStrLn "Almacén limpiado."
			bucle []
		":parse" -> do
			putStrLn "Parseando (placeholder)..."
			parsear (reverse almacen)
			bucle almacen
		_ -> bucle (entrada : almacen)

parsear :: [String] -> IO ()
parsear lineas = do
	putStrLn "---- Contenido ----"
	putStrLn (unlines lineas)
	putStrLn "---- Fin ----"
	putStrLn $ "(Placeholder parse) Líneas: " ++ show (length lineas)

