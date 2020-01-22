module Main where

import System.Environment

import RawParser

main :: IO ()
main = do
  args <- getArgs
  if (elem "--repl" args) then interpret else (compile args) 

compile :: [String] -> IO ()
compile args = putStrLn "Compiling!"

interpret :: IO ()
interpret = interact $ \s -> case parseRawProg s of
    Left err -> show err
    Right val -> show val
