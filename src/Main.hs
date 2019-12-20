module Main where

import System.Environment

import Parser

main :: IO ()
main = do
  args <- getArgs
  if (elem "--repl" args) then (interpret args) else (compile args) 

compile :: [String] -> IO ()
compile args = putStrLn "Compiling!"

interpret :: [String] -> IO ()
interpret args = putStrLn "Interpreting!"