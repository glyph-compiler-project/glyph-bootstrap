module ConcreteSyntax where

data Cell      = Cell [Cell] | Atom String deriving (Show)
data Concrete  = Concrete [Cell] deriving (Show)
