module RawSyntax where

data Cons = Cons Cons Cons | Lit String | Nil deriving (Show)
data Prog = Prog [Cons] deriving (Show)