module RawSyntax where

data Cons = Cons Cons Cons | Lit String | Nil
data Prog = Prog [Cons]