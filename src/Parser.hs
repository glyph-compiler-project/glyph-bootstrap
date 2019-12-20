module Parser where

import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import RawSyntax
import Syntax

-- Create language definition with some presets
languageDef = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedNames   = []
  , Token.reservedOpNames = []
  }
  
-- Create a lexer that can be used to build the parser combinators
lexer = Token.makeTokenParser languageDef

-- Programs are a series of cons cells separated by newlines
prog :: Parser Prog
prog = do
  val <- sepBy1 cons newline
  return $ Prog val

-- A cons value is either a cons cell or a literal value
cons :: Parser Cons
cons = cell <|> val 

-- A cons cell is a set of cell contents surrounded by parentheses
cell :: Parser Cons
cell = Token.parens lexer cellContents

-- Cell contents are two cons values separated by whitespace
cellContents :: Parser Cons
cellContents = do
  l <- cons
  (Token.whiteSpace lexer)
  r <- cons
  return $ Cons l r

-- A literal value is either a string or nil
val :: Parser Cons
val = do 
  value <- many1 letter
  return $ case value of "nil" -> Nil
                         _     -> Lit value