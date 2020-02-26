module ConcreteSyntaxParser where

import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import ConcreteSyntax

-- Create language definition with some presets
languageDef = emptyDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedNames   = []
  , Token.reservedOpNames = []
  }
  
-- Create a lexer that can be used to build the parser combinators using the language definition
lexer = Token.makeTokenParser languageDef

-- Parses an input string into a concrete syntax tree
parseConcrete :: String -> Either ParseError Concrete
parseConcrete inputString = parse' concrete "glyph" inputString

-- Wrapper function that runs the given parser and returns any errors OR a valid raw AST
parse' :: Parser a -> String -> String -> Either ParseError a
parse' parser name inputString = parse parser name inputString 

-- A program is a collection of cells
concrete:: Parser Concrete
concrete = do
  val <- sepBy1 element $ Token.whiteSpace lexer
  return $ Concrete val

element :: Parser Cell
element = atom <|> cell

-- A cell is either a collection of cells or an atom
cell :: Parser Cell
cell = do
  val <- Token.parens lexer $ sepBy1 element $ Token.whiteSpace lexer
  return $ Cell val

atom :: Parser Cell
atom = do
  val <- many1 $ noneOf "( )"
  return $ Atom val