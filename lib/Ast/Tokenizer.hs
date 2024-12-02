module Ast.Tokenizer () where

data Separator = OpenParen | CloseParen

data Literal = LNumber Int | LBoolean Bool | LSymbol String

data Token = TLiteral Literal | TSeparator Separator

tokenize :: String -> Either String [Token]
tokenize _ = Left "error"
