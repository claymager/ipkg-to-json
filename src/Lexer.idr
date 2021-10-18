module Lexer

import Text.Lexer

public export
data PkgToken
  = Comment String
  | Equals
  | Separator
  | Dot
  | Operator String
  | Space
  | StringLit String
  | Identifier String
  | NatLit Nat

public export
Show PkgToken where
  show (Comment x) = "Comment (\{x})"
  show Equals = "EQ"
  show Separator = ","
  show Dot = "."
  show (Operator x) = "Op(\{x})"
  show Space = "<space>"
  show (StringLit x) = "StrLit(\{x})"
  show (Identifier x) = "Id(\{x})"
  show (NatLit k) = "Nat(\{show k})"

operators : List String
operators = [">=", "<=", "==", "&&", ">", "<"]

oneOf' : List String -> Lexer
oneOf' = foldr (\x, y => exact x <|> y) fail

comment : Lexer
comment = lineComment $ exact "--"

ident : Lexer
ident = (alpha <|> is '_') <+> many (alpha <|> is '-' <|> is '_' <|> digit <|> is '.')

export
tokenMap : TokenMap PkgToken
tokenMap =
  [ (is '.', const Dot)
  , (is ',', const Separator)
  , (is '=', const Equals)
  , (some (space <|> newline), const Space)
  , (oneOf' operators, Operator)
  , (comment, Comment)
  , (stringLit, StringLit)
  , (digits, NatLit . cast)
  , (ident, Identifier)
  ]

