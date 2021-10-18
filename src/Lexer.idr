module Lexer

import Data.String.Extra
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
  show Separator = "SEP"
  show Dot = "DOT"
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
  [ (oneOf' operators, Operator)
  , (some (space <|> newline), const Space)
  , (comment, Comment)
  , (stringLit, StringLit . shrink 1)
  , (digits, NatLit . cast)
  , (ident, Identifier)
  , (is '.', const Dot)
  , (is ',', const Separator)
  , (is '=', const Equals)
  ]

