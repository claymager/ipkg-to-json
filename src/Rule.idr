module Rule

import Types
import Lexer

import Text.Parser

public export
Rule : Type -> Type
Rule a = Grammar () PkgToken True a

public export
EmptyRule : Type -> Type
EmptyRule a = Grammar () PkgToken False a


export
comment : Rule String
comment = terminal "Expected comment" $
  \case Comment i => Just i
        _ => Nothing

export
exact : String -> Rule String
exact s = terminal "Expected exactly \{s}" $
  \case Identifier s' => if s == s' then Just s else Nothing
        _ => Nothing

export
identifier : Rule String
identifier = terminal "Expected an Identifier" $
  \case Identifier s => Just s
        _ => Nothing

export
op' : Rule String
op' = terminal "Expected an Operator" $
  \case Operator s => Just s
        _ => Nothing

export
op : String -> Rule ()
op s = terminal "Expected operator \{s}" $
  \case Operator s' => if s == s' then Just () else Nothing
        _ => Nothing


sep' : Rule ()
sep' = terminal "Expected ','" $
  \case Separator => Just ()
        _ => Nothing

export
sep : Rule t -> Rule (List t)
sep r = forget <$> sepBy1 sep' r

export
space : Rule ()
space = terminal "Expected ','" $
  \case Space => Just ()
        _ => Nothing

export
dot : Rule ()
dot = terminal "Expected '.'" $
  \case Dot => Just ()
        _ => Nothing

export
equals : Rule ()
equals = terminal "Expected '='" $
  \case Equals => Just ()
        _ => Nothing

export
stringLit : Rule String
stringLit = terminal "Expected string" $
  \case StringLit s => Just s
        _ => Nothing

export
nat : Rule Nat
nat = terminal "Expected integer" $
  \case NatLit i => Just i
        _ => Nothing
