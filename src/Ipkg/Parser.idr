module Ipkg.Parser

import Data.List
import Text.Lexer
import Text.Parser
import Text.Parser.Core

import Ipkg.Types
import Ipkg.Lexer
import Ipkg.Rule

data DescField : Type where
  PVersion : PkgVersion -> DescField
  PStrField : String -> String -> DescField
  PModules : List String -> DescField
  PDepends : List Depends -> DescField

pkgversion : Rule PkgVersion
pkgversion = PV <$> sepBy1 dot nat

packageName : Rule String
packageName = exact "package" *> identifier

version : Rule PkgVersion
version = property "version" >> pkgversion

modules : Rule (List String)
modules = property "modules" >> sep identifier

deps : Rule (List Depends)
deps = property "depends" >> sep depends
  where
    data Bound = LT PkgVersion Bool | GT PkgVersion Bool
    bound : Rule (List Bound)
    bound = do op "<="
               v <- pkgversion
               pure [ LT v True ]
        <|> do op ">="
               v <- pkgversion
               pure [ GT v True ]
        <|> do op "<"
               v <- pkgversion
               pure [ LT v False ]
        <|> do op ">"
               v <- pkgversion
               pure [ GT v False ]
        <|> do op "=="
               v <- pkgversion
               pure [LT v True, GT v True]

    mkBound : List Bound -> PkgVersionBounds -> EmptyRule PkgVersionBounds
    mkBound (LT b i :: bs) pkgbs
      = maybe (mkBound bs . { upperBound := Just b, upperInclusive := i } $ pkgbs)
              (\_ => fail "Dependency already has an upper bound")
              pkgbs.upperBound
    mkBound (GT b i :: bs) pkgbs
      = maybe (mkBound bs . { lowerBound := Just b, lowerInclusive := i } $ pkgbs)
              (\_ => fail "Dependency already has a lower bound")
              pkgbs.lowerBound
    mkBound [] pkgbs = pure pkgbs

    depends : Rule Depends
    depends = do
      name <- identifier
      bs <- sepBy (op "&&") bound
      pure $ D name !(mkBound (concat bs) anyBounds)

strField : Rule (String, String)
strField = do
  name <- identifier
  equals
  value <- stringLit <|> identifier
  pure (name, value)

field : Rule DescField
field = (PVersion <$> version)
    <|> (PDepends <$> deps)
    <|> (PModules <$> modules)
    <|> (uncurry PStrField <$> strField)

addField : DescField -> Package -> Package
addField (PVersion v) = { version := Just v }
addField (PStrField nm val) = { strFields $= ((nm, val) ::) }
addField (PDepends ds) = { depends := ds }
addField (PModules ms) = { modules := ms }


expr : Rule Package
expr = do
  pkg <- def <$> packageName
  fields <- some field
  eof
  pure $ foldr addField pkg fields

processWhitespace : (List (WithBounds PkgToken), a) ->
                    (List (WithBounds PkgToken), a)
processWhitespace = mapFst (filter notComment)
  where
    notComment : WithBounds PkgToken -> Bool
    notComment t with (t.val)
      notComment _ | (Comment x) = False
      notComment _ | Space = False
      notComment _ | _ = True


runParser : String -> Either (List1 (ParsingError PkgToken)) (Package, List (WithBounds PkgToken))
runParser s = parse expr . fst . processWhitespace $ lex tokenMap s

errMsgs : List1 (ParsingError PkgToken) -> String
errMsgs = concatMap decorate . forget
  where decorate : ParsingError PkgToken -> String
        decorate (Error msg bounds) = "ParseError(\{msg}; \{show bounds})"

export
parsePkg : String -> Either String Package
parsePkg = bimap errMsgs fst . runParser
