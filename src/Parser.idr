module Parser

import System.File
import Data.List
import Text.Lexer
import Text.Parser
import Text.Parser.Core

import Types
import Lexer
import Rule


data DescField : Type where
  PVersion : PkgVersion -> DescField
  PStrField : String -> String -> DescField
  PDepends : List Depends -> DescField


packageName : Rule String
packageName = exact "package" *> identifier

strField : Rule (String, String)
strField = do
  name <- identifier
  equals
  value <- stringLit <|> identifier
  pure (name, value)

pkgversion : Rule PkgVersion
pkgversion = PV <$> sepBy1 dot nat

version : Rule PkgVersion
version = do
  s <- exact "version"
  equals
  pkgversion

deps : Rule (List Depends)
deps = do
  ignore $ exact "depends"
  equals
  sep depends
  where
    data Bound = LT PkgVersion Bool | GT PkgVersion Bool
    bound : Rule (List Bound)
    bound = (op "<=" >> pure [ LT !pkgversion True ])
        <|> (op ">=" >> pure [ GT !pkgversion True ])
        <|> (op "<" >> pure [ LT !pkgversion False ])
        <|> (op ">" >> pure [ GT !pkgversion False ])
        <|> do op "=="
               v <- pkgversion
               pure [LT v True, GT v True]

    mkBound : List Bound -> PkgVersionBounds -> EmptyRule PkgVersionBounds
    mkBound (LT b i :: bs) pkgbs
      = maybe (mkBound bs . { upperBound := Just b, upperInclusive := i } $ pkgbs)
              (\_ => fail "Dependency has an upper bound")
              pkgbs.upperBound
    mkBound (GT b i :: bs) pkgbs
      = maybe (mkBound bs . { lowerBound := Just b, lowerInclusive := i } $ pkgbs)
              (\_ => fail "Dependency has a lower bound")
              pkgbs.upperBound
    mkBound [] pkgbs = pure pkgbs

    depends : Rule Depends
    depends = do
      name <- identifier
      bs <- sepBy (op "&&") bound
      pure $ D name !(mkBound (concat bs) anyBounds)
  
field : Rule DescField
field = (version >>= pure . PVersion)
    <|> (deps >>= pure . PDepends)
    <|> (strField >>= pure . uncurry PStrField)

addField : DescField -> Package -> Package
addField (PVersion v) = { version := Just v }
addField (PStrField nm val) = { strFields $= ((nm, val) ::) }
addField (PDepends ds) = { depends := ds }


expr : Rule Package
expr = do
  pkg <- def <$> packageName
  fields <- some field
  -- eof
  pure $ foldr addField pkg fields
       

processWhitespace : (List (WithBounds PkgToken), Int, Int, String) ->
                    (List (WithBounds PkgToken), Int, Int, String)
processWhitespace = mapFst (filter notComment)
  where
    notComment : WithBounds PkgToken -> Bool
    notComment t with (t.val)
      notComment _ | (Comment x) = False
      notComment _ | Space = False
      notComment _ | _ = True


test : String -> Either (List1 (ParsingError PkgToken)) (Package, List (WithBounds PkgToken))
test s = parse expr . fst . processWhitespace $ lex tokenMap s

Show (ParsingError PkgToken) where
  show (Error s e) = "ParseError: \{s}\n\{show e}"

[LWPT] Show (List (WithBounds PkgToken)) where
  show ((MkBounded val isIrrelevant bounds) :: t) = "\nval: \{show val}\nbounds: \{show bounds}\n" ++ show t
  show [] = ""

testStr : IO String
testStr = either (const "") id <$> readFile "ipkg-to-json.ipkg"

lexemes : IO (List (WithBounds PkgToken))
lexemes = testStr >>= pure . fst . processWhitespace . lex tokenMap

parsemes : IO (Either (List1 (ParsingError PkgToken)) (Package, List (WithBounds PkgToken)))
parsemes = parse expr <$> lexemes

showLexemes : IO ()
showLexemes = traverse_ printLn . fst . processWhitespace $ lex tokenMap !testStr

main : IO ()
main = do
  Right s <- readFile "ipkg-to-json.ipkg"
    | Left err => putStrLn $ show err
  putStrLn . either (show . head) show $ test s
