module Main

import System
import System.File
import Language.JSON

import Ipkg.Types
import Ipkg.Parser

dependsObj : Depends -> JSON
dependsObj (D name (PVB lb li ub ui)) = JObject $ [("name", JString name)] ++
  (case lb of Nothing => []
              Just v => [ ("lowerBound", JString $ show v)
                        , ("lowerInclusive", JBoolean li) ]) ++
  (case ub of Nothing => []
              Just v => [ ("upperBound", JString $ show v)
                        , ("upperInclusive", JBoolean ui) ])

pkgToJSON : Package -> JSON
pkgToJSON (P name version misc depends modules) = JObject $
  [ ("name", JString name)
  , ("depends", JArray (map dependsObj depends))
  , ("version", maybe JNull (JString . show) version)
  , ("modules", JArray (map JString modules))
  ] ++ rest
  where rest : List (String, JSON)
        rest = mapSnd JString <$> misc

main : IO ()
main = do
  (_ :: fn :: []) <- getArgs
    | _ => putStrLn "Bad command line"
  Right s <- readFile fn
    | Left err => putStrLn $ show err
  let Right pkg = parsePkg s
     | Left err => putStrLn err
  printLn $ pkgToJSON pkg
