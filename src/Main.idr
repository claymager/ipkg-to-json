module Main

import System
import Data.List
import Language.JSON

import Core.Core
import Idris.CommandLine
import Idris.Package
import Idris.Package.Types
import Idris.REPL.Common

-- don't want these
import Compiler.Common
import Core.Context
import Idris.Syntax
import Libraries.Utils.Term

pkgToJSON : PkgDesc -> JSON
pkgToJSON desc = JObject $ [
    ("name", JString $ name desc),
    -- ("modules", JArray (map
    -- ("modules", : list (ModuleIdent, String) ||| [(namespace, filename)]
    -- ("mainmod", : maybe (moduleIdent, string) file loaded at repl
    ("depends", JArray (JString <$> pkgname <$> depends desc))
  ] ++ maybeStrings [
    ("version", map show . version),
    ("authors", authors),
    ("maintainers", maintainers),
    ("license", license),
    ("brief", brief),
    ("readme", readme),
    ("homepage", homepage),
    ("sourceloc", sourceloc),
    ("bugtracker", bugtracker),
    ("executable", executable),
    ("sourcedir", sourcedir),
    ("builddir", builddir),
    ("outputdir", outputdir),
    -- Ignore the FC's
    ("options", (map snd) . options),
    ("prebuild", (map snd) . prebuild),
    ("postbuild", (map snd) . postbuild),
    ("preinstall", (map snd) . preinstall),
    ("postinstall", (map snd) . postinstall),
    ("preclean", (map snd) . preclean),
    ("postclean", (map snd) . postclean)
  ]
  where
    project : (String, PkgDesc -> Maybe String) -> Maybe (String, JSON)
    project (nm, projection) = map (\val => (nm, JString val)) $ projection desc
    maybeStrings : List (String, PkgDesc -> Maybe String) -> List (String, JSON)
    maybeStrings xs = catMaybes $ map project xs

stMain : String -> Core PkgDesc
stMain fn = do
  c <- newRef Ctxt !initDefs
  s <- newRef Syn initSyntax
  o <- newRef ROpts (REPL.Opts.defaultOpts Nothing (REPL InfoLvl) [])
  parsePkgFile fn

main : IO ()
main = do
  (_ :: fn :: []) <- getArgs
    | _ => do putStrLn "Invalid command line"
              exitWith (ExitFailure 1)
  setupTerm
  coreRun (stMain fn)
    (\err : Error => do putStrLn ("uncaught error: " ++ show err)
                        exitWith (ExitFailure 1))
    (\res => putStrLn . show . pkgToJSON $ res)
