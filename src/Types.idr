module Types

import Data.List1

public export
data PkgVersion = PV (List1 Nat)


public export
record PkgVersionBounds where
  constructor PVB
  lowerBound : Maybe PkgVersion
  lowerInclusive : Bool
  upperBound : Maybe PkgVersion
  upperInclusive : Bool

export
anyBounds : PkgVersionBounds
anyBounds = PVB Nothing True Nothing True

public export
record Depends where
  constructor D
  name : String
  bounds : PkgVersionBounds
public export
record Package where
  constructor P
  name : String
  version : Maybe PkgVersion
  strFields : List (String, String)
  depends : List Depends

export
def : String -> Package
def s = P s Nothing [] []

public export
Show Depends where
  show (D n b) = n

public export
Show PkgVersion where
  show (PV v) = show v

public export
Show Package where
  show (P n v s d) =
    "package \{n}\n" ++
    (maybe "" (\x => "version \{show x}\n") v) ++
    "strings:\n" ++ show s ++ "\n" ++
    "depends: " ++ show d

