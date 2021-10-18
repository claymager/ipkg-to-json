# ipkg-to-json

Converts idris2 `ipkg` files to a more generally machine-readable json format, spitting the output to `stdout`.

Heavily inspired by [idris2api.Idris.Package](https://github.com/idris-lang/Idris2/tree/main/src/Idris/Package.idr),
but doesn't depend on any of the idris2api source, and has fewer guarantees.

## Usage

```bash
$ ipkg-to-json ipkg-to-json.ipkg | jq

{
  "name": "ipkgToJson",
  "depends": [
    {
      "name": "contrib"
    }
  ],
  "version": "0.1",
  "modules": [
    "Ipkg.Parser",
    "Ipkg.Lexer",
    "Ipkg.Types",
    "Ipkg.Rule"
  ],
  "sourcedir": "src",
  "main": "Main",
  "executable": "ipkg-to-json"
}
```
