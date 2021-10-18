{
  description = "My Idris 2 package";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.idris2-pkgs.url = "github:claymager/idris2-pkgs";

  outputs = { self, nixpkgs, idris2-pkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" "x86_64-linux" "i686-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ idris2-pkgs.overlay ]; };
        ipkg-to-json = pkgs.idris2.buildIdris {
          src = ./.;
          name = "ipkg-to-json";
          idrisLibraries = [ pkgs.idris2.packages.idris2api ];
          version = "0.1";
        };
      in
      {
        defaultPackage = ipkg-to-json;
        devShell = pkgs.mkShell {
          buildInputs = [ pkgs.rlwrap pkgs.idris2.packages.lsp.withPkgs.idris2api pkgs.idris2.withPkgs.idris2api ];
          shellHook = ''
            exec fish
            '';
        };
      }
    );
}
