{
  description = "Flake for my-clippings-csv";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs.haskellPackages; [
            cabal-install
            pkgs.ormolu
            (ghcWithPackages (ps: with ps; [
              megaparsec
              cassava
              mtl
              optparse-applicative
              hspec
              hspec-megaparsec
              haskell-language-server
            ]))
          ];
        };
      });

      packages = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.haskellPackages.developPackage {
          name = "my-clippings-csv";
          root = ./.;
        };
      });
    };
}
