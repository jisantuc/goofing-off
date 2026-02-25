{
  inputs = {
    utils.url = "github:numtide/flake-utils";
    dataframe.url = "github:datahaskell/dataframe";
    nixpkgs.follows = "dataframe/nixpkgs";
  };

  outputs = { nixpkgs, utils, dataframe, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc910";
          haskellPackages = pkgs.haskell.packages.${compiler}.extend(self: super: {
            dataframe = dataframe.packages.${system}.default;
          });
          devDependencies = with haskellPackages; [
            cabal-fmt
            cabal-gild
            cabal-install
            haskell-language-server
            hls-hlint-plugin
            hlint
            ormolu
          ];
          packages = ps: [ (ps.callCabal2nix "goofing-off" ./. { }) ];
        in
        {
          devShells.default = haskellPackages.shellFor {
            inherit packages;
            nativeBuildInputs = devDependencies;
            withHoogle = true;
          };

          devShells.ci = haskellPackages.shellFor {
            inherit packages;
            nativeBuildInputs = with haskellPackages; [ cabal-install ];
          };

          packages = {
            mosconi-sim = haskellPackages.callCabal2nix "mosconi-sim" ./. { };
          };
        }
      );
}

