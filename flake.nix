{
  inputs = {
    utils.url = "github:numtide/flake-utils";
    dataframe.url = "github:DataHaskell/dataframe";
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
            random = pkgs.haskell.packages.${compiler}.callHackage "random" "1.3.1" {};
            time-compat = pkgs.haskell.lib.dontCheck super.time-compat;
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
          packages = ps: [ (ps.callCabal2nix "mosconi-sim" ./. { }) ];
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

