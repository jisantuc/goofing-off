{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.11";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc910";
          haskellPackages = pkgs.haskell.packages.${compiler};
          devDependencies = with haskellPackages; [
            cabal-fmt
            cabal-gild
            cabal-install
            haskell-language-server
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

