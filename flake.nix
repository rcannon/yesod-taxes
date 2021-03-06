
{
  description = "yesod-taxes";
  inputs = 
  {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc8107;
        packageName = "yesod-taxes";
      in 
      {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self {};
        defaultPackage = self.packages.${system}.${packageName};
        devShell = pkgs.mkShell 
        {
          buildInputs = with haskellPackages; 
          [
            haskell-language-server
            ghcid
            cabal-install
            pkgs.postgresql
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
