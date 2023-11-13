{
  description = "Haskell!";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      haskellPackages = pkgs.haskellPackages;

      hsl = pkgs.haskell-language-server.override {
        supportedGhcVersions = [ "94" ];
      };
    in
    {
      devShell.${system} = haskellPackages.developPackage {
        root = ./.;
        returnShellEnv = true;
        modifier = drv: pkgs.haskell.lib.addBuildTools drv (
          [
            hsl
            haskellPackages.cabal-install
            haskellPackages.ghcid
          ]
        );
      };
    };
}
