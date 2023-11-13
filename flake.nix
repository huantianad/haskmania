{
  description = "Haskell!";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      ghc = pkgs.haskell.compiler.ghc94;

      hsl = pkgs.haskell-language-server.override {
        supportedGhcVersions = [ "94" ];
      };
    in
    {
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          ghc
          hsl
          pkgs.cabal-install
          pkgs.stack

          pkgs.pkg-config
          pkgs.pulseaudio
          pkgs.SDL2
        ];
      };
    };
}
