{
  description = "Haskell!";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      ghcVersion = "94";

      ghc = pkgs.haskell.compiler."ghc${ghcVersion}";
      hsl = pkgs.haskell-language-server.override {
        supportedGhcVersions = [ ghcVersion ];
      };
    in
    {
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          ghc
          hsl
          pkgs.cabal-install

          pkgs.zlib
          pkgs.libz
          pkgs.pkg-config
          pkgs.libpulseaudio
          pkgs.SDL2
        ];
      };
    };
}
