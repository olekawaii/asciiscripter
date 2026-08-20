{
  description = "A language for making ascii art animations";

  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell rec {
        inputsFrom = with self.packages.${system}; [
          thorn
          thorn-converters
        ];
        packages = with self.packages.${system}; [
          pkgs.ffmpeg
          thorn
          thorn-converters
        ];
      };

      packages.${system} = rec {
        default = pkgs.symlinkJoin {
          name = "thorn-complete";
          paths = [
            thorn
            thorn-converters
            fonts
          ];
        };

        thorn = pkgs.rustPlatform.buildRustPackage rec {
          pname = "thorn";
          version = "1.0";
          src = ./thorn;
          cargoLock.lockFile = ./thorn/Cargo.lock;
          meta = with pkgs.lib; {
            description = "Interpreter of the thorn language";
            homepage = "https://codeberg.org/olekawaii/thorn";
            license = licenses.gpl3;
            maintainers = [ ];
          };
        };

        thorn-converters = pkgs.stdenv.mkDerivation {
          name = "thorn-converters";
          src = ./converters;
          nativeBuildInputs = with pkgs; [
            ghc
            makeWrapper
          ];
          buildPhase = ''
            ghc th2sh.hs
            ghc th2ppm.hs
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp th2sh th2ppm th2gif $out/bin
            wrapProgram $out/bin/th2gif \
              --add-flags "--font-dir ${fonts}/share/fonts"
          '';
        };

        fonts = pkgs.stdenv.mkDerivation {
          name = "thorn-fonts";
          src = ./fonts;
          installPhase = ''
            mkdir -p $out/share/fonts
            cp * $out/share/fonts
          '';
        };
      };
    };
}
