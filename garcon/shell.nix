{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, blaze-html, bytestring, bytestring-trie
      , comonad, filepath, lens, mtl, process, servant, servant-blaze
      , servant-client, servant-server, stdenv, stm, text, time, trifecta
      , unix, wai, wai-websockets, warp, websockets, zippers
      }:
      mkDerivation {
        pname = "gobble";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base blaze-html bytestring bytestring-trie comonad filepath lens
          mtl process servant servant-blaze servant-client servant-server stm
          text time trifecta unix wai wai-websockets warp websockets zippers
        ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
