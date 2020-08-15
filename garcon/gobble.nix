{ mkDerivation, aeson, base, blaze-html, bytestring, colour
, containers, diagrams, diagrams-lib, diagrams-svg, directory
, filepath, lens, lens-aeson, mtl, process, random, random-shuffle, servant
, servant-blaze, servant-client, servant-server, stdenv, stm, text, time, unix
, wai, wai-websockets, warp, websockets, hashable, utf8-string, data-default
, hspec, unordered-containers
}:
mkDerivation {
  pname = "gobble";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html bytestring colour containers diagrams
    diagrams-lib diagrams-svg directory filepath lens lens-aeson mtl
    process random random-shuffle servant servant-blaze servant-client
    servant-server stm text time unix wai wai-websockets warp
    websockets hashable utf8-string data-default hspec unordered-containers
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
