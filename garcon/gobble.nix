{ mkDerivation, aeson, base, blaze-html, bytestring, containers
, directory, filepath, lens, lens-aeson, mtl, process, servant
, servant-blaze, servant-client, servant-server, stdenv, stm, text
, time, unix, wai, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "gobble";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html bytestring containers directory filepath lens
    lens-aeson mtl process servant servant-blaze servant-client
    servant-server stm text time unix wai wai-websockets warp
    websockets
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}