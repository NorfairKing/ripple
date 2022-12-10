{ mkDerivation, aeson, autodocodec, base, genvalidity, lib
, monad-logger, mtl, persistent, persistent-sqlite
, persistent-template, servant, servant-client, servant-server
, sydtest, sydtest-discover, wai, wai-extra, warp
}:
mkDerivation {
  pname = "ripple-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base genvalidity monad-logger mtl persistent
    persistent-sqlite persistent-template servant servant-client
    servant-server wai wai-extra warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "ripple-server";
}
