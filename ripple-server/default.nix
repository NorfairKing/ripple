{ mkDerivation, aeson, autodocodec, base, bytestring, deepseq
, genvalidity, genvalidity-sydtest, genvalidity-sydtest-aeson
, genvalidity-sydtest-persistent, hashable, lib, monad-logger, mtl
, path-pieces, persistent, persistent-sqlite, persistent-template
, QuickCheck, servant, servant-client, servant-multipart
, servant-multipart-client, servant-server, sydtest
, sydtest-discover, text, validity, wai, wai-extra, warp
}:
mkDerivation {
  pname = "ripple-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring deepseq genvalidity hashable
    monad-logger mtl path-pieces persistent persistent-sqlite
    persistent-template QuickCheck servant servant-client
    servant-multipart servant-multipart-client servant-server text
    validity wai wai-extra warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-sydtest-persistent path-pieces sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "ripple-server";
}
