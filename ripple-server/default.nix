{ mkDerivation, aeson, autodocodec, autodocodec-openapi3, base
, bytestring, deepseq, genvalidity, genvalidity-bytestring
, genvalidity-sydtest, genvalidity-sydtest-aeson
, genvalidity-sydtest-persistent, genvalidity-text, hashable
, http-client, http-media, http-types, insert-ordered-containers
, lens, lib, monad-logger, mtl, openapi3, path-pieces, persistent
, persistent-sqlite, persistent-template, QuickCheck, servant
, servant-client, servant-multipart, servant-multipart-api
, servant-multipart-client, servant-openapi3, servant-server
, sydtest, sydtest-aeson, sydtest-discover, sydtest-persistent
, sydtest-persistent-sqlite, sydtest-servant, sydtest-wai, text
, validity, wai, wai-extra, warp
}:
mkDerivation {
  pname = "ripple-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-openapi3 base bytestring deepseq
    genvalidity genvalidity-bytestring genvalidity-text hashable
    monad-logger mtl openapi3 path-pieces persistent persistent-sqlite
    persistent-template QuickCheck servant servant-client
    servant-multipart servant-multipart-api servant-multipart-client
    servant-server text validity wai wai-extra warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec-openapi3 base bytestring genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-sydtest-persistent
    http-client http-media http-types insert-ordered-containers lens
    openapi3 path-pieces servant servant-client servant-multipart
    servant-openapi3 servant-server sydtest sydtest-aeson
    sydtest-persistent sydtest-persistent-sqlite sydtest-servant
    sydtest-wai text
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "ripple-server";
}
