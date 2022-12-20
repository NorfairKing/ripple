{ mkDerivation, aeson, autodocodec, autodocodec-openapi3, base
, bytestring, deepseq, genvalidity, genvalidity-bytestring
, genvalidity-persistent, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-sydtest-persistent
, genvalidity-text, genvalidity-time, genvalidity-typed-uuid
, hashable, http-api-data, http-client, http-media
, insert-ordered-containers, lens, lib, monad-logger, mtl, openapi3
, path-pieces, persistent, persistent-sqlite, QuickCheck, servant
, servant-client, servant-multipart, servant-multipart-api
, servant-multipart-client, servant-openapi3, servant-server
, sydtest, sydtest-aeson, sydtest-discover
, sydtest-persistent-sqlite, sydtest-servant, sydtest-wai
, template-haskell, text, time, typed-uuid, uuid, wai, warp
}:
mkDerivation {
  pname = "ripple-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-openapi3 base bytestring deepseq
    genvalidity genvalidity-bytestring genvalidity-persistent
    genvalidity-text genvalidity-time genvalidity-typed-uuid hashable
    http-api-data http-media monad-logger mtl openapi3 path-pieces
    persistent persistent-sqlite QuickCheck servant servant-client
    servant-multipart servant-multipart-api servant-multipart-client
    servant-server template-haskell text time typed-uuid uuid wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-sydtest-persistent http-client http-media
    insert-ordered-containers lens openapi3 path-pieces servant
    servant-client servant-multipart servant-openapi3 sydtest
    sydtest-aeson sydtest-persistent-sqlite sydtest-servant sydtest-wai
    text
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "ripple-server";
}
