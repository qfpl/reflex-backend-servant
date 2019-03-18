{ mkDerivation, aeson, base, bytestring, containers, dependent-sum
, hashable, http-api-data, mtl, network, primitive, ref-tf, reflex
, reflex-basic-host, servant, servant-server
, stdenv, stm, tagged, transformers, ttrie, wai, warp
}:
mkDerivation {
  pname = "reflex-server-servant";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers dependent-sum hashable
    http-api-data mtl network primitive ref-tf reflex reflex-basic-host
    servant servant-server stm tagged ttrie wai
  ];
  executableHaskellDepends = [
    base containers dependent-sum hashable http-api-data mtl ref-tf
    reflex reflex-basic-host servant servant-server stm
    transformers wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
