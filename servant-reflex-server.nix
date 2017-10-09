{ mkDerivation, aeson, base, bytestring, containers, dependent-sum
, hashable, http-api-data, mtl, network, primitive, ref-tf, reflex
, reflex-basic-host, reflex-dom-core, servant, servant-server
, stdenv, stm, ttrie, warp
}:
mkDerivation {
  pname = "servant-reflex-server";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers dependent-sum hashable
    http-api-data mtl network primitive ref-tf reflex reflex-basic-host
    reflex-dom-core servant servant-server stm ttrie warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
