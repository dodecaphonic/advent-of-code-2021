{ mkDerivation, base, containers, lens, lib, megaparsec, vector }:
mkDerivation {
  pname = "AdventOfCode2021";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers lens megaparsec vector ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
