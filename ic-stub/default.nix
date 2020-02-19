# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# To regenerate this file execute the following command in this directory:
#
# cp $(nix-build ./generate.nix --no-link)/default.nix ./default.nix
{ mkDerivation, base, binary, bytestring, containers
, data-default-class, filepath, hex-text, mtl, optparse-applicative
, primitive, stdenv, text, transformers, utf8-string, vector
, winter
}:
mkDerivation {
  pname = "ic-stub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring containers data-default-class filepath
    hex-text mtl optparse-applicative primitive text transformers
    utf8-string vector winter
  ];
  doCheck = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
