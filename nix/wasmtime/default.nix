{ rustPlatform, fetchFromGitHub, lib, python, cmake, llvmPackages, clang }:

rustPlatform.buildRustPackage {
  pname = "wasmtime";
  version = "20190927";

  src = fetchFromGitHub {
    owner = "CraneStation";
    repo = "wasmtime";
    rev = "406bc7895a4ec274bc21e03d1ec2b90a085a5687";
    sha256 = "05nq004sp3wz5xp98rrc094f7k77nkjdmwbi7rmncvcj9hsczvw3";
    fetchSubmodules = true;
  };

  cargoSha256 = "1ynnr5cpz6snb65a5hs8jwq2b800x5c6l0kijg5qqb29gh0x8yyg";

  cargoPatches = [ ./cargo-lock.patch ];

  nativeBuildInputs = [ python cmake clang ];
  buildInputs = [ llvmPackages.libclang ];

  LIBCLANG_PATH = "${llvmPackages.libclang}/lib";

  meta = with lib; {
    description = "Standalone JIT-style runtime for WebAsssembly, using Cranelift";
    homepage = https://github.com/CraneStation/wasmtime;
    license = licenses.asl20;
    maintainers = [ maintainers.matthewbauer ];
    platforms = platforms.unix;
  };
}
