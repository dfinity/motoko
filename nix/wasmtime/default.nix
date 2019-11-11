{ rustPlatform, fetchFromGitHub, lib, python, cmake, llvmPackages, clang, stdenv, darwin }:

rustPlatform.buildRustPackage {
  pname = "wasmtime";
  version = "20191111";

  src = fetchFromGitHub {
    owner = "kubkon";
    repo = "wasmtime";
    rev = "d2d623ef2b528f80e477c0ece5187c0fed17b712";
    sha256 = "1mm9276xyw95px105v0i3b1xq852jgmsa742bynlxgw4p1cxm423";
    fetchSubmodules = true;
  };

  cargoSha256 = "0mnwaipa2az3vpgbz4m9siz6bfyhmzwz174k678cv158m7mxx12f";

  cargoPatches = [ ./cargo-lock.patch ];

  nativeBuildInputs = [ python cmake clang ];
  buildInputs = [ llvmPackages.libclang ] ++
   lib.optionals stdenv.isDarwin [ darwin.apple_sdk.frameworks.Security ];
  LIBCLANG_PATH = "${llvmPackages.libclang}/lib";

  meta = with lib; {
    description = "Standalone JIT-style runtime for WebAsssembly, using Cranelift";
    homepage = https://github.com/CraneStation/wasmtime;
    license = licenses.asl20;
    maintainers = [ maintainers.matthewbauer ];
    platforms = platforms.unix;
  };
}
