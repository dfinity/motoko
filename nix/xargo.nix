# xargo is used to build motoko-rts for wasm32. We need to make a shared Wasm
# library for the RTS (that's what moc-ld supports) but Rust ships wasm32
# libraries (core and std) without PIC relocation model, so we use xargo to make
# PIC versions of core and std.

{ rustPlatform-nightly, fetchFromGitHub, lib, python, cmake, llvmPackages, clang, stdenv, darwin, zlib }:

rustPlatform-nightly.buildRustPackage rec {
  name = "xargo";

  src = fetchFromGitHub {
    owner = "japaric";
    repo = "${name}";
    rev = "16035a7c401262824edcb87e1401fe4b05a5ccc0";
    sha256 = "0m1dg7vwmmlpqp20p219gsm7zbnnii6lik6hc2vvfsdmnygf271l";
    fetchSubmodules = true;
  };

  cargoSha256 = "0zzksgi2prgw01m6r4bqjjz902h5g5ich0h3xvb60w4sshlss891";

  # nativeBuildInputs = [ python cmake clang ];
  # buildInputs = [ llvmPackages.libclang ] ++
  #  lib.optionals stdenv.isDarwin [ darwin.apple_sdk.frameworks.Security ];
  # LIBCLANG_PATH = "${llvmPackages.libclang}/lib";

  doCheck = false;
  # error: couldn't lock thumbv6m-panic_abort-eabi's sysroot as read-only
  USER = "nobody"; # for xargo tests (if we would run them)

  meta = with lib; {
    description = "The sysroot manager that lets you build and customize std";
    homepage = "https://github.com/japaric/xargo";
    license = licenses.mit;
    maintainers = [ {
      email = "omer.agacan@dfinity.org";
      github = "osa1";
      githubId = 123123;
      name = "Ömer Sinan Ağacan";
    } ];
    platforms = platforms.unix;
  };
}
