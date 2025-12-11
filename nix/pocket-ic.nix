pkgs: let
  # Map Nix system to binary name (note: Nix uses aarch64, binaries use arm64)
  binaryName = {
    "x86_64-linux" = "pocket-ic-x86_64-linux";
    "aarch64-linux" = "pocket-ic-arm64-linux";
    "x86_64-darwin" = "pocket-ic-x86_64-darwin";
    "aarch64-darwin" = "pocket-ic-arm64-darwin";
  }.${pkgs.system} or (throw "Unsupported system: ${pkgs.system}");

  # SHA256 hashes for each platform's .gz file
  sha256Map = {
    "pocket-ic-x86_64-linux" = "42ffe67ff1688fbc8111ca63d9527f2ad5c02d3462eeef803bb99f89acda7d43";
    "pocket-ic-arm64-linux" = "83991b18925d92471c30f10a00363195b9cd9e5e45bf1dfad522625f8e71d942";
    "pocket-ic-x86_64-darwin" = "b89923b6a216e4f609ea3ebdab69e87dd782e9a722cd5daced5053b8eff866b3";
    "pocket-ic-arm64-darwin" = "cda584415351cbbefbcd59321820c3e1252b3e2f0508d761c667551b5849cea9";
  };

  releaseTag = "release-2025-10-02_03-13-base";
  baseUrl = "https://github.com/dfinity/ic/releases/download/${releaseTag}";

  server = pkgs.stdenv.mkDerivation rec {
    pname = "pocket-ic-server";
    version = "0.1.0"; # Use a proper version
    
    src = pkgs.fetchurl {
      url = "${baseUrl}/${binaryName}.gz";
      sha256 = sha256Map.${binaryName};
      name = "pocket-ic-server.gz"; 
    };
    
    dontUnpack = true; 
    
    nativeBuildInputs = [ pkgs.gzip ];
    
    installPhase = ''
      mkdir -p $out/bin
      
      # Decompress the file into the final binary path
      gunzip -c $src > $out/bin/pocket-ic-server
      
      # Make it executable
      chmod +x $out/bin/pocket-ic-server

      exit 1;
    '';
  };

in {
  inherit server;
}
