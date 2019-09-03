{ pkgs }:

let
  stdenv = pkgs.stdenv;
  lib = stdenv.lib;

  projectName = "dfinity-client";
  projectPath = ./.;

  nodejs = pkgs.nodejs-10_x;

  nodeModules = pkgs.node2nix-node-modules {
    inherit nodejs projectName projectPath;
  };
in
  {
    js-dfinity-client = stdenv.mkDerivation {
      name = "js-dfinity-client";
      src = lib.sourceByRegex projectPath [
        "^index.js$"
        "^package.json$"
        "^(src|tests)(\/[^\/]+)*$"
      ];
      buildInputs = [
        nodejs
        pkgs.haskellPackages.dvm
        pkgs.actorscript.asc
      ];
      buildPhase = ''
        cp -r ${nodeModules.out}/. node_modules
      '';
      installPhase = ''
        mkdir -p $out
        cp index.js $out
        cp package.json $out
        cp -r src $out
      '';
      doInstallCheck = true;
      installCheckPhase = ''
        npm test
      '';
    };

    js-dfinity-client-shell = stdenv.mkDerivation {
      skipCi = true;
      name = "js-dfinity-client-shell";
      buildInputs = [
        nodejs
        pkgs.jq
        pkgs.haskellPackages.dvm
      ];
      shellHook = ''
        if [ ! -f package.json ]; then
          echo "error: dfinity-client-shell must be run from the root of the "dfinity-client" project, but package.json not found"
          exit 1
        fi

        PACKAGE_NAME=$(jq ".name" package.json)

        if [ $PACKAGE_NAME != "\"${projectName}\"" ]; then
          echo "error: dfinity-client-shell must be run from the root of the \dfinity-client\" project, but package name found was $PACKAGE_NAME"
          exit 1
        fi

        # Remove any assets that may already exist
        mkdir -p node_modules
        chmod -R 755 node_modules
        rm -rf node_modules

        cp -r ${nodeModules.out}/. node_modules

        # Change directory ownership so that we can remove them on exit
        chmod -R 755 node_modules

        # Clean up before we exit the shell
        trap "{ rm -rf node_modules; exit 255; }" EXIT
      '';
    };
  }
