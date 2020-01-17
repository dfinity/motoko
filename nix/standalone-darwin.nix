{ runCommandNoCC
, stdenv
, grep
, removeReferencesTo
, callPackage
, lib
}:

{ drv
, exename
, extraBins ? []
, allowedStrippedRefs ? [ "*-crates-io" "*-swift-corefoundation" ]
, usePackager ? true

  # this arg isn't used on darwin, but needs to be present for linux compatibility
, allowedBundledDeps ? []
}:

# these are (1) the references we're okay with getting rid of and the dynamic
# libs that we're fine with pulling from /usr/lib. These should be passed as
# arguments, but they're very generic and for now it's simpler to have them
# here.
let

  inherit (callPackage ./common.nix { inherit drv exename; })
    fingerprint builder strip-references relocate-darwin-syslibs
    ;

  stripDependencies = [ stdenv.cc removeReferencesTo strip-references relocate-darwin-syslibs ];

  # Create a standalone executable from the given rust executable.
  # * drv: the base derivation
  # * name: the executable name, e.g. <drv>/bin/<name>
  # The output is placed in $out/bin/<name>.
  # This works in two steps:
  # 1. Strip out dynamic libraries
  # 2. Strip out other nix store references
  bundle =
    runCommandNoCC "${exename}-mkstandalone"
      {
        buildInputs = [ grep ] ++ stripDependencies;
        inherit exename extraBins;
        inherit allowedStrippedRefs;
        allowedRequisites = [];
      }
      ''
        bundle=$(mktemp -d)

        # rewrite the mach-o header to point to /usr/lib for system libraries
        # (as opposed to the nix store) and strips all other references to the
        # nix store
        patchbin() {
          relocate-darwin-syslibs "$1"
          strip-references "$1"
        }

        mkdir -p $bundle/b
        for bin in $extraBins; do
          echo bundling binary $bin
          cp $bin $bundle/b
          patchbin "$bundle/b/$(basename "$bin")"
        done

        exe="$bundle/$exename"
        cp ${drv}/bin/$exename "$exe"
        patchbin "$exe"

        tar cf $out -C $bundle .
      '';
in

if usePackager
then runCommandNoCC exename { allowedRequisites = []; }
  ''
    mkdir -p $out/bin
    cp ${builder bundle}/bin/packager $out/bin/${exename}
  ''
else runCommandNoCC exename {
  allowedRequisites = [];
  inherit allowedStrippedRefs;
  nativeBuildInputs = stripDependencies;
}
  ''
    mkdir -p $out/bin
    cp ${drv}/bin/${exename} $out/bin/${exename}
    relocate-darwin-syslibs $out/bin/${exename}
    strip-references $out/bin/${exename}
  ''
