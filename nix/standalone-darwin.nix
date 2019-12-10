{ runCommandNoCC
, stdenv
, grep
, removeReferencesTo
, lib
}:

{ drv
, exename
, extraBins ? []
}:

# on darwin we don't support shipping extra binaries
assert extraBins == [];

# these are (1) the references we're okay with getting rid of and the dynamic
# libs that we're fine with pulling from /usr/lib. These should be passed as
# arguments, but they're very generic and for now it's simpler to have them
# here.
let
  allowStrippedRefs = [ "*-crates-io" "*-swift-corefoundation" ];
  allowBundledDeps = [ "libSystem*.dylib" "libresolv*.dylib" "libc++*.dylib" ];
in

# For simplicity we assume those aren't empty. Empty lists would be the case
# analysis.
assert allowStrippedRefs != [];
assert allowBundledDeps != [];

  # Create a standalone executable from the given rust executable.
  # * drv: the base derivation
  # * name: the executable name, e.g. <drv>/bin/<name>
  # The output is placed in $out/bin/<name>.
  # This works in two steps:
  # 1. Strip out dynamic libraries
  # 2. Strip out other nix store references
runCommandNoCC "${exename}-mkstandalone"
    { buildInputs = [ grep stdenv.cc removeReferencesTo ];
      inherit exename;
      allowedRequisites = [];
    }
    ''
      dir=$(mktemp -d)
      exe=$dir/$exename

      cp ${drv}/bin/$exename $exe

      # Make sure we can patch the exe
      chmod +w $exe

      # For all dynamic dependencies...
      otool -L $exe \
        | grep --only-match '${builtins.storeDir}/\S*' > libs
      while read lib; do
        libname=$(basename $lib)

        case $libname in
          # ... strip out the widespread ones by making the RPATH entries
          # point to /usr/lib.
          # XXX: We should be able to statically link libc++ although we've
          # wasted ~ 2 eng/days already, so this is good enough for now.
          # See: https://github.com/rust-lang/rust/issues/64612
          ${lib.concatStringsSep "|" allowBundledDeps})
            newlibname=/usr/lib/''${libname%%\.*}.''${libname##*\.}
            echo "Found $libname:"
            echo "    $lib -> $newlibname"
            install_name_tool -change "$lib" "$newlibname" $exe
            ;;
          *)
            echo "Found unknown library: $libname ($lib)"
            echo "Please handle this case."
            exit 1
            ;;
        esac
      done <libs

      echo "$exename was patched:"
      otool -L $exe

      # Now grab all the other references to the nix store. If it's a reference
      # to source code (crates-io) then strip it out.
      (grep --only-matching -a '${builtins.storeDir}/[^/]*' $exe || true) \
        | uniq > deps

      while read dep; do
        depname=$(basename $dep)

        case "$depname" in
          ${lib.concatStringsSep "|" allowStrippedRefs})
            remove-references-to -t /nix/store/$depname $exe
            ;;
          *)
            echo "Unknown dependency: $dep"
            exit 1
            ;;
        esac
      done <deps


      mkdir -p $out/bin
      cp $exe $out/bin/$exename
    ''
