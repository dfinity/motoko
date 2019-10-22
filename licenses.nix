# Provides some helpers for working with runtime dependencies and licenses


self: pkgs:

# Helpers for figuring out the runtime (derivations) dependencies of a
# derivation.
let
  # Nix doesn't tell us much about the derivations of the runtime dependencies,
  # only their paths. So we first create a mapping from derivation outPath to
  # derivation from all the (top-level) derivation's dependencies, which we
  # then use to look up the actual derivations of the runtime dependencies.

  # Creates a mapping of derivation outPath to derivation of all the
  # dependencies of 'drv0'
  reverseDepsLookupList = drv0:
    let
      wrap = drv:
        { key = drv.outPath ; inherit drv; };

      # We include all the outputs because they each have different outPaths
      drvOutputs = drv:
        # XXX: some derivations, like stdenv, don't have "outputs"
        if builtins.hasAttr "outputs" drv
        then map (output: drv.${output}) drv.outputs
        else [ drv ];

      # Recurse into the derivation fields to find new derivations
      drvDeps = attrs:
          pkgs.lib.mapAttrsToList (k: v:
          if pkgs.lib.isDerivation v then (drvOutputs v)
          else if pkgs.lib.isList v
            then pkgs.lib.concatMap drvOutputs
              (pkgs.lib.filter (x: pkgs.lib.isDerivation x) v)
          else []
          ) attrs;
    in builtins.genericClosure
      { startSet = map wrap (drvOutputs drv0) ;
        operator = obj: map wrap ( pkgs.lib.concatLists (
          (drvDeps obj.drv.drvAttrs) ) ) ;
      };
in

# License specific
let
  # Creates a simple report of the form:
  #   libressl-2.8.3: publicDomain, bsdOriginal, bsd0, bsd3, gpl3, isc
  mkDrvLicenseReport = drv:
    let
      name =
        if builtins.hasAttr "meta" drv &&
          builtins.hasAttr "homepage" drv.meta
        then ''<a href="${drv.meta.homepage}">${drv.name}</a>''
        else drv.name;
      license =
        if builtins.hasAttr "meta" drv &&
          builtins.hasAttr "license" drv.meta
        then
          if pkgs.lib.isList drv.meta.license then
            pkgs.lib.concatStringsSep ", " (
              map renderLicense drv.meta.license)
          else renderLicense drv.meta.license
        else "(no license)";
    in "${name}: ${license}";

  renderLicense = license:
    if pkgs.lib.isAttrs license
    then if pkgs.lib.hasAttr "spdxId" license
      then "<a href=\"https://spdx.org/licenses/${license.spdxId}.html\">${license.spdxId} (${license.shortName})</a>"
      else license.shortName
    else if pkgs.lib.isString license
      then license
      else abort "no idea how to handle license of type " +
             "${builtins.typeOf license}";

  # [ { path = "/nix/store/..."; license = "BSD3, foo, bar"; } ]
  buildTimeLicensesList = drv:
    map (obj:
      { path = obj.drv.outPath;
        name = obj.drv.name;
        license = mkDrvLicenseReport obj.drv;
      }
    ) (reverseDepsLookupList drv);

  # Creates an HTML report of the licenses used by the derivation
  drvReport = drv:
    let
      allLicenses = pkgs.writeText "${drv.name}-all-licenses"
        (builtins.toJSON (buildTimeLicensesList drv));

      # Creates a file which is a JSON list of the runtime dependencies.
      cinfo = pkg: self.runCommandNoCC "cinfo-${pkg.name}"
        { buildInputs = [ self.jq ]; }
        ''
          cat ${self.closureInfo { rootPaths = [ pkg ]; }}/store-paths |\
            grep -v "^$" |\
            jq -R -s -c 'split("\n")' |\
            jq -c 'map(select( length > 0 ))' > $out
        '';
    in pkgs.runCommand "${drv.name}-licenses-report" { buildInputs = [ pkgs.jq ]; }
    # The next few lines output an HTML document.
    # The runtime dependency section tells jq to iterate over all licenses
    # (allLicenses) and only select those of which the path (outPath) is
    # present in the list of runtime dependency paths.
    ''
      (
        echo "<h1>Dependencies of <tt>${drv.name}</tt></h1>"
        echo "<h2>Run-time</h2>"
        echo "<ul>"
        cat ${allLicenses} |\
          jq -r --slurpfile runtime ${cinfo drv} \
            'unique_by(.name) | map(select(. as $obj | $runtime | any(.[] | . == $obj.path)) | .license)
            | .[] | "  <li>\(.)</li>"'
        echo "</ul>"
        echo "<h2>All licenses</h2>"
        echo "<ul>"
        cat ${allLicenses} |\
          jq -r 'unique_by(.name) | map(.license) | .[] | "  <li>\(.)</li>"'
        echo "</ul>"
      ) > $out
    '';

  # Wraps the drvReport in a derivation that produces hydra build products
  runtimeLicensesReport = drv:
    pkgs.runCommand "${drv.name}-licenses-html" {}
      ''
        mkdir -p $out
        cp ${drvReport drv} $out/licenses.${drv.name}.html

        mkdir -p $out/nix-support
        echo "report licenses $out/licenses.${drv.name}.html" >> \
          $out/nix-support/hydra-build-products
      '';
in {
  lib = pkgs.lib // {
    runtime = { inherit runtimeLicensesReport; } ;
  };
}
