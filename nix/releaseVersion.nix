# Extracts the first version number in Changelog.md
{ pkgs, officialRelease ? false }:
let
  version =
    builtins.head (builtins.head (builtins.filter (x: x != null) (
      builtins.map (builtins.match "## ([0-9.]+).*") (
        pkgs.lib.splitString "\n" (builtins.readFile ../Changelog.md)
      )
    )));
in
if officialRelease then version else "${version}+"
