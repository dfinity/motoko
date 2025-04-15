# This default.nix file is only necessary in .github/workflows/test.yml where we
# use nix-build-uncached which doesn't support Nix Flakes yet. This expression
# loads the flake and selects the packages for the current system from it.
(builtins.getFlake (toString ./.)).packages.${builtins.currentSystem}
