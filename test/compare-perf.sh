#!/usr/bin/env bash

# This script compares the perf
# between the currently built version and the previous version
#
# With -f from -t to other branches can be selected (anything that git
# rev-parse understands)

old="$(git rev-parse HEAD)"
new=""

while getopts "f:t:" o; do
    case "${o}" in
        f)
            old="$(git rev-parse ${OPTARG})"
            ;;
        t)
            new="$(git rev-parse ${OPTARG})"
            ;;
    esac
done
shift $((OPTIND-1))

function build_ref_to {
  rm -f $2-moc
  if [ -z "$1" ]
  then
    echo "Building $2 moc from working copy.."
    chronic nix-build -E '((import ./..) {}).tests.perf' \
      -o $2-perf
  else
    echo "Building $2 moc (rev $1).."
    chronic nix-build \
      --argstr ref "$(git for-each-ref --count 1 --contains "$1" --format '%(refname)')" \
      --argstr rev "$1" \
      --argstr path "$(realpath "$(dirname $0)/..")" \
      -E '
      {rev, ref, path}:
      let nixpkg = import ../nix {}; in
      let checkout = (builtins.fetchGit {url = path; ref = ref; rev = rev;}).outPath; in
      builtins.trace checkout (
      ((import checkout) {}).tests.perf)' \
      -o $2-perf
  fi
  test -e $2-perf || exit 1
}
build_ref_to "$old" old
build_ref_to "$new" new

$(dirname $0)/diff-stats.pl old-perf/stats.csv new-perf/stats.csv
