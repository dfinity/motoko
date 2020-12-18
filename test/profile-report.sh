#!/usr/bin/env bash

# This script runs the programs in perf/ with wasm-profiler instrumentation
# and dumps flamegraphs and an index.html in _profile.
#
# It expect on the path:
# moc drun wasm-profiler-instrument wasm-profiler-postproc flamegraph
# (Should all be present in the nix shell)

set -e

cd "$(dirname "${BASH_SOURCE[0]}")"

if ! [ -d perf ]; then
  echo "Did not find perf/"
  exit 1
fi

rm -rf _profile
mkdir _profile

rm -rf _profile_build
mkdir _profile_build

cat > _profile/index.html <<__END__
<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Motoko prof flamegraphs</title>
  </head>
  <body>
  <h1>Motoko prof flamegraphs</h1>
__END__

for file in perf/*.mo; do
  base="$(basename "$file" .mo)"
  cat >> _profile/index.html <<__END__
  <h2>$base.mo</h1>
  <img src="$base.svg"></img>
  <img src="$base-reverse.svg"></img>
__END__
done

  cat >> _profile/index.html <<__END__
 </body>
</html>
__END__

for file in perf/*.mo; do
  base="$(basename "$file" .mo)"
  echo "Profiling $base..."
  moc -g "$file" -o "_profile_build/$base.wasm"
  wasm-profiler-instrument --ic-system-api -i "_profile_build/$base.wasm" -o "_profile_build/$base.instrumented.wasm"
  # qr.mo takes far too long with profiling instrumentation.
  # so limit to 60s and 30MB
  timeout 60s ./drun-wrapper.sh "_profile_build/$base.instrumented.wasm" "$file" |&
    head -c $((100*1024*1024)) |
    wasm-profiler-postproc flamegraph "_profile_build/$base.instrumented.wasm" > "_profile_build/$base.flamegraph"
  flamegraph --title "$base.mo" < "_profile_build/$base.flamegraph" > "_profile/$base.svg"
  flamegraph --title "$base.mo (reverse)" --reverse < "_profile_build/$base.flamegraph" > "_profile/$base-reverse.svg"
done
