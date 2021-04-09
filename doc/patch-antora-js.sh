#!/usr/bin/env bash

# Warning: Horrible use of duct tape here
#
# The official antora build fetches moc from https://download.dfinity.systems/motoko/
# and the base files via https://data.jsdelivr.com/v1/package/gh/ and
# https://cdn.jsdelivr.net/gh/
#
# But in this preview, we want to use the build artifacts of _this_ build.
# So what we do here is to patch the output files to change the paths.
#
# We also generate a fake JSON file list, like the one that jsdelivr creates.

if ! [ -d build ]; then
  echo "Did not find build/, did you run antora?"
  exit 1
fi

MOC_JS="${MOC_JS:-../src/moc.js}"

if ! [ -r "$MOC_JS" ]; then
  echo "Did not find \"$MOC_JS\". Please run make -C ../src moc.js or set  \$MOC_JS."
  exit 1
fi

if ! [ -d "$MOTOKO_BASE" ]; then
  echo "Did not find \"$MOTOKO_BASE\". Please set \$MOTOKO_BASE"
  exit 1
fi

cp -v "$MOC_JS" build/site/_/js/vendor/

mkdir -p build/site/_/js/vendor/dfinity/motoko-base/src
cat > build/site/_/js/vendor/dfinity/motoko-base/flat <<__END__
  { "default": null,
    "files": [
__END__

for file in "$MOTOKO_BASE"/*; do
  base="$(basename "$file")"
  cp -v "$file" build/site/_/js/vendor/dfinity/motoko-base/src
  cat >> build/site/_/js/vendor/dfinity/motoko-base/flat <<__END__
    { "name": "/src/$base" },
__END__
done
cat >> build/site/_/js/vendor/dfinity/motoko-base/flat <<__END__
    { "name": "does-not-exist-but-JSON-does-not-like-trailing-commas" }
  ]
}
__END__


echo "Patching build/site/docs/*/*.html"
sed -i -e 's,https://download.dfinity.systems/motoko/[0-9\.]*/js/moc-interpreter-[0-9\.]*.js,../../_/js/vendor/moc.js,' build/site/docs/*/*.html
# shellcheck disable=SC2016
sed -i -e 's,https://data.jsdelivr.com/v1/package/gh/${repo}@${version}/flat,../../_/js/vendor/${repo}/flat,' build/site/docs/*/*.html
# shellcheck disable=SC2016
sed -i -e 's,https://cdn.jsdelivr.net/gh/${repo}@${version},../../_/js/vendor/${repo},' build/site/docs/*/*.html

