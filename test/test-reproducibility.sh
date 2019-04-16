#!/usr/bin/env bash

# This script compiles the test suite under various
# different environments (using reprotest) and tests whether
# the binary output is the same.

# It needs the tool reprotest to be installed

ASC=${ASC:-$(realpath $(dirname $0)/../src/asc)}

files="*/*.as"

reprotest --source-pattern "*/*.as" \
  --vary=-fileordering,-user_group,-domain_host \
  "for file in */*.as; do ${ASC} -c --map --dfinity \$file 2> \$(basename \$file).stderr || true ; done" \
  '*.wasm *.map *.stderr'

