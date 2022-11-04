#!/usr/bin/env bash

# This script compiles the test suite under various
# different environments (using reprotest) and tests whether
# the binary output is the same.

# It needs the tool reprotest to be installed

files="*/*.mo"

reprotest --source-pattern "*/*.mo" \
  --vary=-fileordering,-user_group,-domain_host \
  "for file in */*.mo; do moc -c --map \$file 2> \$(basename \$file).stderr || true ; done" \
  '*.wasm *.map *.stderr'

