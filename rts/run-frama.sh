#!/usr/bin/env bash

# See README.md, Static analysis with Frama-C

set -e

frama-c \
  -eva \
  -lib-entry \
  -out-external \
  -input frama-main.c,idl.c \
  -no-warn-signed-overflow \
  -eva-stop-at-nth-alarm 0 \
  -eva-split-return 0 \
  -eva-precision 6 \
  -eva-split-limit 256 \
  -eva-builtin malloc:Frama_C_malloc_fresh \
  -eva-warn-undefined-pointer-comparison none

# -no-warn-signed-overflow seems to otherwise give false warnings?
# or else I do not understand why there is a signed overflow in a left-shift
# with all arguments unsigned.
