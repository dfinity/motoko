#!/usr/bin/env bash

# See README.md, Static analysis with Frama-C

set -e

frama-c \
  -eva \
  -eva-slevel 20 \
  -lib-entry \
  -out-external \
  -input frama-main.c,idl.c \
  -no-warn-signed-overflow \
  -eva-stop-at-nth-alarm 0 \
  -eva-warn-undefined-pointer-comparison none

# -no-warn-signed-overflow seems to otherwise give false warnings?
# or else I do not understand why there is a signed overflow in a left-shift
# with all arguments unsigned.
