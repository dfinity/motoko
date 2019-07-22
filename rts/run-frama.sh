#!/usr/bin/env bash

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

