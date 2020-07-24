#!/usr/bin/env bash

if [ "$(uname)" = "Linux" ]
then
  # nothing to do on linux
  true
else
  # on osx, this should give the expected warning
  WARNING='file other-Module.mo has been located with a name of different case'

  $MOC --check lib/pkg/wrong-case-import.mo \
      |& grep -q -F "warning, $WARNING" \
      || echo "warning '$WARNING' not encountered"
fi
