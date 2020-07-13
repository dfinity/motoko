#!/usr/bin/env bash

if [ "$(uname)" = "Linux" ]
then
  # nothing to do on linux
  true
else
  # on osx, this should give the expected warning
  moc --check lib/pkg/wrong-case-import.mo |
    grep -q -F "warning, file other-Module.mo has been located with a name of different case"
fi
