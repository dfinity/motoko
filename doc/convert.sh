#!/bin/bash

set -e

for f in $(find $1 -name '*.adoc'); do
  echo $f
  asciidoctor -a IC="Internet Computer" -a proglang=Motoko -a company-id=DFINITY -b docbook -a leveloffset=+1 -o $2/$(basename $f .adoc).xml $f
  pandoc -t gfm  --no-highlight --wrap=none -f docbook $2/$(basename $f .adoc).xml \
  > $2/$(basename $f .adoc).md || true
  sed -i 's/.xml/.md/g' $2/$(basename $f .adoc).md
  echo $f
done
