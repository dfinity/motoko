#!/bin/bash

set -e

for f in $(find $1 -name '*.adoc'); do
  echo $f
  sed -i 's/\[source, *motoko\]/\[source, motoko\]/g' $f
  sed -i 's/\[source.no-repl, *motoko\]/\[source, motoko no-repl\]/g' $f
  sed -i 's/\[source.run, *motoko\]/\[source, motoko run\]/g' $f
  sed -i 's/\[source, *candid\]/\[source,candid\]/g' $f
  sed -i 's/\[source, *bnf\]/\[source,bnf\]/g' $f
  sed -i 's/\[source, *bash\]/\[source,bash\]/g' $f
  sed -i 's/\[source\#\([a-zA-Z]*\), *motoko\]/\[source, motoko name=\1\]/g' $f
  sed -i 's/\[source\.include\_\([a-zA-Z]*\), *motoko\]/\[source, motoko include\=\1\]/p' $f
  sed -i 's/\[source\.include\_\([a-zA-Z]*\)\_\([a-zA-Z]*\), *motoko\]/\[source, motoko include=\1KOMMA\2\]/g' $f #dirty German hack
  sed -i 's/\[source\#\([a-zA-Z]*\)\.include\_\([a-zA-Z]*\), *motoko\]/\[source, motoko name=\1 include\=\2\]/g' $f
  sed -i 's/\[source\#\([a-zA-Z]*\)\.include\_\([a-zA-Z]*\)\_\([a-zA-Z]*\), *motoko\]/\[source, motoko name=\1 include\=\2KOMMA\3\]/g' $f #dirty German hack
  sed -i 's/include::/INCLUDE::/g' $f
  sed -i -z 's/\n\n\/\/\/\//\n\n```COMMENT/g' $f
  sed -i -z 's/\n\/\/\/\//\nENDCOMMENT\n```/g' $f
  asciidoctor -a IC="Internet Computer" -a proglang=Motoko -a company-id=DFINITY -b docbook -a leveloffset=+1 -o $2/$(basename $f .adoc).xml $f
  pandoc -t gfm  --no-highlight --wrap=none -f docbook $2/$(basename $f .adoc).xml \
  > $2/$(basename $f .adoc).md || true
  sed -i 's/``` COMMENT/<!---/g' $2/$(basename $f .adoc).md
  sed -i -z 's/ENDCOMMENT\n```/--->/g' $2/$(basename $f .adoc).md
  #sed -i -z 's/\nINCLUDE::\.\([a-zA-Z0-9\.\/\_\-]*\)\[\]/ file=\1/g' $2/$(basename $f .adoc).md
  sed -i -z 's/\nINCLUDE::\.\([^[]*\)\[\]/ file=\1/g' $2/$(basename $f .adoc).md
  sed -i -z 's/\nINCLUDE::\.\([^[]*\)\[lines=\([0-9]*\)\.\.\([0-9]*\)\]/ file=\1\#L\2-L\3/g' $2/$(basename $f .adoc).md
  sed -i 's/KOMMA/,/g' $2/$(basename $f .adoc).md #undo German HACK above
  sed -i 's/.xml/.md/g' $2/$(basename $f .adoc).md
  sed -i 's/<\/div>/:::/g' $2/$(basename $f .adoc).md
  sed -i 's/<div class="tip">/:::tip/g' $2/$(basename $f .adoc).md
  sed -i 's/<div class="warning">/:::danger/g' $2/$(basename $f .adoc).md
  sed -i 's/<div class="note">/:::note/g' $2/$(basename $f .adoc).md
  echo $f
done
