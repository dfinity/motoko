#!/bin/bash
mkdir -p _out
for p in .mops/*; do
    p_name_at_version=$(basename $p)
    p_name=${p_name_at_version%@*}
    echo package $p
    rm -f _out/$(basename $p_name).mo;
    for f in $p/src/*.mo;  do
      #echo file $f
      if [ $p_name = 'base' ]; then
          continue 2
      fi
      #echo $NAME
      echo "import _ \"mo:$p_name/$(basename $f ".mo")\";" >> _out/$(basename $p_name_at_version).mo;
    done
    moc --check --package base $MOTOKO_BASE \
        $(npx mops sources | \
              grep -v 'package base ' | \
              grep -v 'package icrc-nft-mo') \
        _out/$p_name_at_version.mo &> \
        _out/$p_name_at_version.tc
done
