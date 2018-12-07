#!/usr/bin/env bash

# A simple test runner. Synopsis:
#
# ./run.sh foo.as [bar.as ..]
#
# Options:
#
#    -a:  Update the files in ok/
#    -d: Compile with --dfinity, use dvm to run
#

realpath() {
    [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}


ACCEPT=no
DFINITY=no
EXTRA_ASC_FLAGS=
ASC=${ASC:-$(realpath $(dirname $0)/../src/asc)}
WASM=${WASM:-wasm}
DVM_WRAPPER=$(realpath $(dirname $0)/dvm.sh)

while getopts "ad" o; do
    case "${o}" in
        a)
            ACCEPT=yes
            ;;
        d)
            DFINITY=yes
            EXTRA_ASC_FLAGS=--dfinity
            ;;
    esac
done

shift $((OPTIND-1))

failures=no

for file in "$@";
do
  if ! [ -r $file ]
  then
    echo "File $file does not exist."
    failures=yes
    continue
  fi

  # We run all commands in the directory of the .as file,
  # so that no paths leak into the output
  pushd $(dirname $file) >/dev/null

  base=$(basename $file .as)
  out=_out
  ok=ok

  echo -n "$base:"
  [ -d $out ] || mkdir $out
  [ -d $ok ] || mkdir $ok

  rm -f $out/$base.{tc,wasm,wasm.map,wasm-run,dvm-run}

  # First run all the steps, and remember what to diff
  diff_files=

  # Typeckeck
  echo -n " [tc]"
  $ASC $ASC_FLAGS --check $base.as > $out/$base.tc 2>&1
  tc_succeeded=$?
  diff_files="$diff_files $base.tc"

  if [ "$tc_succeeded" -eq 0 ];
  then
    # Interpret
    echo -n " [run]"
    $ASC $ASC_FLAGS -r $base.as > $out/$base.run 2>&1
    diff_files="$diff_files $base.run"

    # Interpret with lowering
    echo -n " [run-low]"
    $ASC $ASC_FLAGS -r -a -A $base.as > $out/$base.run-low 2>&1
    diff_files="$diff_files $base.run-low"

    # Diff interpretations without/with lowering
    echo -n " [diff-low]"
    diff -u -N $out/$base.run $out/$base.run-low > $out/$base.diff-low 
    diff_files="$diff_files $base.diff-low"

    # Compile
    echo -n " [wasm]"
    $ASC $ASC_FLAGS $EXTRA_ASC_FLAGS --map -c $base.as 2> $out/$base.wasm.stderr
    diff_files="$diff_files $base.wasm.stderr"
    if [ -e $base.wasm ]
    then
      mv $base.wasm $base.wasm.map $out

      if [ "$SKIP_RUNNING" != yes ]
      then
        if [ $DFINITY = 'yes' ]
        then
          echo -n " [dvm]"
          $DVM_WRAPPER $out/$base.wasm > $out/$base.dvm-run 2>&1
          diff_files="$diff_files $base.dvm-run"
        else
          echo -n " [wasm-run]"
          $WASM _out/$base.wasm  > $out/$base.wasm-run 2>&1
          sed 's/wasm:0x[a-f0-9]*:/wasm:0x___:/g' $out/$base.wasm-run >$out/$base.wasm-run.temp
          mv -f $out/$base.wasm-run.temp $out/$base.wasm-run
          diff_files="$diff_files $base.wasm-run"
        fi
      fi
    fi
  fi
  echo ""

  # normalize files
  for file in $diff_files
  do
    if [ -e "$out/$file" ]
    then
      grep -E -v '^Raised at|^Re-raised at|^Re-Raised at|^Called from' $out/$file > $out/$file.norm
      mv $out/$file.norm $out/$file
    fi
  done

  if [ $ACCEPT = yes ]
  then
    for outfile in $diff_files
    do
      if [ -s $out/$outfile ]
      then
        cp $out/$outfile $ok/$outfile.ok
      else
        rm -f $ok/$outfile.ok
      fi
    done
  else
    for file in $diff_files
    do
      diff -u -N $ok/$file.ok $out/$file
      if [ $? != 0 ]; then failures=yes; fi
    done
  fi
  popd >/dev/null
done

if [ $failures = yes ]
then
  echo "Some tests failed."
  exit 1
else
  echo "All tests passed."
fi



