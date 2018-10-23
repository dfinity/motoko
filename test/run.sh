#!/usr/bin/env bash

# A simple test runner. Synopsis:
#
# ./run.sh foo.as [bar.as ..]
#
# Options:
#
#    -a:  Update the files in ok/
#    -d: Compile with --dfinity, use dsh to run
#


ACCEPT=no
DFINITY=no
EXTRA_ASC_FLAGS=
ASC=${ASC:-$(realpath $(dirname $0)/../src/asc)}
WASM=${WASM:-wasm}
DSH_WRAPPER=$(realpath $(dirname $0)/dsh.sh)

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

  rm -f $out/$base.{tc,wasm,wasm.map,wasm-run,dsh-run}

  # First run all the steps, and remember what to diff
  diff_files=

  # Typeckeck
  echo -n " [tc]"
  $ASC $ASC_FLAGS --check $base.as > $out/$base.tc 2>&1
  tc_succeeded=$?
  diff_files="$diff_files $base.tc"

  if [ "$tc_succeeded" -eq 0 ];
  then
    echo -n " [run]"

    # Interpret
    $ASC $ASC_FLAGS -r -v $base.as > $out/$base.run 2>&1
    diff_files="$diff_files $base.run"

    # Compile
    echo -n " [wasm]"
    $ASC $ASC_FLAGS $EXTRA_ASC_FLAGS --map -c $base.as 2> $out/$base.wasm.stderr
    diff_files="$diff_files $base.wasm.stderr"
    if [ -e $base.wasm ]
    then
      mv $base.wasm $base.wasm.map $out

      if [ $DFINITY = 'yes' ];
      then
        echo -n " [dsh]"
        $DSH_WRAPPER $out/$base.wasm > $out/$base.dsh-run 2>&1
        diff_files="$diff_files $base.dsh-run"
      else
        echo -n " [wasm-run]"
        $WASM _out/$base.wasm  > $out/$base.wasm-run 2>&1
        diff_files="$diff_files $base.wasm-run"
      fi
    fi
  fi
  echo ""

  if [ $ACCEPT = yes ]
  then
    for outfile in $diff_files; do
      if [ -s $out/$outfile ]
      then
        cp $out/$outfile $ok/$outfile.ok
      else
        rm -f $ok/$outfile.ok
      fi
    done
  else
    for file in $diff_files; do
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



