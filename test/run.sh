#!/usr/bin/env bash

# A simple test runner. Synopsis:
#
# ./run.sh foo.as [bar.as ..]
#
# Options:
#
#    -a: Update the files in ok/
#    -d: Compile without -no-dfinity-api, uses dvm to run
#    -s: Be silent in sunny-day execution
#

function realpath() {
    [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}


ACCEPT=no
DFINITY=no
EXTRA_ASC_FLAGS=
ASC=${ASC:-$(realpath $(dirname $0)/../src/asc)}
AS_LD=${AS_LD:-$(realpath $(dirname $0)/../src/as-ld)}
DIDC=${DIDC:-$(realpath $(dirname $0)/../src/didc)}
export AS_LD
WASM=${WASM:-wasm}
DVM_WRAPPER=$(realpath $(dirname $0)/dvm.sh)
ECHO=echo

while getopts "ads" o; do
    case "${o}" in
        a)
            ACCEPT=yes
            ;;
        d)
            DFINITY=yes
            ;;
        s)
            ECHO=true
            ;;
    esac
done

if [ $DFINITY = "no" ]
then
    EXTRA_ASC_FLAGS=-no-dfinity-api
fi

shift $((OPTIND-1))

failures=no

function normalize () {
  if [ -e "$1" ]
  then
    grep -a -E -v '^Raised by|Raised at|^Re-raised at|^Re-Raised at|^Called from' $1 |
    sed 's/\x00//g' |
    sed 's/\x1b\[[0-9;]*[a-zA-Z]//g' |
    sed 's/^.*[IW], hypervisor:/hypervisor:/g' |
    sed 's/wasm:0x[a-f0-9]*:/wasm:0x___:/g' |
    sed 's/prelude:[^:]*:/prelude:___:/g' |
    sed 's/^.*run-dfinity\/\.\.\/dvm.sh: line/dvm.sh: line/g' |
    sed 's/ *[0-9]* Illegal instruction.*dvm/ Illegal instruction dvm/g' |
    sed 's/ calling func\$[0-9]*/ calling func$NNN/g' |
    cat > $1.norm
    mv $1.norm $1
  fi
}

for file in "$@";
do
  if ! [ -r $file ]
  then
    echo "File $file does not exist."
    failures=yes
    continue
  fi

  if [ ${file: -3} == ".as" ]
  then base=$(basename $file .as)
  elif [ ${file: -3} == ".sh" ]
  then base=$(basename $file .sh)
  elif [ ${file: -4} == ".wat" ]
  then base=$(basename $file .wat)
  elif [ ${file: -4} == ".did" ]
  then base=$(basename $file .did)
  else
    echo "Unknown file extension in $file, expected .as, .sh, .wat or .did"; exit 1
    failures=yes
    continue
  fi

  # We run all commands in the directory of the .as file,
  # so that no paths leak into the output
  pushd $(dirname $file) >/dev/null

  out=_out
  ok=ok

  $ECHO -n "$base:"
  [ -d $out ] || mkdir $out
  [ -d $ok ] || mkdir $ok

  rm -f $out/$base.{tc,wasm,wasm.map,wasm-run,dvm-run,filecheck,diff-ir,diff-low,stdout,stderr,linked.wat}
  if [ $ACCEPT = yes ]
  then
    rm -f $ok/$base.{tc,wasm,wasm.map,wasm-run,dvm-run,filecheck,diff-ir,diff-low,stdout,stderr,linked.wat}.ok
  fi

  # First run all the steps, and remember what to diff
  diff_files=

  if [ ${file: -3} == ".as" ]
  then
    # Typecheck
    $ECHO -n " [tc]"
    $ASC $ASC_FLAGS --check $base.as > $out/$base.tc 2>&1
    tc_succeeded=$?
    normalize $out/$base.tc
    diff_files="$diff_files $base.tc"

    if [ "$tc_succeeded" -eq 0 ];
    then
      if [ "$SKIP_RUNNING" != yes ]
      then
        # Interpret
        $ECHO -n " [run]"
        $ASC $ASC_FLAGS -r $base.as > $out/$base.run 2>&1
        normalize $out/$base.run
        diff_files="$diff_files $base.run"

        # Interpret IR without lowering
        $ECHO -n " [run-ir]"
        $ASC $ASC_FLAGS -r -iR -no-async -no-await $base.as > $out/$base.run-ir 2>&1
        normalize $out/$base.run-ir
        diff_files="$diff_files $base.run-ir"

        # Diff interpretations without/with lowering
        diff -u -N --label "$base.run" $out/$base.run --label "$base.run-ir" $out/$base.run-ir > $out/$base.diff-ir
        diff_files="$diff_files $base.diff-ir"

        # Interpret IR with lowering
        $ECHO -n " [run-low]"
        $ASC $ASC_FLAGS -r -iR $base.as > $out/$base.run-low 2>&1
        normalize $out/$base.run-low
        diff_files="$diff_files $base.run-low"

        # Diff interpretations without/with lowering
        diff -u -N --label "$base.run" $out/$base.run --label "$base.run-low" $out/$base.run-low > $out/$base.diff-low
        diff_files="$diff_files $base.diff-low"

      fi

      # Compile
      $ECHO -n " [wasm]"
      $ASC $ASC_FLAGS $EXTRA_ASC_FLAGS --map -c $base.as -o $out/$base.wasm 2> $out/$base.wasm.stderr
      normalize $out/$base.wasm.stderr
      diff_files="$diff_files $base.wasm.stderr"

      # Check filecheck
      if [ "$SKIP_RUNNING" != yes ]
      then
        if grep -F -q CHECK $base.as
        then
          $ECHO -n " [FileCheck]"
          wasm2wat --no-check --enable-multi-value $out/$base.wasm > $out/$base.wat
          cat $out/$base.wat | FileCheck $base.as > $out/$base.filecheck 2>&1
          diff_files="$diff_files $base.filecheck"
        fi
      fi

      # Run compiled program
      if [ -e $out/$base.wasm ]
      then
        if [ "$SKIP_RUNNING" != yes ]
        then
          if [ $DFINITY = 'yes' ]
          then
            $ECHO -n " [dvm]"
            $DVM_WRAPPER $out/$base.wasm $base.as > $out/$base.dvm-run 2>&1
            normalize $out/$base.dvm-run
            diff_files="$diff_files $base.dvm-run"
          else
            $ECHO -n " [wasm-run]"
            $WASM $out/$base.wasm  > $out/$base.wasm-run 2>&1
            normalize $out/$base.wasm-run
            diff_files="$diff_files $base.wasm-run"
          fi
        fi
      fi
    fi
  elif [ ${file: -3} == ".sh" ]
  then
    # The file is a shell script, just run it
    $ECHO -n " [out]"
    ./$(basename $file) > $out/$base.stdout 2> $out/$base.stderr
    normalize $out/$base.stdout
    normalize $out/$base.stderr
    diff_files="$diff_files $base.stdout $base.stderr"
  elif [ ${file: -4} == ".wat" ]
  then
    # The file is a .wat file, so we are expected to test linking
    $ECHO -n " [as-ld]"
    rm -f $out/$base.{base,lib,linked}.{wasm,wat,o}
    make --quiet $out/$base.{base,lib}.wasm
    $AS_LD -b $out/$base.base.wasm -l $out/$base.lib.wasm -o $out/$base.linked.wasm > $out/$base.as-ld 2>&1
    diff_files="$diff_files $base.as-ld"

    if [ -e $out/$base.linked.wasm ]
    then
        $ECHO -n " [wat]"
	wasm2wat $out/$base.linked.wasm -o $out/$base.linked.wat 2> $out/$base.linked.wat.stderr
        diff_files="$diff_files $base.linked.wat $base.linked.wat.stderr"
    fi

  else
    # The file is a .did file, so we are expected to test the idl
    # Typecheck
    $ECHO -n " [tc]"
    $DIDC --check $base.did > $out/$base.tc 2>&1
    tc_succeeded=$?
    normalize $out/$base.tc
    diff_files="$diff_files $base.tc"

    if [ "$tc_succeeded" -eq 0 ];
    then
      $ECHO -n " [js]"
      $DIDC --js $base.did > $out/$base.js 2>&1
      diff_files="$diff_files $base.js"
    fi
  fi
  $ECHO ""

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
      if [ -e $ok/$file.ok -o -e $out/$file ]
      then
        diff -a -u -N --label "$file (expected)" $ok/$file.ok --label "$file (actual)" $out/$file
        if [ $? != 0 ]; then failures=yes; fi
      fi
    done
  fi
  popd >/dev/null
done

if [ $failures = yes ]
then
  echo "Some tests failed."
  exit 1
else
  $ECHO "All tests passed."
fi
