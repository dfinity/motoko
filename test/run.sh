#!/usr/bin/env bash

# A simple test runner. Synopsis:
#
# ./run.sh foo.mo [bar.mo ..]
#
# Options:
#
#    -a: Update the files in ok/
#    -1: Use Ancient API
#    -2: Use IC API
#    -s: Be silent in sunny-day execution
#    -i: Only check mo to idl generation
#    -r: Activate release mode (eliminate `debug` blocks)
#

function realpath() {
    [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}


ACCEPT=no
API=wasm
IDL=no
RELEASE=no
EXTRA_MOC_FLAGS=
MOC=${MOC:-$(realpath $(dirname $0)/../src/moc)}
MO_LD=${MO_LD:-$(realpath $(dirname $0)/../src/mo-ld)}
DIDC=${DIDC:-$(realpath $(dirname $0)/../src/didc)}
export MO_LD
WASM=${WASM:-wasm}
DVM_WRAPPER=$(realpath $(dirname $0)/dvm.sh)
DRUN_WRAPPER=$(realpath $(dirname $0)/drun-wrapper.sh)
ECHO=echo

while getopts "a12sir" o; do
    case "${o}" in
        a)
            ACCEPT=yes
            ;;
        1)
            API=ancient
            ;;
        2)
            API=ic
            ;;
        s)
            ECHO=true
            ;;
        i)
            IDL=yes
            ;;
        r)
            RELEASE=yes
            ;;
    esac
done

if [ $API = "wasm" ]; then EXTRA_MOC_FLAGS=-no-system-api; fi
if [ $API = "ancient" ]; then EXTRA_MOC_FLAGS=-ancient-system-api; fi
if [ $RELEASE = "yes" ]; then MOC_FLAGS=--release; fi

shift $((OPTIND-1))

failures=no

function normalize () {
  if [ -e "$1" ]
  then
    grep -a -E -v '^Raised by|^Raised at|^Re-raised at|^Re-Raised at|^Called from|^ *at ' $1 |
    sed 's/\x00//g' |
    sed 's/\x1b\[[0-9;]*[a-zA-Z]//g' |
    sed 's/^.*[IW], hypervisor:/hypervisor:/g' |
    sed 's/wasm:0x[a-f0-9]*:/wasm:0x___:/g' |
    sed 's/prelude:[^:]*:/prelude:___:/g' |
    sed 's/ calling func\$[0-9]*/ calling func$NNN/g' |
    sed 's/rip_addr: [0-9]*/rip_addr: XXX/g' |
    sed 's,/private/tmp/,/tmp/,g' |
    sed 's,/tmp/.*dfinity.[^/]*,/tmp/dfinity.XXX,g' |
    sed 's,/build/.*dfinity.[^/]*,/tmp/dfinity.XXX,g' |
    sed 's,/tmp/.*ic.[^/]*,/tmp/ic.XXX,g' |
    sed 's,/build/.*ic.[^/]*,/tmp/ic.XXX,g' |
    sed 's/^.*run-dfinity\/\.\.\/drun.sh: line/drun.sh: line/g' |
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

  if [ ${file: -3} == ".mo" ]
  then base=$(basename $file .mo)
  elif [ ${file: -3} == ".sh" ]
  then base=$(basename $file .sh)
  elif [ ${file: -4} == ".wat" ]
  then base=$(basename $file .wat)
  elif [ ${file: -4} == ".did" ]
  then base=$(basename $file .did)
  else
    echo "Unknown file extension in $file, expected .mo, .sh, .wat or .did"; exit 1
    failures=yes
    continue
  fi

  # We run all commands in the directory of the .mo file,
  # so that no paths leak into the output
  pushd $(dirname $file) >/dev/null

  out=_out
  ok=ok

  $ECHO -n "$base:"
  [ -d $out ] || mkdir $out
  [ -d $ok ] || mkdir $ok

  rm -f $out/$base.{tc,wasm,wasm.map,wasm-run,wasm.stderr,drun-run,filecheck,diff-ir,diff-low,stdout,stderr,linked.wat,did,did.tc,js.out}
  if [ $ACCEPT = yes ]
  then
    rm -f $ok/$base.{tc,wasm,wasm.map,wasm-run,wasm.stderr,drun-run,filecheck,diff-ir,diff-low,stdout,stderr,linked.wat,did,did.tc,js.out}.ok
  fi

  # First run all the steps, and remember what to diff
  diff_files=

  if [ ${file: -3} == ".mo" ]
  then
    # Typecheck
    $ECHO -n " [tc]"
    $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS --check $base.mo > $out/$base.tc 2>&1
    tc_succeeded=$?
    normalize $out/$base.tc
    diff_files="$diff_files $base.tc"

    if [ "$tc_succeeded" -eq 0 ]
    then
      if [ $IDL = 'yes' ]
      then
        $ECHO -n " [idl]"
        $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS --idl $base.mo -o $out/$base.did 2> $out/$base.idl.stderr
        idl_succeeded=$?
        normalize $out/$base.did
        normalize $out/$base.idl.stderr
        diff_files="$diff_files $base.did $base.idl.stderr"
        if [ "$idl_succeeded" -eq 0 ]
        then
          $ECHO -n " [didc]"
          $DIDC --check $out/$base.did > $out/$base.did.tc 2>&1
          diff_files="$diff_files $base.did.tc"
        fi
      else
        if [ "$SKIP_RUNNING" != yes ]
        then
          # Interpret
          $ECHO -n " [run]"
          $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS -r $base.mo > $out/$base.run 2>&1
          normalize $out/$base.run
          diff_files="$diff_files $base.run"

          # Interpret IR without lowering
          $ECHO -n " [run-ir]"
          $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS -r -iR -no-async -no-await $base.mo > $out/$base.run-ir 2>&1
          normalize $out/$base.run-ir
          diff_files="$diff_files $base.run-ir"

          # Diff interpretations without/with lowering
          diff -u -N --label "$base.run" $out/$base.run --label "$base.run-ir" $out/$base.run-ir > $out/$base.diff-ir
          diff_files="$diff_files $base.diff-ir"

	  if false; then
          # Interpret IR with lowering
          $ECHO -n " [run-low]"
          $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS -r -iR $base.mo > $out/$base.run-low 2>&1
          normalize $out/$base.run-low
          diff_files="$diff_files $base.run-low"

          # Diff interpretations without/with lowering
          diff -u -N --label "$base.run" $out/$base.run --label "$base.run-low" $out/$base.run-low > $out/$base.diff-low
          diff_files="$diff_files $base.diff-low"
          fi

        fi

        # Compile
        $ECHO -n " [wasm]"
        $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS --map -c $base.mo -o $out/$base.wasm 2> $out/$base.wasm.stderr
        normalize $out/$base.wasm.stderr
        diff_files="$diff_files $base.wasm.stderr"

        # Check filecheck
        if [ "$SKIP_RUNNING" != yes ]
        then
          if grep -F -q CHECK $base.mo
          then
            $ECHO -n " [FileCheck]"
            wasm2wat --no-check --enable-multi-value $out/$base.wasm > $out/$base.wat
            cat $out/$base.wat | FileCheck $base.mo > $out/$base.filecheck 2>&1
            diff_files="$diff_files $base.filecheck"
          fi
        fi

        # Run compiled program
        if [ -e $out/$base.wasm ]
        then
          if [ "$SKIP_RUNNING" != yes ]
          then
            if [ $API = ancient ]
            then
              $ECHO -n " [dvm]"
              $DVM_WRAPPER $out/$base.wasm $base.mo > $out/$base.dvm-run 2>&1
              normalize $out/$base.dvm-run
              diff_files="$diff_files $base.dvm-run"
            elif [ $API = ic ]
            then
              $ECHO -n " [drun]"
              $DRUN_WRAPPER $out/$base.wasm $base.mo > $out/$base.drun-run 2>&1
              normalize $out/$base.drun-run
              diff_files="$diff_files $base.drun-run"
            else
              $ECHO -n " [wasm-run]"
              $WASM $out/$base.wasm  > $out/$base.wasm-run 2>&1
              normalize $out/$base.wasm-run
              diff_files="$diff_files $base.wasm-run"
            fi
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
    $ECHO -n " [mo-ld]"
    rm -f $out/$base.{base,lib,linked}.{wasm,wat,o}
    make --quiet $out/$base.{base,lib}.wasm
    $MO_LD -b $out/$base.base.wasm -l $out/$base.lib.wasm -o $out/$base.linked.wasm > $out/$base.mo-ld 2>&1
    diff_files="$diff_files $base.mo-ld"

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
      $ECHO -n " [pp]"
      $DIDC --pp $base.did > $out/$base.pp.did
      sed -i 's/import "/import "..\//g' $out/$base.pp.did
      $DIDC --check $out/$base.pp.did > $out/$base.pp.tc 2>&1
      diff_files="$diff_files $base.pp.tc"
      
      $ECHO -n " [js]"
      $DIDC --js $base.did -o $out/$base.js >& $out/$base.js.out
      normalize $out/$base.js
      normalize $out/$base.js.out
      diff_files="$diff_files $base.js.out $base.js"

      if [ -e $out/$base.js ]
      then
        $ECHO -n " [node]"
        export NODE_PATH=$NODE_PATH:$ESM

        node -r esm $out/$base.js > $out/$base.node 2>&1
        normalize $out/$base.node
        diff_files="$diff_files $base.node"

        node -r esm -e \
        "import actorInterface from './$out/$base.js';
        import { makeActor, makeHttpAgent } from '$JS_USER_LIBRARY';
        const httpAgent = makeHttpAgent({ canisterId: 1 });
        const actor = makeActor(actorInterface)(httpAgent);
        assert(Object.entries(actor).length > 0);"
      fi
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
