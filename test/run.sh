#!/usr/bin/env bash

# A simple test runner. Synopsis:
#
# ./run.sh foo.mo [bar.mo ..]
#
# Options:
#
#    -a: Update the files in ok/
#    -2: Use IC API
#    -3: Use Stub API
#    -t: Only typecheck
#    -s: Be silent in sunny-day execution
#    -i: Only check mo to idl generation
#    -r: Activate release mode (eliminate `debug` blocks)
#

function realpath() {
    [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}


ACCEPT=no
API=wasi
IDL=no
RELEASE=no
EXTRA_MOC_FLAGS=
MOC=${MOC:-$(realpath $(dirname $0)/../src/moc)}
MO_LD=${MO_LD:-$(realpath $(dirname $0)/../src/mo-ld)}
DIDC=${DIDC:-$(realpath $(dirname $0)/../src/didc)}
export MO_LD
WASMTIME=${WASMTIME:-wasmtime}
DRUN_WRAPPER=$(realpath $(dirname $0)/drun-wrapper.sh)
IC_STUB_RUN=${IC_STUB_RUN:-ic-stub-run}
SKIP_RUNNING=${SKIP_RUNNING:-no}
ONLY_TYPECHECK=no
ECHO=echo

while getopts "a23stir" o; do
    case "${o}" in
        a)
            ACCEPT=yes
            ;;
        2)
            API=ic
            ;;
        3)
            API=stub
            ;;
        s)
            ECHO=true
            ;;
        t)
            ONLY_TYPECHECK=true
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
if [ $API = "wasi" ]; then EXTRA_MOC_FLAGS=-wasi-system-api; fi
if [ $API = "stub" ]; then EXTRA_MOC_FLAGS=-stub-system-api; fi
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
    sed 's/trap at 0x[a-f0-9]*/trap at 0x___:/g' |
    sed 's/source location: @[a-f0-9]*/source location: @___:/g' |
    cat > $1.norm
    mv $1.norm $1
  fi
}

function run () {
  # first argument: extension of the output file
  # remaining argument: command line
  # uses from scope: $out, $file, $base, $diff_files

  local ext="$1"
  shift

  if grep -q "^//SKIP $ext" $file; then return; fi

  $ECHO -n " [$ext]"
  "$@" >& $out/$base.$ext
  local ret=$?

  if [ $ret != 0 ]
  then echo "Return code $ret" >> $out/$base.$ext.ret
  else rm -f $out/$base.$ext.ret
  fi
  diff_files="$diff_files $base.$ext.ret"

  normalize $out/$base.$ext
  diff_files="$diff_files $base.$ext"

  return $ret
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

  rm -f $out/$base.*

  # First run all the steps, and remember what to diff
  diff_files=

  if [ ${file: -3} == ".mo" ]
  then
    # Typecheck
    run tc $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS --check $base.mo
    tc_succeeded=$?

    if [ "$tc_succeeded" -eq 0 -a "$ONLY_TYPECHECK" = "no" ]
    then
      if [ $IDL = 'yes' ]
      then
        run idl $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS --idl $base.mo -o $out/$base.did
        idl_succeeded=$?

        normalize $out/$base.did
        diff_files="$diff_files $base.did"

        if [ "$idl_succeeded" -eq 0 ]
        then
          run didc $DIDC --check $out/$base.did
        fi
      else
        if [ "$SKIP_RUNNING" != yes ]
        then
          # Interpret
          run run $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS --hide-warnings -r $base.mo

          # Interpret IR without lowering
          run run-ir $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS --hide-warnings -r -iR -no-async -no-await $base.mo

          # Diff interpretations without/with lowering
          if [ -e $out/$base.run -a -e $out/$base.run-ir ]
          then
            diff -u -N --label "$base.run" $out/$base.run --label "$base.run-ir" $out/$base.run-ir > $out/$base.diff-ir
            diff_files="$diff_files $base.diff-ir"
          fi

          # Interpret IR with lowering
          run run-low $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS --hide-warnings -r -iR $base.mo

          # Diff interpretations without/with lowering
          if [ -e $out/$base.run -a -e $out/$base.run-low ]
          then
            diff -u -N --label "$base.run" $out/$base.run --label "$base.run-low" $out/$base.run-low > $out/$base.diff-low
            diff_files="$diff_files $base.diff-low"
          fi

        fi

        # Compile
        run comp $MOC $MOC_FLAGS $EXTRA_MOC_FLAGS --hide-warnings --map -c $base.mo -o $out/$base.wasm

        if [ -e $out/$base.wasm ]
        then
          # Validate wasm
          run valid wasm-validate $out/$base.wasm

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
          if [ "$SKIP_RUNNING" != yes ]
          then
            if [ $API = ic ]
            then
              run drun-run $DRUN_WRAPPER $out/$base.wasm $base.mo
            elif [ $API = stub ]
	    then
              DRUN=$IC_STUB_RUN run ic-stub-run $DRUN_WRAPPER $out/$base.wasm $base.mo
            elif [ $API = wasi ]
            then
              run wasm-run $WASMTIME --disable-cache $out/$base.wasm
            else
              echo "Unkonwn API $API"
	      exit 1
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
        run wasm2wat wasm2wat $out/$base.linked.wasm -o $out/$base.linked.wat
        diff_files="$diff_files $base.linked.wat"
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
        const httpAgent = makeHttpAgent({ canisterId: "ffffffffffffffff" });
        const actor = makeActor(actorInterface)(httpAgent);
        assert(Object.entries(actor).length > 0);"
      fi
    fi
  fi
  $ECHO ""

  if [ $ACCEPT = yes ]
  then
    rm -f $ok/$base.*

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
