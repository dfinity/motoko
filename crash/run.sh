ulimit -c unlimited
ulimit -c
echo "kernel core dump pattern"
cat /proc/sys/kernel/core_pattern
cargo run
ls -la
mkdir -p $out
if [ -e core.* ]
then 
    mkdir -p $out/dumps
    cp core.* $out/dumps
fi
touch $out/fail
