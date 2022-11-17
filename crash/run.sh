ulimit -c unlimited
ulimit -c
echo "kernel core dump pattern"
cat /proc/sys/kernel/core_pattern
cargo run
ls -la
mkdir -p $out/dumps
cp core.* $out/dumps
