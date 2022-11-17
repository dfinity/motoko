ulimit -c unlimited
ulimit -c
echo "kernel core dump pattern"
cat /proc/sys/kernel/core_pattern
cargo run
ls -la
echo "Core dump copied to /build/dumps"
mkdir -p /build/dumps
cp core.* /build/dumps
ls -la /build/dumps
echo "Core dump output location"
echo $out/dumps
exit 1