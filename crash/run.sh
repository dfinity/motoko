ulimit -c unlimited
ulimit -c
echo "kernel core dump pattern"
cat /proc/sys/kernel/core_pattern
cargo run
ls -la
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
    ls -la /build/dumps
fi
echo "Core dump output location"
echo $out/dumps
exit 1