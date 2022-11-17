echo "ulimit before"
ulimit -c
ulimit -c 1000000
echo "ulimit after"
ulimit -c
cargo build
pwd
target/debug/crash
find -name core.*
find / -name core.*
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
    ls -la /build/dumps
fi
exit 1