ulimit -c 1000000
cargo build
pwd
target/debug/crash
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
    ls -la /build/dumps
fi
