ulimit -c unlimited
cargo build
target/debug/crash
find *
find /var/*
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
    ls -la /build/dumps
fi
