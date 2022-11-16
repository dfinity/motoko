ulimit -c unlimited
cargo build
pwd
target/debug/crash
find -name core*
find / -name core*
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
    ls -la /build/dumps
fi
