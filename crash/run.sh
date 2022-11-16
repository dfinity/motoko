ulimit -c unlimited
cargo build
target/debug/crash
ls -la core.*
echo "List all files"
find /*
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
    ls -la /build/dumps
fi
