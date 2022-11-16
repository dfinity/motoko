ulimit -c unlimited
cargo run
find -name core.*
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
fi
ls -la /build/dumps
