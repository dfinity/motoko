ulimit -c unlimited
cargo build
pwd
target/debug/crash
cat /var/log/apport.log
ls -lar /*
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
    ls -la /build/dumps
fi
