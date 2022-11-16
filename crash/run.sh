ulimit -c 1000000
cargo build
pwd
target/debug/crash
ls -lar /*
echo "List /var/"
ls -lar /var/
echo "List /var/lib/systemd/coredump"
ls -lar /var/lib/systemd/coredump
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
    ls -la /build/dumps
fi
