ulimit -c 1000000
ulimit -c
cargo run
ls -la 
ls -la target/debug
ls -la /var/lib/systemd/coredump
ls -la /var/crash
ls -la /var/lib/apport/coredump
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
    ls -la /build/dumps
fi
exit 1