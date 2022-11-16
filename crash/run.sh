ulimit -c unlimited
cargo run
echo "search in local directory"
pwd
find -name core.*
echo "search in /var/lib/systemd/coredump"
ls -la /var/lib/systemd/coredump
echo "search in /var/crash"
ls -la /var/lib/systemd/coredump
echo "search in /var/lib/apport/coredump"
ls -la /var/lib/apport/coredump
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
    ls -la /build/dumps
fi
