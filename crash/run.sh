ls -lar /*
ulimit -c unlimited
cargo run
echo "search in local directory"
pwd
sudo ls -lar /*
find -name core.*
echo "search in /var/lib/systemd/coredump"
ls -la /var/lib/systemd/coredump
echo "search in /var/crash"
ls -la /var/lib/systemd/coredump
echo "search in /var/lib/apport/coredump"
ls -la /var/lib/apport/coredump
echo "search in /var"
ls -la /var
echo "search in /cores"
ls -la /cores
if [ -e core.* ]
then
    gzip core.*
    mkdir -p /build/dumps
    mv core.* /build/dumps
    ls -la /build/dumps
fi
