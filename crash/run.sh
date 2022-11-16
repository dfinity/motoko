ulimit -c unlimited
cargo run
find -name core.*
gzip core.*
mkdir -p /dumps
mv core.* /dumps
