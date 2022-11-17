ulimit -c unlimited
ulimit -c
echo "kernel core dump pattern"
cat /proc/sys/kernel/core_pattern
cargo run
ls -la
echo "Core dump copied to /nix/dumps"
cp core.* /nix/dumps
echo "Core dump output location"
echo $out/dumps
exit 1