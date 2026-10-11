# PocketIC results — 2026-10-11

These measurements compare the frozen pre-optimization plugins with the
optimized compiler and runtimes, using the same generated Motoko actor and
PocketIC 14.0.0 with production instruction limits. Each standard workload
runs three times in fresh canisters. All three trials produced identical
instruction/allocation counts and output bytes for each phase of all 20 workloads.
See the [benchmark instructions](../README.md) for the input definitions and
measurement protocol.

The table reports raw `performanceCounter(0)` instruction counts and cumulative
heap allocation in bytes. It includes the counter and output-assignment cost;
the control row measures that cost separately. Input construction, reply
serialization, correctness checks, and end-of-message GC are excluded.
These are instruction counts, not wall-clock measurements.

| Workload | Baseline instructions | Candidate instructions | Reduction | Baseline allocated | Candidate allocated |
|---|---:|---:|---:|---:|---:|
| control | 256 | 256 | 0.0% | 0 | 0 |
| sha256-0 | 5,787 | 451 | 92.2% | 56 | 56 |
| sha256-32 | 5,926 | 5,787 | 2.3% | 56 | 56 |
| sha256-55 | 5,949 | 5,810 | 2.3% | 56 | 56 |
| sha256-56 | 10,824 | 10,585 | 2.2% | 56 | 56 |
| sha256-64 | 10,677 | 10,422 | 2.4% | 56 | 56 |
| sha256-1024 | 83,937 | 82,002 | 2.3% | 56 | 56 |
| sha256-65536 | 5,007,041 | 4,892,210 | 2.3% | 56 | 56 |
| sha256-1048576 | 80,025,521 | 78,190,370 | 2.3% | 56 | 56 |
| candid-empty | 1,317 | 412 | 68.7% | 272 | 32 |
| candid-nat | 2,374 | 575 | 75.8% | 304 | 32 |
| candid-static-record | 14,771 | 1,611 | 89.1% | 1,120 | 104 |
| candid-blob-64k | 70,982 | 66,322 | 6.6% | 66,080 | 65,576 |
| candid-nat64-4096 | 214,247 | 193,147 | 9.8% | 33,312 | 32,808 |
| candid-sparse-128 | 1,114,979 | 797,964 | 28.4% | 6,024 | 7,128 |
| candid-sparse-1024 | 8,770,907 | 6,142,640 | 30.0% | 21,264 | 22,520 |
| candid-nested-option-arrays | 502,175 | 410,075 | 18.3% | 2,464 | 2,504 |
| candid-variants-64 | 1,097,006 | 405,059 | 63.1% | 60,272 | 33,464 |
| candid-variants-256 | 13,457,507 | 1,735,032 | 87.1% | 632,064 | 138,976 |
| candid-variants-1024 | 198,731,745 | 7,549,601 | 96.2% | 8,817,552 | 585,072 |

SHA-256 uses deterministic bytes `(index * 17 + 11) % 256`; every result is
checked against Python `hashlib`. Canonical Candid outputs are checked against
host canonicalization of ordinary Candid. The before/after comparison also
requires identical output sizes and hashes. Ordinary `to_candid` measurements
are retained in the raw reports for context; its different semantics make it
an inappropriate substitute for the canonical encoder in a speedup ratio.

The small record-join cache trades bounded scratch allocation for fewer
repeated joins. Consequently, some dynamic record workloads allocate slightly
more after optimization, while static values and large variant unions allocate
less. The cache itself adds 800 bytes to calls that join records and 8 bytes to
other dynamic-shape calls compared with the same optimized build without the
cache; static fast paths bypass it. The complete before/after changes are shown
in the table. Allocation totals are not retained live memory or process RSS.

The [baseline report](baseline.json) and [optimized report](after.json) contain
every trial and the Wasm/source hashes. The [baseline manifest](baseline-artifacts.json)
and [optimized manifest](after-artifacts.json) identify the compiler, RTS, and
plugin packages used for each build. The baseline was an uncommitted local
snapshot frozen under `_build/plugin-perf/baseline`, not a historical Git commit.
Rebuilding the current source reproduces the optimized implementation; repeating
the old build requires that frozen snapshot.

The additional 6,000-case Null-payload variant stress completed in **one trial**:
55,871,792 instructions, 4,770,888 allocated bytes, and a
47,886-byte result. Its output matched the host canonicalizer. The
[stress report](stress-6000.json) records this separate workload; there is no
same-input baseline runtime comparison, so no before/after ratio is claimed.
The large input constructor is split into 256-case helper functions solely to
stay within IC per-function limits during setup. Construction and host
reference encoding remain outside the measured interval.

The final implementation also passed the full native and JavaScript release
build, 17 plugin integration/diagnostic/GC fixtures, 19 native runtime tests,
packaging checks, and 600 seeded compiled-Wasm output comparisons against the
reference interpreter.

The reproducible comparison command, run from the repository root, is:

```sh
python3 test/compute-plugins/bench/compare.py \
  test/compute-plugins/bench/results/baseline.json \
  test/compute-plugins/bench/results/after.json
```
