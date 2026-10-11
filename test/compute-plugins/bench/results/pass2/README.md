# Second PocketIC optimization pass — 2026-10-11

This pass compares against the **previous optimized implementation**, whose
results remain in the [first-pass report](../README.md). The reductions below
are additional improvements, not the cumulative reduction from the original
unoptimized plugins. Each comparison uses identical generated Motoko source
and PocketIC 14.0.0 with production instruction limits.

The standard suite has 20 workloads, the separate fixed-shape array suite has
15, and the repeated-tag suite has five. Each runs three times in fresh
canisters. Every trial within a version had identical instruction/allocation
counts and output bytes. All before/after output hashes and sizes match;
SHA-256 is also checked against Python `hashlib`, and canonical Candid against
host canonicalization of ordinary Candid. The separate 6,000-case stress uses
one trial per version.

The tables report raw `performanceCounter(0)` instruction counts and cumulative
heap allocation in bytes. Control remains 256 instructions; no overhead is
subtracted. Input construction, four forced GC increments, reply serialization,
correctness checks, and end-of-message GC are outside the measured interval.
Allocation is neither retained live memory nor process RSS. These are IC
instruction counts, not wall-clock timings. Ordinary `to_candid` measurements
remain in the raw reports for context; its semantics differ from canonical
encoding and it is not used as the speedup baseline.

## Standard suite

| Workload | Baseline instructions | Final instructions | Reduction | Baseline allocated | Final allocated |
|---|---:|---:|---:|---:|---:|
| control | 256 | 256 | 0.0% | 0 | 0 |
| sha256-0 | 451 | 445 | 1.3% | 56 | 56 |
| sha256-32 | 5,787 | 4,966 | 14.2% | 56 | 56 |
| sha256-55 | 5,810 | 5,824 | -0.2% | 56 | 56 |
| sha256-56 | 10,585 | 10,598 | -0.1% | 56 | 56 |
| sha256-64 | 10,422 | 8,322 | 20.1% | 56 | 56 |
| sha256-1024 | 82,002 | 81,952 | 0.1% | 56 | 56 |
| sha256-65536 | 4,892,210 | 4,888,128 | 0.1% | 56 | 56 |
| sha256-1048576 | 78,190,370 | 78,124,848 | 0.1% | 56 | 56 |
| candid-empty | 412 | 412 | 0.0% | 32 | 32 |
| candid-nat | 575 | 575 | 0.0% | 32 | 32 |
| candid-static-record | 1,611 | 1,611 | 0.0% | 104 | 104 |
| candid-blob-64k | 66,322 | 66,322 | 0.0% | 65,576 | 65,576 |
| candid-nat64-4096 | 193,147 | 144,010 | 25.4% | 32,808 | 32,808 |
| candid-sparse-128 | 797,964 | 739,856 | 7.3% | 7,128 | 7,200 |
| candid-sparse-1024 | 6,142,640 | 5,674,150 | 7.6% | 22,520 | 22,592 |
| candid-nested-option-arrays | 410,075 | 327,080 | 20.2% | 2,504 | 3,296 |
| candid-variants-64 | 405,059 | 130,599 | 67.8% | 33,464 | 4,608 |
| candid-variants-256 | 1,735,032 | 523,183 | 69.8% | 138,976 | 15,664 |
| candid-variants-1024 | 7,549,601 | 2,190,885 | 71.0% | 585,072 | 60,256 |

SHA-256 specializes the common 32- and 64-byte inputs while retaining the
existing general path. Dispatch adds 14 instructions for the 55-byte case and
13 for 56 bytes; bulk inputs improve by about 0.08%. Every SHA call still
allocates 56 bytes. The specialized runtime adds 20,506 bytes of Wasm compared
with the previous SHA runtime. The complete standard actor grows from 863,995
to 930,028 bytes with both plugins' changes.

CandidX walks array storage directly under the existing no-GC contract, builds
known type headers at compile time, reuses repeated dynamic shape work, and
collects primitive-payload variant cases before sorting/deduplicating once.
The latter avoids intermediate union nodes. General dynamic payloads retain
the balanced join path. Sparse-record allocation increases by 72 bytes per
call; the nested-option workload adds a bounded 792-byte shape cache.

## Fixed-shape record arrays

This additional suite was generated separately, then compiled against the
same frozen baseline and final packages. It covers fixed-width record fields,
Text, Blob, and `[Nat8]`, including empty outer arrays, singleton values,
empty byte fields, and 1,024-element arrays. Input construction remains outside
the measured call. The original standard actor source is unchanged.

| Workload | Baseline instructions | Final instructions | Reduction | Baseline allocated | Final allocated |
|---|---:|---:|---:|---:|---:|
| control | 256 | 256 | 0.0% | 0 | 0 |
| candid-fixed-records-0 | 5,305 | 571 | 89.2% | 584 | 40 |
| candid-text-records-0 | 5,305 | 593 | 88.8% | 584 | 40 |
| candid-fixed-records-1 | 13,630 | 954 | 93.0% | 1,136 | 80 |
| candid-text-records-1 | 11,443 | 1,002 | 91.2% | 992 | 56 |
| candid-fixed-records-1024 | 2,330,796 | 351,914 | 84.9% | 22,624 | 21,568 |
| candid-text-records-1024 | 1,766,625 | 485,618 | 72.5% | 22,480 | 21,544 |
| candid-blob-records-0 | 5,305 | 617 | 88.4% | 584 | 40 |
| candid-nat8-records-0 | 5,305 | 617 | 88.4% | 584 | 40 |
| candid-blob-records-1-empty | 13,877 | 1,154 | 91.7% | 1,312 | 64 |
| candid-nat8-records-1-empty | 13,831 | 1,061 | 92.3% | 1,312 | 64 |
| candid-blob-records-1-full | 13,979 | 1,214 | 91.3% | 1,344 | 96 |
| candid-nat8-records-1-full | 15,341 | 2,017 | 86.9% | 1,344 | 96 |
| candid-blob-records-1024-mixed | 2,369,965 | 550,624 | 76.8% | 32,344 | 31,096 |
| candid-nat8-records-1024-mixed | 3,283,069 | 1,066,416 | 67.5% | 32,344 | 31,096 |

## Repeated variant tags

These arrays have a static 1,024-case variant type with Nat payloads. Input
construction cycles through the stated number of tags; both the selected tag
and its payload use the cycle index. The empty and singleton rows exercise the
small-array paths. This suite checks the collector beyond distinct-case unions.

| Workload | Baseline instructions | Final instructions | Reduction | Baseline allocated | Final allocated |
|---|---:|---:|---:|---:|---:|
| candid-repeated-variants-0-tags-1 | 5,298 | 5,364 | -1.2% | 584 | 584 |
| candid-repeated-variants-1-tags-1 | 10,338 | 9,199 | 11.0% | 912 | 816 |
| candid-repeated-variants-1024-tags-1 | 1,550,254 | 775,630 | 50.0% | 3,048 | 2,984 |
| candid-repeated-variants-4096-tags-4 | 11,332,137 | 5,187,276 | 54.2% | 11,016 | 140,520 |
| candid-repeated-variants-6000-tags-64 | 25,656,833 | 10,204,490 | 60.2% | 45,488 | 276,744 |

The collector skips adjacent identical cases, keeping the one-tag workload's
allocation small. Cyclic repeats still buffer all input cases before sorting:
the four-tag workload allocates 140,520 bytes instead of 11,016, and the 64-tag
workload 276,744 instead of 45,488. They use 54.2% and 60.2% fewer measured
instructions, respectively. Since end-of-message GC is excluded, this extra
temporary allocation is a performance tradeoff. The empty dynamic variant
array costs 66 extra instructions with unchanged allocation.

## Wide-union stress

The same 6,000-case Null-payload actor succeeds before and after this pass,
with one trial each. Its 47,886-byte result matches the host canonicalizer and
the previous optimized output. Input constructors are split into 256-case
helpers so setup stays within IC function-complexity limits; this setup is
outside measurement.

| Workload | Baseline instructions | Final instructions | Reduction | Baseline allocated | Final allocated |
|---|---:|---:|---:|---:|---:|
| candid-variants-6000 | 55,871,792 | 13,017,858 | 76.7% | 4,770,888 | 407,208 |

This is a same-input incremental comparison with the previous optimized build.
The first pass did not have a same-input original runtime baseline for this
stress workload. Here instruction count falls by 76.7% and allocation by 91.5%.

## Raw reports and reproduction

| Suite | Previous optimized baseline | Final |
|---|---|---|
| Standard | [baseline.json](baseline.json) | [after.json](after.json) |
| Fixed-shape arrays | [static-arrays-baseline.json](static-arrays-baseline.json) | [static-arrays-after.json](static-arrays-after.json) |
| Repeated tags | [repeated-variants-baseline.json](repeated-variants-baseline.json) | [repeated-variants-after.json](repeated-variants-after.json) |
| 6,000-case stress | [stress-baseline.json](stress-baseline.json) | [stress-after.json](stress-after.json) |

The [baseline manifest](baseline-artifacts.json) and
[final manifest](after-artifacts.json) each identify 29 compiler, RTS, package,
and source files. Before this pass, all 28 entries inherited from the previous
manifest matched the live files. The baseline additionally preserves the
pre-pass `src/codegen/compile.ml` from `pass2/root-before`.
Saved artifacts are under `_build/plugin-perf/pass2/baseline` and
`_build/plugin-perf/pass2/final`. These are local snapshots, not historical Git
commits. The previous standard/stress reports are preserved from the first
pass; the new array/repeated suites were compiled and measured with the same
frozen baseline compiler and packages. Every raw report records the generated
source and Wasm hashes, tool versions, and individual measurements.

See the [harness instructions](../../README.md) for generation, compilation,
and PocketIC commands, including the separate array generator and
`--repeated-variants` option. Frozen native compilers use their corresponding
`MOC_RELEASE_RTS` and `MOC_DEBUG_RTS` artifacts. Reproduce each saved comparison
from the repository root, for example:

```sh
python3 test/compute-plugins/bench/compare.py \
  test/compute-plugins/bench/results/pass2/baseline.json \
  test/compute-plugins/bench/results/pass2/after.json
```

The final implementation also passed the full native and JavaScript release
build, 30 native runtime tests, 19 plugin integration fixtures including forced
GC, and packaging checks. Independent comparisons covered 600 generated
Candid cases and 588 SHA-256 inputs against `hashlib`. All 22 paired baseline/final
resource-limit probes agreed, including visit, depth, and output-size limits.
Final compiler, package, RTS, and source hashes match the saved manifest and
the working tree.
