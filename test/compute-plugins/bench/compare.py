#!/usr/bin/env python3
"""Check output equivalence and summarize raw PocketIC measurement medians."""

import json
import statistics
import sys


def load(path):
    report = json.load(open(path))
    cases = {case["name"]: case for case in report["cases"]}
    return report, cases


baseline, before = load(sys.argv[1])
candidate, after = load(sys.argv[2])
assert not baseline.get("fatal_error"), f"Baseline failed: {baseline.get('fatal_error')}"
assert not candidate.get("fatal_error"), f"Candidate failed: {candidate.get('fatal_error')}"
assert before and after, "Reports contain no workloads"
assert before.keys() == after.keys(), "Workload sets differ; use the same filter for both runs"
assert baseline["pocket_ic_version"] == candidate["pocket_ic_version"], "PocketIC versions differ"
assert baseline["variant_width"] == candidate["variant_width"], "Variant widths differ"
assert baseline.get("variant_payload", "nat") == candidate.get("variant_payload", "nat"), "Variant payloads differ"
if "source_sha256" in baseline and "source_sha256" in candidate:
    assert baseline["source_sha256"] == candidate["source_sha256"], "Actor source differs"
print("| Workload | Baseline instructions | Candidate instructions | Reduction | Baseline allocated | Candidate allocated |")
print("|---|---:|---:|---:|---:|---:|")
for name, case in after.items():
    a = [item for item in before[name]["measurements"] if item["phase"] == "plugin"]
    b = [item for item in case["measurements"] if item["phase"] == "plugin"]
    if any("error" in sample for sample in a + b):
        print(f"| {name} | {'trap' if any('error' in item for item in a) else 'success'} | {'trap' if any('error' in item for item in b) else 'success'} | — | — | — |")
        continue
    assert len(a) == baseline["trials"] and len(b) == candidate["trials"], f"{name}: missing trials"
    assert len({item["output_sha256"] for item in a + b}) == 1, f"{name}: output differs"
    assert len({item["output_bytes"] for item in a + b}) == 1, f"{name}: output size differs"
    x = statistics.median(item["instructions"] for item in a)
    y = statistics.median(item["instructions"] for item in b)
    alloc_a = statistics.median(item["allocated_bytes"] for item in a)
    alloc_b = statistics.median(item["allocated_bytes"] for item in b)
    print(f"| {name} | {x:,} | {y:,} | {100 * (x - y) / x:.1f}% | {alloc_a:,} | {alloc_b:,} |")
