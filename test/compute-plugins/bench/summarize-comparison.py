#!/usr/bin/env python3
"""Validate paired native/plugin reports and write instruction-cost comparisons."""

import argparse
import csv
import json
from pathlib import Path
import statistics


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("reports", nargs="+", type=Path)
parser.add_argument("--out", type=Path, required=True, help="Output CSV path")
parser.add_argument("--expected-cases", type=int)
parser.add_argument("--expected-trials", type=int)
args = parser.parse_args()
rows = []
versions = set()
identities = set()


def natural(value, context):
    # The runner writes large BigInt counters as decimal strings.
    assert type(value) is int or (isinstance(value, str) and value.isascii()
                                 and value.isdecimal()), f"{context}: invalid counter {value!r}"
    value = int(value)
    assert value >= 0, f"{context}: negative counter"
    return value


for path in args.reports:
    report = json.loads(path.read_text())
    assert not report.get("fatal_error"), f"{path}: {report.get('fatal_error')}"
    assert report.get("failed_measurements", 0) == 0, f"{path}: failed benchmark phases"
    assert report["cases"], f"{path}: no workloads"
    versions.add(report["pocket_ic_version"])
    trials = report["trials"]
    assert type(trials) is int and trials > 0, f"{path}: invalid trial count"
    if args.expected_trials is not None:
        assert trials == args.expected_trials, \
            f"{path}: expected {args.expected_trials} trials, got {trials}"
    for case in report["cases"]:
        identity = report["label"], case["name"]
        assert identity not in identities, f"Duplicate workload {identity}"
        identities.add(identity)
        assert case["family"] in {"control", "sha256", "candidx"}, \
            f"{identity}: unknown workload family"
        expected = ({"plugin"} if case["family"] == "control" else
                    {"native", "plugin"} if case["family"] == "sha256" else
                    {"ordinary", "native", "normalize", "plugin"})
        declarations = case.get("phases")
        if declarations is not None:
            assert len(declarations) == len(expected) and \
                {phase["label"] for phase in declarations} == expected, \
                f"{identity}: unexpected phase declarations"
        require_no_gc = {"plugin"} | {
            phase["label"] for phase in declarations or [] if phase.get("require_no_gc")
        }
        phases = {phase: [] for phase in expected}
        observed = set()
        for sample in case["measurements"]:
            assert "error" not in sample, f"{identity}: {sample}"
            assert type(sample["trial"]) is int, f"{identity}: invalid trial index"
            key = sample["trial"], sample["phase"]
            assert key not in observed, f"{identity}: duplicate sample {key}"
            observed.add(key)
            assert sample["phase"] in expected, f"{identity}: unexpected phase {key}"
            for field in ["instructions", "allocated_bytes", "reclaimed_bytes", "output_bytes"]:
                sample[field] = natural(sample[field], f"{identity}/{key}/{field}")
            assert sample["instructions"] > 0, f"{identity}/{key}: zero instructions"
            digest = sample["output_sha256"]
            assert isinstance(digest, str) and len(digest) == 64 and \
                all(char in "0123456789abcdef" for char in digest), \
                f"{identity}/{key}: invalid output digest"
            if sample["phase"] in require_no_gc:
                assert sample["reclaimed_bytes"] == 0, \
                    f"{identity}/{key}: unexpected synchronous reclamation"
            phases[sample["phase"]].append(sample)
        assert observed == {(trial, phase) for trial in range(trials) for phase in expected}, \
            f"{identity}: incomplete trial set"
        for phase, samples in phases.items():
            assert len({(sample["output_sha256"], sample["output_bytes"])
                        for sample in samples}) == 1, \
                f"{identity}/{phase}: outputs differ between trials"
        equivalent = [sample for phase, samples in phases.items() if phase != "ordinary"
                      for sample in samples]
        assert len({sample["output_sha256"] for sample in equivalent}) == 1, \
            f"{identity}: implementation outputs differ"
        assert len({sample["output_bytes"] for sample in equivalent}) == 1, \
            f"{identity}: implementation output sizes differ"
        row = {"suite": report["label"], "workload": case["name"],
               "family": case["family"], "trials": trials,
               "output_bytes": equivalent[0]["output_bytes"]}
        for phase in ["native", "plugin", "normalize", "ordinary"]:
            samples = phases.get(phase, [])
            for metric, field in [("instructions", "instructions"),
                                  ("allocated_bytes", "allocated_bytes"),
                                  ("reclaimed_bytes", "reclaimed_bytes")]:
                values = [int(sample[field]) for sample in samples]
                row[f"{phase}_{metric}"] = statistics.median(values) if values else ""
                row[f"{phase}_{metric}_min"] = min(values) if values else ""
                row[f"{phase}_{metric}_max"] = max(values) if values else ""
            row[f"{phase}_gc_trials"] = sum(sample["reclaimed_bytes"] > 0 for sample in samples) \
                if samples else ""
        baseline, plugin = row["native_instructions"], row["plugin_instructions"]
        row["native_over_plugin"] = baseline / plugin if baseline != "" else ""
        row["instruction_reduction_percent"] = 100 * (baseline - plugin) / baseline \
            if baseline != "" else ""
        rows.append(row)

assert len(versions) == 1, "PocketIC versions differ"
if args.expected_cases is not None:
    assert len(rows) == args.expected_cases, f"Expected {args.expected_cases} cases, got {len(rows)}"
args.out.parent.mkdir(parents=True, exist_ok=True)
with args.out.open("w", newline="") as output:
    writer = csv.DictWriter(output, fieldnames=list(rows[0]))
    writer.writeheader()
    writer.writerows(rows)

print("| Workload | Native instructions (median) | Plugin instructions (median) | Native/plugin |")
print("|---|---:|---:|---:|")
for row in rows:
    baseline = row["native_instructions"]
    ratio = row["native_over_plugin"]
    print(f"| {row['workload']} | {baseline:,}" if baseline != "" else
          f"| {row['workload']} | —", end="")
    print(f" | {row['plugin_instructions']:,} | {ratio:.2f}× |" if ratio != "" else
          f" | {row['plugin_instructions']:,} | — |")
