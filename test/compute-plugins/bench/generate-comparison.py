#!/usr/bin/env python3
"""Add native CandidX and Mops SHA-256 branches to the existing benchmark inputs."""

import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys


HERE = Path(__file__).resolve().parent
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--out", type=Path, required=True)
parser.add_argument("--suite", choices=["main", "static-arrays", "repeated-variants"],
                    default="main")
parser.add_argument("--provenance", type=Path,
                    help="JSON object describing the exact compiler, plugins and libraries")
args = parser.parse_args()

generator = "generate-static-arrays.py" if args.suite == "static-arrays" else "generate.py"
command = [sys.executable, str(HERE / generator), "--out", str(args.out)]
if args.suite == "repeated-variants":
    command.append("--repeated-variants")
subprocess.run(command, check=True)

source_path = args.out / "main.mo"
source = source_path.read_text()
manifest = json.loads((args.out / "cases.json").read_text())
has_sha = any(case["family"] == "sha256" for case in manifest["cases"])
if has_sha:
    # Avoid a repeating-byte hash input. Setup runs outside every measurement.
    source, replaced = re.subn(
        r"  func makeBlob\(size : Nat\) : Blob \{.*?\n  \};", """  func makeBlob(size : Nat) : Blob {
    var state : Nat32 = 0x9e37_79b9;
    Prim.arrayToBlob(Prim.Array_tabulate<Nat8>(size, func(_) {
      state := state ^ (state << 13);
      state := state ^ (state >> 17);
      state := state ^ (state << 5);
      Prim.natToNat8(Prim.nat32ToNat(state & 255))
    }))
  };""", source, count=1, flags=re.DOTALL)
    assert replaced == 1, "Expected the main suite's Blob input generator"
    for case in manifest["cases"]:
        if case["family"] != "sha256":
            continue
        state = 0x9E3779B9
        payload = bytearray()
        for _ in range(int(case["name"].removeprefix("sha256-"))):
            state = (state ^ (state << 13)) & 0xFFFFFFFF
            state ^= state >> 17
            state = (state ^ (state << 5)) & 0xFFFFFFFF
            payload.append(state & 255)
        case["expected_sha256_hex"] = hashlib.sha256(payload).hexdigest()
    manifest["sha256_inputs"] = {"generator": "xorshift32, low byte after each step",
                                  "seed": "0x9e3779b9", "shifts": [13, -17, 5]}
imports = 'import CandidX "mo:candidx";\n'
if has_sha:
    imports += 'import SHA256 "mo:sha2/Sha256";\n'
source = source.replace('import Prim "mo:⛔";\n', 'import Prim "mo:⛔";\n' + imports, 1)
source = source.replace("actor {\n", """actor {
  transient var ordinaryInput : Blob = "";
  func nativeCanonical(input : Blob) : Blob {
    switch (CandidX.canonicalize(input)) {
      case (#ok bytes) bytes;
      case (#err message) Prim.trap(message)
    }
  };
""", 1)


def method(name):
    pattern = rf"  public func {re.escape(name)}\(\) : async Measurement \{{\n.*?\n  \}};\n"
    matches = list(re.finditer(pattern, source, re.DOTALL))
    assert len(matches) == 1, f"Expected one generated method {name}"
    return matches[0].group()


def expression(body):
    match = re.search(r"    output := (.*?);\n    let ended =", body, re.DOTALL)
    assert match, "Generated measurement expression not found"
    return match.group(1)


def branch(original, name, value):
    body = method(original)
    body = body.replace(f"func {original}()", f"func {name}()", 1)
    before = expression(body)
    return body.replace(f"output := {before};", f"output := {value};", 1)


additional = []
for case in manifest["cases"]:
    index = case["index"]
    plugin = {"label": "plugin", "method": case["method"], "require_no_gc": True}
    if case["family"] == "control":
        plugin["kind"] = "control"
        case["phases"] = [plugin]
    elif case["family"] == "sha256":
        plugin["kind"] = "sha256"
        native = f"native_{index}"
        additional.append(branch(case["method"], native, "SHA256.fromBlob(#sha256, bytes)"))
        case["phases"] = [
            {"label": "native", "method": native, "kind": "sha256"}, plugin,
        ]
    else:
        plugin["kind"] = "canonical_candid"
        ordinary = case["ordinary_method"]
        ordinary_body = method(ordinary)
        ordinary_expression = expression(ordinary_body)
        # Save the ordinary bytes after all measurement counters were sampled.
        # Only the normalization-only branch reuses this prepared Blob.
        marker = "    { instructions = ended - started;"
        assert ordinary_body.count(marker) == 1
        source = source.replace(ordinary_body, ordinary_body.replace(
            marker, "    ordinaryInput := output;\n" + marker, 1), 1)
        native, normalize = f"native_{index}", f"normalize_{index}"
        additional.extend([
            branch(case["method"], native, f"nativeCanonical({ordinary_expression})"),
            branch(case["method"], normalize, "nativeCanonical(ordinaryInput)"),
        ])
        case["phases"] = [
            {"label": "ordinary", "method": ordinary, "kind": "ordinary_candid"},
            {"label": "native", "method": native, "kind": "canonical_candid"},
            {"label": "normalize", "method": normalize, "kind": "canonical_candid"},
            plugin,
        ]

assert source.endswith("}\n")
source = source[:-2] + "\n".join(additional) + "}\n"
source = source.replace(source.splitlines()[0],
                        "// Generated by test/compute-plugins/bench/generate-comparison.py.", 1)
source_path.write_text(source)
manifest["suite"] = f"native-comparison-{args.suite}"
manifest["implementations"] = json.loads(args.provenance.read_text()) if args.provenance else {}
manifest["methodology"] = (
    "Same compiler and actor for all implementations; fresh canister per workload/trial. "
    "Inputs are configured outside measurement. Output is cleared and four GC increments run "
    "before each phase. Raw performanceCounter(0) deltas include counter/assignment overhead; "
    "control is reported separately. Native CandidX includes to_candid plus canonicalize and "
    "successful Result unwrapping. Normalize is canonicalize alone on ordinary bytes saved "
    "outside measurement. Native SHA is Mops sha2 Sha256.fromBlob(#sha256, input). "
    "Synchronous GC, if any, is included and reported; plugin phases require zero reclamation. "
    "Reply encoding, input setup, and message-end GC are outside the interval."
)
manifest["correctness"] = (
    "Every canonical Candid output is checked against host canonicalization of the ordinary "
    "Candid output for the same input. Both SHA implementations are checked against Python "
    "hashlib digests. Equivalent phases must return identical bytes."
)
(args.out / "cases.json").write_text(json.dumps(manifest, indent=2) + "\n")
