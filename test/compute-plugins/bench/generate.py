#!/usr/bin/env python3
"""Generate the dependency-free Motoko actor and workload manifest."""

import argparse
import hashlib
import json
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--out", type=Path, required=True)
parser.add_argument("--variant-width", type=int, default=1024)
parser.add_argument("--variant-payload", choices=["nat", "null"], default="nat")
parser.add_argument("--host-variant-reference", action="store_true",
                    help="Encode reference variants on the host to avoid a second enormous actor helper")
parser.add_argument("--repeated-variants", action="store_true",
                    help="Use a separate variant workload set with repeated tags")
args = parser.parse_args()
assert args.variant_width >= 1024
assert not (args.repeated_variants and args.host_variant_reference)
args.out.mkdir(parents=True, exist_ok=True)

cases = []
methods = []
configure = []


def add(name, expression, setup="", family="candidx", ordinary=None, expected=None):
    index = len(cases)
    entry = {"name": name, "index": index, "family": family,
             "method": f"bench_{index}"}
    if expected is not None:
        entry["expected_sha256_hex"] = expected
    configure.append(f"case {index} {{ {setup} }};")
    expressions = [(entry["method"], expression)]
    if ordinary is not None:
        entry["ordinary_method"] = f"ordinary_{index}"
        expressions.append((entry["ordinary_method"], ordinary))
    for method, expr in expressions:
        methods.append(f"""
  public func {method}() : async Measurement {{
    let allocatedBefore = Prim.rts_total_allocation();
    let reclaimedBefore = Prim.rts_reclaimed();
    let started = Prim.performanceCounter(0);
    output := {expr};
    let ended = Prim.performanceCounter(0);
    let allocatedAfter = Prim.rts_total_allocation();
    let reclaimedAfter = Prim.rts_reclaimed();
    {{ instructions = ended - started;
       allocated = allocatedAfter - allocatedBefore;
       reclaimed = reclaimedAfter - reclaimedBefore;
       outputSize = output.size() }}
  }};
""")
    cases.append(entry)


add("control", '""', family="control")
for size in [0, 32, 55, 56, 64, 1024, 65536, 1048576]:
    payload = bytes((i * 17 + 11) % 256 for i in range(size))
    add(f"sha256-{size}", "Prim.sha256(bytes)", f"bytes := makeBlob({size});",
        family="sha256", expected=hashlib.sha256(payload).hexdigest())

add("candid-empty", "Prim.to_candid_canonical()", ordinary="to_candid()")
add("candid-nat", "Prim.to_candid_canonical(scalar)", "scalar := 42;", ordinary="to_candid(scalar)")
add("candid-static-record", "Prim.to_candid_canonical(staticRecord)",
    ordinary="to_candid(staticRecord)")
add("candid-blob-64k", "Prim.to_candid_canonical(bytes)", "bytes := makeBlob(65536);",
    ordinary="to_candid(bytes)")
add("candid-nat64-4096", "Prim.to_candid_canonical(numbers)",
    "numbers := Prim.Array_tabulate<Nat64>(4096, func(i) { Prim.natToNat64(i * 17) });",
    ordinary="to_candid(numbers)")
for count in [128, 1024]:
    add(f"candid-sparse-{count}", "Prim.to_candid_canonical(records)", f"""
      let memo = makeBlob(32);
      records := Prim.Array_tabulate<Sparse>({count}, func(i) {{
        {{ id = i; tag = if (i % 4 == 0) ?"tag" else null;
           memo = if (i % 3 == 0) ?memo else null; absent = null;
           nested = {{ flag = i % 2 == 0; unused = null }} }}
      }});
    """, ordinary="to_candid(records)")
add("candid-nested-option-arrays", "Prim.to_candid_canonical(nested)", """
      nested := Prim.Array_tabulate<?[?Nat]>(64, func(i) {
        if (i % 3 == 0) null else ?Prim.Array_tabulate<?Nat>(16, func(j) {
          if (j % 2 == 0) null else ?(i * 16 + j)
        })
      });
    """, ordinary="to_candid(nested)")
variant_workloads = [(count, None) for count in [64, 256, 1024] +
                    ([args.variant_width] if args.variant_width > 1024 else [])]
if args.repeated_variants:
    variant_workloads = [(0, 1), (1, 1), (1024, 1), (4096, 4), (6000, 64)]
for count, cycle in variant_workloads:
    name = (f"candid-variants-{count}" if cycle is None else
            f"candid-repeated-variants-{count}-tags-{cycle}")
    build_choice = "choice" if cycle is None else f"func(i) {{ choice(i % {cycle}) }}"
    add(name, "Prim.to_candid_canonical(variants)",
        f"variants := Prim.Array_tabulate<Choice>({count}, {build_choice});",
        ordinary=None if args.host_variant_reference else "to_candid(variants)")
    if args.host_variant_reference:
        cases[-1]["reference_variant_count"] = count

payload_type = "Nat" if args.variant_payload == "nat" else "Null"
payload_value = "i" if args.variant_payload == "nat" else "null"
tags = "; ".join(f"#t{i:04d} : {payload_type}" for i in range(args.variant_width))
choices = "\n".join(f"      case {i} #t{i:04d}({payload_value});" for i in range(args.variant_width))
choice_definition = f"""  func choice(i : Nat) : Choice {{
    switch i {{
{choices}
      case _ Prim.trap("variant index out of range")
    }}
  }};"""
if args.variant_width > 1024:
    # Setup is outside measurement. Split its literal case constructor so it
    # does not itself exceed IC per-function complexity limits in stress runs.
    builders = []
    dispatch = []
    for bucket, first in enumerate(range(0, args.variant_width, 256)):
        cases_in_bucket = "\n".join(
            f"      case {i} #t{i:04d}({payload_value});"
            for i in range(first, min(first + 256, args.variant_width)))
        builders.append(f"""  func choice_{bucket}(i : Nat) : Choice {{
    switch i {{
{cases_in_bucket}
      case _ Prim.trap("variant index out of range")
    }}
  }};""")
        dispatch.append(f"      case {bucket} choice_{bucket}(i);")
    choice_definition = "\n".join(builders) + f"""
  func choice(i : Nat) : Choice {{
    switch (i / 256) {{
{chr(10).join(dispatch)}
      case _ Prim.trap("variant index out of range")
    }}
  }};"""
source = f"""// Generated by test/compute-plugins/bench/generate.py.
import Prim "mo:⛔";
actor {{
  type Measurement = {{ instructions : Nat64; allocated : Nat; reclaimed : Nat; outputSize : Nat }};
  type Sparse = {{ id : Nat; tag : ?Text; memo : ?Blob; absent : ?Nat;
    nested : {{ flag : Bool; unused : ?Nat }} }};
  type Choice = {{ {tags} }};
  transient var bytes : Blob = "";
  transient var scalar : Nat = 42;
  transient let staticRecord = {{ number = 42 : Nat; signed = -17 : Int32;
    wide = 123456789 : Nat64; flag = true; text = "hello 🌐"; real = 1.25 : Float }};
  transient var numbers : [Nat64] = [];
  transient var records : [Sparse] = [];
  transient var nested : [?[?Nat]] = [];
  transient var variants : [Choice] = [];
  transient var output : Blob = "";
  func makeBlob(size : Nat) : Blob {{
    Prim.arrayToBlob(Prim.Array_tabulate<Nat8>(size, func(i) {{
      Prim.natToNat8((i * 17 + 11) % 256)
    }}))
  }};
{choice_definition}
  public func configure(index : Nat) : async () {{
    bytes := ""; numbers := []; records := []; nested := []; variants := []; output := "";
    switch index {{
      {chr(10).join(configure)}
      case _ Prim.trap("unknown benchmark")
    }}
  }};
  public func clear() : async () {{ output := "" }};
  public query func bytesResult() : async Blob {{ output }};
  {chr(10).join(methods)}
}}
"""
(args.out / "main.mo").write_text(source)
(args.out / "cases.json").write_text(json.dumps({"variant_width": args.variant_width,
    "variant_payload": args.variant_payload, "cases": cases}, indent=2) + "\n")
print(args.out / "main.mo")
