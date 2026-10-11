#!/usr/bin/env python3
"""Use the CandidX repository's generated compiler probes with this plugin.

Pass this executable as both --moc and --runtime-moc to CandidX's tests/fuzz.py
or tests/verify.py, with --skip-wasm. Set CANDIDX_TEST_ROOT to a directory
containing their --out directory (default: this repo's _build/candidx-tests).

The default compiler is bin/moc with _build/compute-plugins/candidx. To test a
frozen build, set CANDIDX_TEST_ARTIFACTS to a directory containing moc,
mo-rts.wasm, mo-rts-debug.wasm, and candidx/{plugin.cmxs,runtime.wasm}.

Only generated probes are supported: their unqualified legacy operator calls
are changed to Prim.to_candid_canonical in adjacent .plugin.mo copies. Values,
assertions, imports, and oracle sources are preserved. This is not a general
Motoko source rewriter. Every translation and its source hashes are logged in
CANDIDX_TEST_ROOT/compiler-invocations.jsonl.
"""

import hashlib
import json
import os
from pathlib import Path
import re
import sys


ROOT = Path(__file__).resolve().parents[2]
CALL = re.compile(r"(?<![A-Za-z0-9_.])to_candid_canonical\s*(?=\()")


def main():
    probe_root = Path(os.environ.get("CANDIDX_TEST_ROOT", ROOT / "_build/candidx-tests")).resolve()
    probe_root.mkdir(parents=True, exist_ok=True)
    arguments = sys.argv[1:]
    rewrites = []
    for i, argument in enumerate(arguments):
        source = Path(argument)
        if source.suffix != ".mo" or not source.is_file():
            continue
        original = source.read_text()
        translated, count = CALL.subn("Prim.to_candid_canonical", original)
        if not count:
            continue
        source = source.resolve()
        if not source.is_relative_to(probe_root):
            raise RuntimeError(f"Refusing to rewrite a non-probe source: {source}")
        if 'import Prim "mo:⛔";' not in original:
            raise RuntimeError(f"Generated direct probe has no Prim binding: {source}")
        destination = source.with_name(source.stem + ".plugin.mo")
        if not destination.resolve().is_relative_to(probe_root):
            raise RuntimeError(f"Translated probe escapes CANDIDX_TEST_ROOT: {destination}")
        destination.write_text(translated)
        arguments[i] = str(destination)
        rewrites.append({
            "source": str(source), "translated": str(destination), "calls": count,
            "source_sha256": hashlib.sha256(original.encode()).hexdigest(),
            "translated_sha256": hashlib.sha256(translated.encode()).hexdigest(),
        })
    with (probe_root / "compiler-invocations.jsonl").open("a") as log:
        log.write(json.dumps({"arguments": arguments, "rewrites": rewrites}) + "\n")

    environment = dict(os.environ)
    frozen = environment.get("CANDIDX_TEST_ARTIFACTS")
    if frozen:
        artifacts = Path(frozen).resolve()
        compiler = artifacts / "moc"
        plugin = artifacts / "candidx"
        environment["MOC_RELEASE_RTS"] = str(artifacts / "mo-rts.wasm")
        environment["MOC_DEBUG_RTS"] = str(artifacts / "mo-rts-debug.wasm")
    else:
        compiler = ROOT / "bin/moc"
        plugin = ROOT / "_build/compute-plugins/candidx"
    os.execve(compiler, [str(compiler), "--plugin", str(plugin), *arguments], environment)


if __name__ == "__main__":
    main()
