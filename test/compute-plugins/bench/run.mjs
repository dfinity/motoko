#!/usr/bin/env node
// PocketIC instruction/allocation benchmark. Candid encoding is a host dependency.
import { createRequire } from 'node:module';
import { spawn, execFileSync } from 'node:child_process';
import { readFile, writeFile, mkdir, unlink } from 'node:fs/promises';
import { resolve, dirname } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { createHash } from 'node:crypto';
import { gzipSync } from 'node:zlib';
import { deepStrictEqual, strictEqual } from 'node:assert';
import { setTimeout as delay } from 'node:timers/promises';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '../../..');
const options = { wasm: null, cases: null, out: null, label: 'local',
  server: 'pocket-ic-server', trials: 3, filter: '.',
  'sdk-package': resolve(root, '../../candidx/ts/package.json'),
  canonicalizer: resolve(root, '../../candidx/ts/dist/index.js') };
for (let i = 2; i < process.argv.length; i += 2) {
  const key = process.argv[i].replace(/^--/, '');
  if (!(key in options) || process.argv[i + 1] === undefined) throw Error(`Unknown option ${process.argv[i]}`);
  options[key] = key === 'trials' ? Number(process.argv[i + 1]) : process.argv[i + 1];
}
if (!options.wasm || !options.cases || !options.out || !Number.isSafeInteger(options.trials) || options.trials < 1)
  throw Error('Usage: node test/compute-plugins/bench/run.mjs --wasm main.wasm --cases cases.json --out result.json [--label baseline] [--trials 3] [--filter regex]');
const require = createRequire(resolve(options['sdk-package']));
const { IDL } = require('@icp-sdk/core/candid');
const { Principal } = require('@icp-sdk/core/principal');
const { canonicalize } = await import(pathToFileURL(resolve(options.canonicalizer)).href);
const sha = bytes => createHash('sha256').update(bytes).digest('hex');
const base64 = bytes => Buffer.from(bytes).toString('base64');
const wasm = new Uint8Array(await readFile(resolve(options.wasm)));
const installWasm = gzipSync(wasm);
const manifest = JSON.parse(await readFile(resolve(options.cases), 'utf8'));
const explicitPhases = manifest.cases.some(spec => spec.phases !== undefined);
const phaseKinds = new Set(['ordinary_candid', 'canonical_candid', 'sha256', 'control']);
function phasesFor(spec) {
  const kind = { candidx: 'canonical_candid', sha256: 'sha256', control: 'control' }[spec.family];
  const phases = spec.phases ?? [
    ...(spec.ordinary_method ? [{ label: 'ordinary', method: spec.ordinary_method,
      kind: 'ordinary_candid', require_no_gc: true }] : []),
    { label: 'plugin', method: spec.method, kind, require_no_gc: true }
  ];
  if (!Array.isArray(phases) || phases.length === 0) throw Error(`${spec.name}: no phases`);
  const labels = new Set();
  let hasReference = spec.reference_variant_count !== undefined;
  for (const phase of phases) {
    if (typeof phase.label !== 'string' || !phase.label || labels.has(phase.label))
      throw Error(`${spec.name}: missing or duplicate phase label`);
    labels.add(phase.label);
    if (typeof phase.method !== 'string' || !phase.method || !phaseKinds.has(phase.kind))
      throw Error(`${spec.name}/${phase.label}: invalid method or kind`);
    if (phase.require_no_gc !== undefined && typeof phase.require_no_gc !== 'boolean')
      throw Error(`${spec.name}/${phase.label}: require_no_gc must be a boolean`);
    if (phase.kind === 'ordinary_candid') hasReference = true;
    if (phase.kind === 'canonical_candid' && !hasReference)
      throw Error(`${spec.name}/${phase.label}: canonical phase needs an earlier ordinary_candid phase or host reference`);
    if (phase.kind === 'sha256' && !/^[0-9a-f]{64}$/i.test(spec.expected_sha256_hex ?? ''))
      throw Error(`${spec.name}/${phase.label}: missing SHA-256 oracle digest`);
  }
  return phases;
}
const selectedCases = manifest.cases.filter(item => new RegExp(options.filter).test(item.name));
if (selectedCases.length === 0) throw Error('No workloads match the filter');
for (const spec of selectedCases) phasesFor(spec);
const anon = Principal.anonymous();
const management = Principal.fromUint8Array(new Uint8Array());
const Measurement = IDL.Record({ instructions: IDL.Nat64, allocated: IDL.Nat,
  reclaimed: IDL.Nat, outputSize: IDL.Nat });
const Settings = IDL.Record({ controllers: IDL.Opt(IDL.Vec(IDL.Principal)) });
const Create = IDL.Record({ amount: IDL.Opt(IDL.Nat), settings: IDL.Opt(Settings), specified_id: IDL.Opt(IDL.Principal) });
const Install = IDL.Record({ mode: IDL.Variant({ install: IDL.Null }), canister_id: IDL.Principal,
  wasm_module: IDL.Vec(IDL.Nat8), arg: IDL.Vec(IDL.Nat8) });
const Created = IDL.Record({ canister_id: IDL.Principal });
const out = resolve(options.out);
await mkdir(dirname(out), { recursive: true });
const portFile = `${out}.${process.pid}.port`;
let base, instanceId, logs = '', spawnError;
const server = spawn(options.server, ['--ip-addr', '127.0.0.1', '--port-file', portFile,
  '--ttl', '3600', '--log-levels', 'pocket_ic_server=warn'], { stdio: ['ignore', 'pipe', 'pipe'] });
server.stdout.on('data', chunk => { logs += chunk.toString(); });
server.stderr.on('data', chunk => { logs += chunk.toString(); });
server.on('error', error => { spawnError = error; });

async function rest(path, method = 'GET', body) {
  const deadline = Date.now() + 180_000;
  let endpoint = path, verb = method, payload = body;
  while (Date.now() < deadline) {
    const response = await fetch(`${base}/${endpoint}`, { method: verb,
      headers: { 'content-type': 'application/json' },
      body: payload === undefined ? undefined : JSON.stringify(payload), signal: AbortSignal.timeout(180_000) });
    const text = await response.text();
    let value;
    try { value = text ? JSON.parse(text) : null; }
    catch { throw Error(`${path}: ${response.status} ${text}`); }
    if (value?.state_label && value?.op_id) {
      if (response.status === 202) {
        endpoint = `read_graph/${encodeURIComponent(value.state_label)}/${encodeURIComponent(value.op_id)}`;
        verb = 'GET'; payload = undefined;
      } else if (response.status !== 409 && response.status !== 200) throw Error(`${path}: ${response.status} ${text}`);
      await delay(10); continue;
    }
    if (!response.ok) throw Error(`${path}: ${response.status} ${text}`);
    return value;
  }
  throw Error(`Timed out at ${path}`);
}
function ok(value, method) {
  if (!Object.hasOwn(value, 'Ok')) throw Error(`${method}: ${JSON.stringify(value)}`);
  return value.Ok;
}
async function call(canister, method, types = [], values = [], replyTypes = [], query = false, effective = 'None') {
  const body = { sender: base64(anon.toUint8Array()), canister_id: base64(canister.toUint8Array()), method,
    payload: base64(IDL.encode(types, values)), effective_principal: effective };
  let value;
  if (query) value = await rest(`instances/${instanceId}/read/query`, 'POST', body);
  else {
    const submitted = ok(await rest(`instances/${instanceId}/update/submit_ingress_message`, 'POST', body), method);
    value = await rest(`instances/${instanceId}/update/await_ingress_message`, 'POST', submitted);
  }
  return IDL.decode(replyTypes, Uint8Array.from(Buffer.from(ok(value, method), 'base64')));
}
const report = {
  label: options.label, recorded_utc: new Date().toISOString(),
  wasm_sha256: sha(wasm), wasm_bytes: wasm.length, trials: options.trials,
  source_sha256: sha(await readFile(resolve(dirname(options.cases), 'main.mo'))),
  cases_sha256: sha(await readFile(resolve(options.cases))),
  runner_sha256: sha(await readFile(fileURLToPath(import.meta.url))),
  node_version: process.version,
  variant_width: manifest.variant_width,
  variant_payload: manifest.variant_payload ?? 'nat',
  ...(manifest.implementations === undefined ? {} : { implementations: manifest.implementations }),
  ...(manifest.provenance === undefined ? {} : { provenance: manifest.provenance }),
  measurement_protocol: { fresh_canister: 'per workload/trial', forced_gc_calls_before_phase: 4,
    synchronous_gc: 'included in instruction count; reclaimed_bytes reported',
    end_of_message_gc: 'excluded', legacy_requires_no_gc: true },
  methodology: manifest.methodology ?? (explicitPhases ?
    'Fresh canister per workload/trial; configure inputs outside measurement; clear output and run four forced GC increments before each phase. Each synchronous update measures one call using performanceCounter(0), rts_total_allocation, and rts_reclaimed, retains its output, and replies after measurement. Raw counts include counter/assignment overhead and any synchronous collection; end-of-message GC is excluded. Explicit phases permit synchronous reclamation unless require_no_gc is true. Ordinary Candid phases are references/context, not equivalent canonical encoders.' :
    'Fresh canister per workload/trial; configure inputs outside measurement; four forced GC increments before each operation. Each update measures one call using performanceCounter(0) and rts_total_allocation, retains its output, and replies after measurement. Raw counts include counter/assignment overhead; control is reported separately. Ordinary to_candid has different semantics and is context, not an equivalent implementation.'),
  correctness: manifest.correctness ?? 'Every SHA-256 phase is checked against the oracle digest in the workload manifest. Every canonical Candid phase is checked against host canonicalization of ordinary Candid. Equivalent phase outputs must also match byte-for-byte within each trial.',
  failed_measurements: 0,
  cases: []
};
async function save() {
  await writeFile(out, JSON.stringify(report, (_, value) => typeof value === 'bigint' ?
    (value <= BigInt(Number.MAX_SAFE_INTEGER) ? Number(value) : value.toString()) : value, 2) + '\n');
}
try {
  for (let attempt = 0; attempt < 1000; attempt++) {
    if (spawnError) throw spawnError;
    if (server.exitCode !== null) throw Error(`PocketIC exited: ${logs}`);
    try {
      const port = (await readFile(portFile, 'utf8')).trim();
      if (/^[1-9][0-9]*$/.test(port)) { base = `http://127.0.0.1:${port}`; break; }
    } catch {}
    await delay(10);
  }
  if (!base) throw Error('PocketIC startup timed out');
  report.pocket_ic_version = execFileSync(options.server, ['--version'], { encoding: 'utf8' }).trim();
  const subnet = { state_config: 'New', instruction_config: 'Production', subnet_admins: null, cost_schedule: 'Normal' };
  const config = { subnet_config_set: { nns: null, sns: null, ii: null, fiduciary: null,
    bitcoin: null, test_threshold_keys: null, system: [], application: [subnet], cloud_engine: [], verified_application: [] },
    http_gateway_config: null, state_dir: null, icp_config: null, log_level: null, bitcoind_addr: null,
    dogecoind_addr: null, icp_features: null, incomplete_state: 'Disabled',
    initial_time: { Timestamp: { nanos_since_epoch: 1700000000000000000 } },
    mainnet_nns_subnet_id: false, disable_ingress_validation: false };
  const created = await rest('instances', 'POST', config);
  if (!created.Created) throw Error(`Cannot create instance: ${JSON.stringify(created)}`);
  instanceId = created.Created.instance_id;
  const effective = { CanisterId: created.Created.topology.default_effective_canister_id.canister_id };
  for (const spec of selectedCases) {
    const phases = phasesFor(spec);
    const result = { name: spec.name, family: spec.family,
      ...(spec.phases === undefined ? {} : { phases }), measurements: [] };
    report.cases.push(result);
    let hostReference;
    if (spec.reference_variant_count !== undefined) {
      const payloadType = manifest.variant_payload === 'null' ? IDL.Null : IDL.Nat;
      const choice = IDL.Variant(Object.fromEntries(Array.from({ length: manifest.variant_width },
        (_, index) => [`t${String(index).padStart(4, '0')}`, payloadType])));
      const values = Array.from({ length: spec.reference_variant_count }, (_, index) =>
        ({ [`t${String(index).padStart(4, '0')}`]: manifest.variant_payload === 'null' ? null : BigInt(index) }));
      hostReference = new Uint8Array(IDL.encode([IDL.Vec(choice)], [values]));
    }
    for (let trial = 0; trial < options.trials; trial++) {
      const [made] = await call(management, 'provisional_create_canister_with_cycles', [Create],
        [{ amount: [10n ** 15n], settings: [{ controllers: [[anon]] }], specified_id: [] }], [Created], false, effective);
      const canister = made.canister_id;
      await call(management, 'install_code', [Install], [{ mode: { install: null }, canister_id: canister,
        wasm_module: installWasm, arg: new Uint8Array(IDL.encode([], [])) }], [], false,
        { CanisterId: base64(canister.toUint8Array()) });
      await call(canister, 'configure', [IDL.Nat], [BigInt(spec.index)]);
      let ordinary = hostReference;
      let canonicalReference;
      const equivalentOutputs = new Map();
      for (const phase of phases) {
        await call(canister, 'clear');
        for (let i = 0; i < 4; i++) await call(canister, '__motoko_gc_trigger');
        let sample;
        try {
          [sample] = await call(canister, phase.method, [], [], [Measurement]);
          if (phase.require_no_gc) strictEqual(sample.reclaimed, 0n, 'Unexpected synchronous garbage collection');
          const [bytes] = await call(canister, 'bytesResult', [], [], [IDL.Vec(IDL.Nat8)], true);
          strictEqual(sample.outputSize, BigInt(bytes.length), 'Reply size mismatch');
          if (phase.kind === 'ordinary_candid') { ordinary = bytes; canonicalReference = undefined; }
          else if (phase.kind === 'canonical_candid') {
            if (!ordinary) throw Error(`${spec.name}: ordinary Candid reference did not succeed`);
            canonicalReference ??= canonicalize(ordinary);
            deepStrictEqual(bytes, canonicalReference, `${spec.name}/${phase.label}: canonical bytes`);
          } else if (phase.kind === 'sha256') {
            strictEqual(Buffer.from(bytes).toString('hex'), spec.expected_sha256_hex.toLowerCase(), `${spec.name}/${phase.label}: SHA-256 digest`);
          }
          if (phase.kind === 'canonical_candid' || phase.kind === 'sha256') {
            if (equivalentOutputs.has(phase.kind))
              deepStrictEqual(bytes, equivalentOutputs.get(phase.kind), `${spec.name}/${phase.label}: branch outputs differ`);
            else equivalentOutputs.set(phase.kind, bytes);
          }
          result.measurements.push({ trial, phase: phase.label, instructions: sample.instructions,
            allocated_bytes: sample.allocated, reclaimed_bytes: sample.reclaimed,
            output_bytes: sample.outputSize, output_sha256: sha(bytes) });
          console.log(`${options.label} ${spec.name} trial ${trial + 1} ${phase.label}: ${sample.instructions} instructions, ${sample.allocated} allocated, ${sample.reclaimed} reclaimed`);
        } catch (error) {
          report.failed_measurements++;
          result.measurements.push({ trial, phase: phase.label,
            ...(sample === undefined ? {} : { instructions: sample.instructions,
              allocated_bytes: sample.allocated, reclaimed_bytes: sample.reclaimed }), error: String(error) });
          console.log(`${options.label} ${spec.name} trial ${trial + 1} ${phase.label}: ${error}`);
          if (!/Canister(?:Called)?Trap|CanisterInstructionLimitExceeded/.test(String(error))) throw error;
        }
        await save();
      }
    }
  }
  await save();
  console.log(out);
  if (explicitPhases && report.failed_measurements > 0) process.exitCode = 1;
} catch (error) {
  report.fatal_error = String(error);
  await save();
  throw error;
} finally {
  if (instanceId !== undefined && base) await rest(`instances/${instanceId}`, 'DELETE').catch(() => {});
  server.kill('SIGTERM');
  await writeFile(`${out}.log`, logs);
  await unlink(portFile).catch(() => {});
}
