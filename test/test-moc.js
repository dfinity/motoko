process.on("unhandledRejection", (error) => {
  assert.fail(error);
});

const assert = require("assert").strict;

// Load moc.js
const moc = require("moc.js");

// Store files
moc.Motoko.saveFile("empty.mo", "");
moc.Motoko.saveFile("ok.mo", "1");
moc.Motoko.saveFile("bad.mo", "1+");
moc.Motoko.saveFile(
  "actor.mo",
  'actor { type A<B> = B; public query func main() : async A<Text> { "abc" } }'
);

assert.equal(moc.Motoko.readFile("empty.mo"), "");
assert.equal(moc.Motoko.readFile("ok.mo"), "1");

// Compile the empty module in wasi and ic mode
const empty_wasm_plain = moc.Motoko.compileWasm("wasi", "empty.mo");
const empty_wasm_ic = moc.Motoko.compileWasm("ic", "empty.mo");

// For the plain module...
// Check that the code looks like a WebAssembly binary
assert.equal(typeof empty_wasm_plain, "object");
assert.deepEqual(
  empty_wasm_plain.code.wasm.subarray(0, 4),
  new Uint8Array([0, 97, 115, 109])
);
assert.deepEqual(
  empty_wasm_plain.code.wasm.subarray(4, 8),
  new Uint8Array([1, 0, 0, 0])
);
assert.equal(typeof empty_wasm_plain.diagnostics, "object");
assert.equal(empty_wasm_plain.diagnostics.length, 0);

// Check that the WebAssembly binary can be loaded
WebAssembly.compile(empty_wasm_plain.code.wasm);

// Now again for the ic module
assert.equal(typeof empty_wasm_ic, "object");
assert.deepEqual(
  empty_wasm_plain.code.wasm.subarray(0, 4),
  new Uint8Array([0, 97, 115, 109])
);
assert.deepEqual(
  empty_wasm_plain.code.wasm.subarray(4, 8),
  new Uint8Array([1, 0, 0, 0])
);
assert.equal(typeof empty_wasm_ic.diagnostics, "object");
assert.equal(empty_wasm_ic.diagnostics.length, 0);

WebAssembly.compile(empty_wasm_ic.code.wasm);

// The plain and the ic module should not be the same
assert.notEqual(empty_wasm_plain.code.wasm, empty_wasm_ic.code.wasm);

moc.Motoko.removeFile("empty.mo");
assert.throws(() => {
  moc.Motoko.compileWasm("ic", "empty.mo");
}, /No such file or directory/);

// Check if error messages are correctly returned
const bad_result = moc.Motoko.compileWasm("ic", "bad.mo");
// Uncomment to see what to paste below
// console.log(JSON.stringify(bad_result, null, 2));
assert.deepStrictEqual(bad_result, {
  diagnostics: [
    {
      range: {
        start: {
          line: 0,
          character: 2,
        },
        end: {
          line: 0,
          character: 2,
        },
      },
      severity: 1,
      source: "bad.mo",
      message:
        "unexpected end of input, expected one of token or <phrase> sequence:\n  <exp_bin(ob)>",
    },
  ],
  code: null,
});

// Check the check command (should print errors, but have no code)
assert.deepStrictEqual(moc.Motoko.check("ok.mo"), {
  diagnostics: [],
  code: null,
});

assert.deepStrictEqual(moc.Motoko.check("bad.mo"), {
  diagnostics: [
    {
      range: {
        start: {
          line: 0,
          character: 2,
        },
        end: {
          line: 0,
          character: 2,
        },
      },
      severity: 1,
      source: "bad.mo",
      message:
        "unexpected end of input, expected one of token or <phrase> sequence:\n  <exp_bin(ob)>",
    },
  ],
  code: null,
});

// Check WASM reproducibility
assert.deepStrictEqual(
  moc.Motoko.compileWasm("ic", "actor.mo").code.wasm,
  moc.Motoko.compileWasm("ic", "actor.mo").code.wasm
);
