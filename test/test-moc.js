process.on("unhandledRejection", (error) => {
  assert.fail(error);
});

const assert = require("assert").strict;

// Load moc.js
const { Motoko } = require("moc.js");

// Store files
Motoko.saveFile("empty.mo", "");
Motoko.saveFile("ok.mo", "1");
Motoko.saveFile("bad.mo", "1+");
Motoko.saveFile(
  "actor.mo",
  'actor { type A<B> = B; public query func main() : async A<Text> { "abc" } }'
);
Motoko.saveFile(
  "ast.mo",
  `
  /** Program comment
      multi-line */
  import Prim "mo:prim";

  actor {
    /// Type comment
    type T = Nat;
    /// Variable comment
    stable var x : T = 0;
    /** Function comment */
    public query func main() : async T { x };
    /// Sub-module comment
    module M {
      /// Class comment
      public class C() {};
    };
  }`
);
Motoko.saveFile("text.mo", `let s = "${"⛔|".repeat(10000)}"; s.size()`); // #3822

assert.equal(Motoko.readFile("empty.mo"), "");
assert.equal(Motoko.readFile("ok.mo"), "1");

// Compile the empty module in wasi and ic mode
const empty_wasm_plain = Motoko.compileWasm("wasi", "empty.mo");
const empty_wasm_ic = Motoko.compileWasm("ic", "empty.mo");

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

Motoko.removeFile("empty.mo");
assert.throws(() => {
  Motoko.compileWasm("ic", "empty.mo");
}, /No such file or directory/);

// Check if error messages are correctly returned
const bad_result = Motoko.compileWasm("ic", "bad.mo");
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
      code: "M0001",
      category: "syntax",
      message:
        "unexpected end of input, expected one of token or <phrase> sequence:\n  <exp_bin(ob)>",
    },
  ],
  code: null,
});

// Check the check command (should print errors, but have no code)
assert.deepStrictEqual(Motoko.check("ok.mo"), {
  diagnostics: [],
  code: null,
});

assert.deepStrictEqual(Motoko.check("bad.mo"), {
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
      category: "syntax",
      code: "M0001",
      message:
        "unexpected end of input, expected one of token or <phrase> sequence:\n  <exp_bin(ob)>",
    },
  ],
  code: null,
});

const astString = JSON.stringify(
  Motoko.parseMotoko(Motoko.readFile("ast.mo"))
);

// Run interpreter
assert.deepStrictEqual(Motoko.run([], "actor.mo"), {
  stdout: "`ys6dh-5cjiq-5dc` : actor {main : shared query () -> async A__9<Text>}\n",
  stderr: "",
  result: { error: null },
});

// Check doc comments
assert.match(astString, /"name":"\*","args":\["Program comment\\n      multi-line"/);
assert.match(astString, /"name":"\*","args":\["Type comment"/);
assert.match(astString, /"name":"\*","args":\["Variable comment"/);
assert.match(astString, /"name":"\*","args":\["Function comment"/);
assert.match(astString, /"name":"\*","args":\["Sub-module comment"/);
assert.match(astString, /"name":"\*","args":\["Class comment"/);

// Check that long text literals type-check without error
assert.deepStrictEqual(Motoko.check("text.mo"), {
  code: null,
  diagnostics: [],
});

const candid = `
type T = nat;
/// Program comment
///       multi-line
service : {
  /// Function comment
  main: () -> (T) query;
}
`.trim() + '\n';
assert.deepStrictEqual(Motoko.candid('ast.mo'), {
  diagnostics: [
    {
      category: 'type',
      code: 'M0194',
      message: 'unused identifier Prim (delete or rename to wildcard `_` or `_Prim`)',
      range: {
        end: {
          character: 13,
          line: 3
        },
        start: {
          character: 9,
          line: 3
        }
      },
      severity: 2,
      source: 'ast.mo'
    },
    {
      category: 'type',
      code: 'M0194',
      message: 'unused identifier M (delete or rename to wildcard `_` or `_M`)',
      range: {
        end: {
          character: 12,
          line: 13
        },
        start: {
          character: 11,
          line: 13
        }
      },
      severity: 2,
      source: 'ast.mo'
    }
  ], code: candid
});
