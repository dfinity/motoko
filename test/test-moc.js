process.on("unhandledRejection", (error) => {
  console.log(`Unhandled promise rejection:\n${error}`);
});

process.on("uncaughtException", (error) => {
  console.log(`Uncaught exception:\n${error}`);
});

const assert = require("assert").strict;
const fs = require("fs");
const path = require("path");
const { execSync } = require("child_process");

// Load moc.js
const { Motoko } = require("moc.js");

// Store files
Motoko.saveFile("empty.mo", "");
Motoko.saveFile("ok.mo", "1");
Motoko.saveFile("bad.mo", "1+");
Motoko.saveFile(
  "actor.mo",
  'persistent actor { type A<B> = B; public query func main() : async A<Text> { "abc" } }'
);
Motoko.saveFile(
  "ast.mo",
  `
  /** Program comment
      multi-line */
  import Prim "mo:prim";

  persistent actor {
    /// Type comment
    type T = Nat;
    /// Variable comment
    var x : T = 0;
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
        "unexpected end of input, expected one of token or <phrase> sequence:\n  <exp_bin(ob)> (e.g. '42')",
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
        "unexpected end of input, expected one of token or <phrase> sequence:\n  <exp_bin(ob)> (e.g. '42')",
    },
  ],
  code: null,
});

// Run interpreter
assert.deepStrictEqual(Motoko.run([], "actor.mo"), {
  stdout:
    "`ys6dh-5cjiq-5dc` : actor {main : shared query () -> async A<Text>}\n",
  stderr: "",
  result: { error: null },
});

// Check AST format
const astFile = Motoko.readFile("ast.mo");
for (const ast of [
  Motoko.parseMotoko(/*enable_recovery=*/ false, astFile),
  Motoko.parseMotokoTyped(["ast.mo"]).code[0].ast,
  Motoko.parseMotokoTypedWithScopeCache(
    /*enable_recovery=*/ false,
    ["ast.mo"],
    new Map()
  ).code[0][0].ast, // { diagnostics; code: [[{ ast; immediateImports }], cache] }
  Motoko.parseMotoko(/*enable_recovery=*/ true, astFile),
  Motoko.parseMotokoTypedWithScopeCache(
    /*enable_recovery=*/ true,
    ["ast.mo"],
    new Map()
  ).code[0][0].ast, // { diagnostics; code: [[{ ast; immediateImports }], cache] }
]) {
  const astString = JSON.stringify(ast);

  // Check doc comments
  assert.match(
    astString,
    /"name":"\*","args":\["Program comment\\n      multi-line"/
  );
  assert.match(astString, /"name":"\*","args":\["Type comment"/);
  assert.match(astString, /"name":"\*","args":\["Variable comment"/);
  assert.match(astString, /"name":"\*","args":\["Function comment"/);
  assert.match(astString, /"name":"\*","args":\["Sub-module comment"/);
  assert.match(astString, /"name":"\*","args":\["Class comment"/);
}

// Check that long text literals type-check without error
assert.deepStrictEqual(Motoko.check("text.mo"), {
  code: null,
  diagnostics: [],
});

// Check Candid format
const candid =
  `
type T = nat;
/// Program comment
///       multi-line
service : {
  /// Function comment
  main: () -> (T) query;
}
`.trim() + "\n";
assert.deepStrictEqual(Motoko.candid("ast.mo"), {
  diagnostics: [
    {
      category: "type",
      code: "M0194",
      message:
        "unused identifier Prim (delete or rename to wildcard `_` or `_Prim`)",
      range: {
        end: {
          character: 13,
          line: 3,
        },
        start: {
          character: 9,
          line: 3,
        },
      },
      severity: 2,
      source: "ast.mo",
    },
    {
      category: "type",
      code: "M0194",
      message: "unused identifier M (delete or rename to wildcard `_` or `_M`)",
      range: {
        end: {
          character: 12,
          line: 13,
        },
        start: {
          character: 11,
          line: 13,
        },
      },
      severity: 2,
      source: "ast.mo",
    },
  ],
  code: candid,
});

// Check error recovery
const badAstFile = Motoko.readFile("bad.mo");

assert(Motoko.parseMotoko(/*enable_recovery=*/ false, badAstFile).code == null);
assert(Motoko.parseMotoko(/*enable_recovery=*/ true, badAstFile).code != null);
assert(
  Motoko.parseMotokoTypedWithScopeCache(
    /*enable_recovery=*/ false,
    ["bad.mo"],
    new Map()
  ).code == null
);
// TODO: This requires avoid dropping 'code' field in all checks though all pipeline e.g. infer_prog
// assert(Motoko.parseMotokoTypedWithScopeCache(/*enable_recovery=*/true, ["bad.mo"], new Map()).code != null);

const baseDir = process.env.MOTOKO_BASE;
console.log("Base library path:", baseDir);

if (!fs.existsSync(baseDir)) {
  throw new Error(`Base library not found: ${baseDir}`);
}

const baseFiles = fs
  .readdirSync(baseDir)
  .filter((file) => file.endsWith(".mo"));
assert.notEqual(baseFiles, []);
baseFiles.forEach((file) => {
  assert.match(file, /\.mo$/);
  Motoko.saveFile(
    path.join("base-path", file),
    fs.readFileSync(path.join(baseDir, file), "utf8")
  );
});
Motoko.addPackage("base", "base-path");

Motoko.saveFile(
  "Wasm.mo",
  `
  import Int "mo:base/Int";
  import OrderedSet "mo:base/OrderedSet";
  import Iter "mo:base/Iter";

  persistent actor {
      public func sortAndRemoveDuplicates(array : [Int]) : async [Int] {
          let Set = OrderedSet.Make<Int>(Int.compare);
          let set = Set.fromIter(Iter.fromArray(array));
          Iter.toArray(Set.vals(set));
      };

      public func run() : async () {
          assert ((await sortAndRemoveDuplicates([])) == []);
          assert ((await sortAndRemoveDuplicates([1])) == [1]);
          assert ((await sortAndRemoveDuplicates([1, 2, 3])) == [1, 2, 3]);
          assert ((await sortAndRemoveDuplicates([3, 2, 1])) == [1, 2, 3]);
          assert ((await sortAndRemoveDuplicates([2, 2, 1, 3, 3])) == [1, 2, 3]);
          assert ((await sortAndRemoveDuplicates([1, 1, 1, 1, 1])) == [1]);
          assert ((await sortAndRemoveDuplicates([1, -1, 1, -1, 1])) == [-1, 1]);
          assert ((await sortAndRemoveDuplicates([-2, -3, -2, -4, -1, -3, -3])) == [-4, -3, -2, -1]);
      };
  };
  `
);

const wasmResult = Motoko.compileWasm("ic", "Wasm.mo");
assert.equal(typeof wasmResult, "object");
assert.deepEqual(wasmResult.diagnostics, []);
assert.notEqual(wasmResult.code, null);

const tempDir = __dirname; // TODO

const wasmPath = path.join(tempDir, "temp.wasm");
fs.writeFileSync(wasmPath, wasmResult.code.wasm);

const drunPath = path.join(tempDir /* TODO */, "drun-wrapper.sh");
let scriptPath;

try {
  scriptPath = path.join(tempDir, "test-script.txt");
  fs.writeFileSync(
    scriptPath,
    `
    install ${wasmPath}
    call 0x42 sortAndRemoveDuplicates (vec { 3; 2; 1; 2 })
    call 0x42 run ()
    `
  );

  const result = execSync(`${drunPath} < ${scriptPath}`, {
    encoding: "utf8",
    cwd: tempDir,
  });

  assert.match(result, /vec \{ 1; 2; 3 \}/);
} finally {
  if (fs.existsSync(wasmPath)) fs.unlinkSync(wasmPath);
  if (scriptPath && fs.existsSync(scriptPath)) fs.unlinkSync(scriptPath);
}

Motoko.removeFile("Wasm.mo");
