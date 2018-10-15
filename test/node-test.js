const assert = require('assert');

// Load asc.js
const m = require('asc.js');

// Compile the empty module in plain and dfinity mode
const empty_wasm_plain = m.ActorScript.compileWasm('wasm', false, '');
const empty_wasm_dfinity = m.ActorScript.compileWasm('dfinity', false, '');

// For the plain module...
// Check that the code looks like a WebAssembly binary
assert.equal(typeof(empty_wasm_plain), 'object');
assert.equal(empty_wasm_plain.code.substr(0,4), '\0asm');
assert.equal(empty_wasm_plain.code.substr(4,4), '\1\0\0\0');
assert.equal(typeof(empty_wasm_plain.diagnostics), 'object');
assert.equal(empty_wasm_plain.diagnostics.length, 0);

// Check that the WebAssembly binary can be loaded
WebAssembly.compile(Buffer.from(empty_wasm_plain.code, 'ascii'))
  .catch(err => assert.fail(err));

// Now again for the definity module
assert.equal(typeof(empty_wasm_dfinity), 'object');
assert.equal(empty_wasm_dfinity.code.substr(0,4), '\0asm');
assert.equal(empty_wasm_dfinity.code.substr(4,4), '\1\0\0\0');
assert.equal(typeof(empty_wasm_dfinity.diagnostics), 'object');
assert.equal(empty_wasm_dfinity.diagnostics.length, 0);

WebAssembly.compile(Buffer.from(empty_wasm_dfinity.code, 'ascii'))
  .catch(err => assert.fail(err));

// The plain and the dfinity module should not be the same
assert.notEqual(empty_wasm_plain.code, empty_wasm_dfinity.code);

// Check if error messages are correctly returned
const bad_result = m.ActorScript.compileWasm('dfinity', false, '1+');
// Uncomment to see what to paste below
// console.log(JSON.stringify(bad_result, null, 2));
assert.deepStrictEqual(bad_result, {
  "diagnostics": [
    {
      "range": {
        "start": {
          "line": 0,
          "character": 2
        },
        "end": {
          "line": 0,
          "character": 2
        }
      },
      "severity": 1,
      "source": "actorscript",
      "message": "unexpected token"
    }
  ],
  "code": null,
  "map": null
});

// Check the check command (should print errors, but have no code)
assert.deepStrictEqual(m.ActorScript.check('1'), {
  "diagnostics": [],
  "code": null
});

assert.deepStrictEqual(m.ActorScript.check('1+'), {
  "diagnostics": [
    {
      "range": {
        "start": {
          "line": 0,
          "character": 2
        },
        "end": {
          "line": 0,
          "character": 2
        }
      },
      "severity": 1,
      "source": "actorscript",
      "message": "unexpected token"
    }
  ],
  "code": null
});

// Create a source map, and check some of its structure
const with_map = m.ActorScript.compileWasm('dfinity', true, '');
assert.equal(typeof(with_map.map), 'string')
let map
assert.doesNotThrow(() => map = JSON.parse(with_map.map), SyntaxError)
assert.ok(Array.isArray(map.sources))
assert.ok(Array.isArray(map.sourcesContent))
assert.equal(typeof(map.mappings), 'string')
assert.equal(typeof(map.version), 'number')
