const assert = require('assert');
const m = require('asc.js');

const empty_wasm_plain = m.ActorScript.compileWasm('wasm', '', false);
const empty_wasm_dfinity = m.ActorScript.compileWasm('dfinity', '', false);

assert.equal(typeof(empty_wasm_plain), 'object');
assert.equal(empty_wasm_plain.code.substr(0,4), '\0asm');
assert.equal(empty_wasm_plain.code.substr(4,4), '\1\0\0\0');
assert.equal(typeof(empty_wasm_plain.diagnostics), 'object');
assert.equal(empty_wasm_plain.diagnostics.length, 0);

WebAssembly.compile(Buffer.from(empty_wasm_plain.code, 'ascii'))
  .catch(err => assert.fail(err));

assert.equal(typeof(empty_wasm_dfinity), 'object');
assert.equal(empty_wasm_dfinity.code.substr(0,4), '\0asm');
assert.equal(empty_wasm_dfinity.code.substr(4,4), '\1\0\0\0');
assert.equal(typeof(empty_wasm_dfinity.diagnostics), 'object');
assert.equal(empty_wasm_dfinity.diagnostics.length, 0);

WebAssembly.compile(Buffer.from(empty_wasm_dfinity.code, 'ascii'))
  .catch(err => assert.fail(err));

assert.notEqual(empty_wasm_plain.code, empty_wasm_dfinity.code);

const bad_result = m.ActorScript.compileWasm('dfinity', '1+', false);
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

const with_map = m.ActorScript.compileWasm('dfinity', '', true);
