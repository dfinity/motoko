var assert = require('assert');
var m = require('asc.js');

var empty_wasm_plain = m.ActorScript.compileWasm(false,'');
var empty_wasm_dfinity = m.ActorScript.compileWasm(true, '');

assert.equal(typeof(empty_wasm_plain), 'object');
assert.equal(empty_wasm_plain.code.substr(0,4), '\0asm');
assert.equal(empty_wasm_plain.code.substr(4,4), '\1\0\0\0');
assert.equal(typeof(empty_wasm_plain.diagnostics), 'object');
assert.equal(empty_wasm_plain.diagnostics.length, 0);

WebAssembly.compile(Buffer.from(empty_wasm_plain.code,'ascii'))
  .catch(err => assert.fail(err));

assert.equal(typeof(empty_wasm_dfinity), 'object');
assert.equal(empty_wasm_dfinity.code.substr(0,4), '\0asm');
assert.equal(empty_wasm_dfinity.code.substr(4,4), '\1\0\0\0');
assert.equal(typeof(empty_wasm_dfinity.diagnostics), 'object');
assert.equal(empty_wasm_dfinity.diagnostics.length, 0);

WebAssembly.compile(Buffer.from(empty_wasm_dfinity.code,'ascii'))
  .catch(err => assert.fail(err));

assert.notEqual(empty_wasm_plain.code, empty_wasm_dfinity.code);

var bad_result = m.ActorScript.compileWasm(true,'1+');
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
  "code": null
});

assert.deepStrictEqual(m.ActorScript.checkString('1'), {
  "diagnostics": [],
  "code": null
});

assert.deepStrictEqual(m.ActorScript.checkString('1+'), {
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
