var assert = require('assert');
var m = require('asc.js');

var empty_wasm_plain = m.ActorScript.compileWasm(false,'');
var empty_wasm_dfinity = m.ActorScript.compileWasm(true, '');

assert.equal(typeof(empty_wasm_plain), 'string');
assert.equal(empty_wasm_plain.substr(0,4), '\0asm');
assert.equal(empty_wasm_plain.substr(4,4), '\1\0\0\0');

WebAssembly.compile(Buffer.from(empty_wasm_plain,'ascii'))
  .catch(err => assert.fail(err));

assert.equal(typeof(empty_wasm_dfinity), 'string');
assert.equal(empty_wasm_dfinity.substr(0,4), '\0asm');
assert.equal(empty_wasm_dfinity.substr(4,4), '\1\0\0\0');

WebAssembly.compile(Buffer.from(empty_wasm_dfinity,'ascii'))
  .catch(err => assert.fail(err));

assert.notEqual(empty_wasm_plain, empty_wasm_dfinity);
