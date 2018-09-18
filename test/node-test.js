var assert = require('assert');
var m = require('asc.js');

var empty_wasm = m.ActorScript.compileWasm('');

assert.equal(typeof(empty_wasm), 'string');
assert.equal(empty_wasm.substr(0,4), '\0asm');
assert.equal(empty_wasm.substr(4,4), '\1\0\0\0');

WebAssembly.compile(Buffer.from(empty_wasm,'ascii'))
  .catch(err => assert.fail(err));


