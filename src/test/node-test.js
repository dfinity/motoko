var assert = require('assert');
var m = require('asc.js');

var empty_wasm = m.ActorScript.compileWasm('');

assert.equal(typeof(empty_wasm), 'string');
assert.equal(empty_wasm.substr(0,4), '\0asm');
assert.equal(empty_wasm.substr(4,4), '\1\0\0\0');

/* There must be a better way, ideally of providing an ArrayBuffer directly in
 * asc.js... help appreciated. */
function strToBuffer (string) {
  let arrayBuffer = new ArrayBuffer(string.length * 1);
  let newUint = new Uint8Array(arrayBuffer);
  newUint.forEach((_, i) => {
    newUint[i] = string.charCodeAt(i);
  });
  return newUint;
}

WebAssembly.compile(strToBuffer(empty_wasm))
  .catch(err => assert.fail(err));


