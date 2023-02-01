process.on('unhandledRejection', error => { assert.fail(error); });

const assert = require('assert').strict;

// Load moc.js
const moc = require('moc_interpreter.js');

// Store files
moc.Motoko.saveFile('empty.mo', '');
moc.Motoko.saveFile('ok.mo', '1');
moc.Motoko.saveFile('bad.mo', '1+');
moc.Motoko.saveFile('limit.mo', 'var i = 0; while (i < 10000) { i += 1 }; i');

assert.deepStrictEqual(moc.Motoko.run([], 'ok.mo'), {
  result: 0,
  stderr: '',
  stdout: '1 : Nat\n'
});

assert.deepStrictEqual(moc.Motoko.run([], 'bad.mo'), {
  result: 0,
  stderr: 'bad.mo:1.3: syntax error [M0001], unexpected end of input, expected one of token or <phrase> sequence:\n  <exp_bin(ob)>\n',
  stdout: '',
});

assert.deepStrictEqual(moc.Motoko.run([], 'limit.mo'), {
  result: 0,
  stderr: '',
  stdout: '10_000 : Nat\n'
});

moc.Motoko.setRunStepLimit(5000);

assert.deepStrictEqual(moc.Motoko.run([], 'limit.mo'), {
  result: 0,
  stderr: 'cancelled: interpreter reached step limit\n',
  stdout: ''
});
