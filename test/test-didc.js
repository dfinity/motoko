process.on('unhandledRejection', error => { assert.fail(error); });

const assert = require('assert').strict;

// Load didc.js
const didc = require('didc.js').Didc;
assert.equal(didc.jsOfDid("service : { hello : (text) -> (text) query }"), 'export default ({ IDL }) => {\n return IDL.Service({\'hello\': IDL.Func([IDL.Text], [IDL.Text], [\'query\'])});\n};');
