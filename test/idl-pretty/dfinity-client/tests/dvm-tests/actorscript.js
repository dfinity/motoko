const util = require('util');
const tape = require('tape')
const fs = require('fs')
const path = require('path');
const { withFile } = require('tmp-promise');
const execFile = util.promisify(require('child_process').execFile);

const { IDL, Types } = require('../../')
const { DvmClient } = require('../../src/DvmClient')

const Client = DvmClient;

const compileToWasm = fn =>
  {
    return withFile(async ({path, fd}) => {
      const { stderr, stdout } = await execFile('asc', [ fn, '-o', path, '-no-check-ir'])
      return await fs.readFileSync(path)
    },{ prefix: path.basename(fn,'.as') + '_', postfix: '.wasm'})
  }

tape('hello-world', async t => {
  const client = new Client({ secretKey: Buffer.from('bf827d7cb181df0c960fdce8b3afcd2e6fda98c60215f22aee1cf656b606de33', 'hex') })
  try {
    await client.connect()
  } catch (e) {
    console.log(e)
    console.log('run local testnet for Client tests!')
    process.exit()
  }

  const wasm = await compileToWasm(`${__dirname}/../actors/hello-world-message.as`)
  const sig = new IDL.ActorInterface({
    hello: IDL.Message(IDL.Text, IDL.Text)
  })
  const a = await sig.new(client, wasm)
  const output = await a.hello('World')
  t.equals(output.toString(), 'Hello World!', 'got Hello World')

  /* Doesnt work with dvm
  const output2 = await a.hello(new Types.UserIdentity())
  t.equals(output2.toString(), 'Hello d3e45f43d121501d87cfa88e6f08f4b9238e1652a2a536485a96cfc88d34fc10!', 'got Hello user')
  */

  t.end()
})

tape('structured-params', async t => {
  const client = new Client()
  try {
    await client.connect()
  } catch (e) {
    console.log(e)
    console.log('run local testnet for Client tests!')
    process.exit()
  }

  const wasm = await compileToWasm(`${__dirname}/../actors/structured-params.as`)
  const sig = new IDL.ActorInterface({
    hello: IDL.Message(IDL.Arr(IDL.Obj({who: IDL.Text, what: IDL.Text, count: IDL.Int})), IDL.Text),
    returnRecord: IDL.Message([], IDL.Arr(IDL.Obj({id: IDL.Nat, short_name: IDL.Text, description: IDL.Text}))),
  })
  const a = await sig.new(client, wasm)
  const output1 = await a.hello([])
  t.equals(output1, 'Reporting this:\n', 'got expected result')

  const output2 = await a.hello([
    { who: 'Turing', what: 'beer', count: 3 },
    { who: 'Church', what: 'wine', count: 5 }
  ])
  t.equals(output2, 'Reporting this:\nTuring drank 3 bottles of beer.\nChurch drank 5 bottles of wine.\n', 'got expected result')

  const output3 = await a.returnRecord()
  t.deepEquals(output3, [
    { id : 1, short_name: 'C', description: 'Central' }
  ])
  t.end()
})

tape('structured-return', async t => {
  const client = new Client()
  try {
    await client.connect()
  } catch (e) {
    console.log(e)
    console.log('run local testnet for Client tests!')
    process.exit()
  }

  const wasm = await compileToWasm(`${__dirname}/../actors/transpose.as`)
  const sig = new IDL.ActorInterface({
    transpose: IDL.Message(IDL.Arr(IDL.Tuple(IDL.Int, IDL.Text)), IDL.Obj({'ints': IDL.Arr(IDL.Int), 'txts': IDL.Arr(IDL.Text)}))
  })
  const a = await sig.new(client, wasm)
  const output1 = await a.transpose([])
  t.deepEquals(output1, {'ints': [], 'txts': []}, 'got expected result')

  const output2 = await a.transpose([
    [5, 'Hi'],
    [6, 'Ho']
  ])
  t.deepEquals(output2, {'ints': [5, 6], 'txts': ['Hi', 'Ho']}, 'got expected result')

  try {
    await a.transpose()
    t.fail()
  } catch (e) {
    t.equal(e.message, 'Wrong number of message arguments', 'Arg length')
  }
  t.end()
})
