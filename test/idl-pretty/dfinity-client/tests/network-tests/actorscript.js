const tape = require('tape')
const fs = require('fs')
const { Client, IDL, Types } = require('../../')

tape('hello-world', async t => {
  const client = new Client({ secretKey: Buffer.from('bf827d7cb181df0c960fdce8b3afcd2e6fda98c60215f22aee1cf656b606de33', 'hex') })
  try {
    await client.connect()
  } catch (e) {
    console.log(e)
    console.log('run local testnet for Client tests!')
    process.exit()
  }

  const wasm = await fs.readFileSync(`${__dirname}/../actors/hello-world-message.wasm`)
  const sig = new IDL.ActorInterface({
    hello: IDL.Message(IDL.Text, IDL.Text)
  })
  const a = await sig.new(client, wasm)
  const output = await a.hello('World')
  t.equals(output.toString(), 'Hello World!', 'got Hello World')

  const output2 = await a.hello(new Types.UserIdentity())
  t.equals(output2.toString(), 'Hello d3e45f43d121501d87cfa88e6f08f4b9238e1652a2a536485a96cfc88d34fc10!', 'got Hello user')
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

  const wasm = await fs.readFileSync(`${__dirname}/../actors/structured-params.wasm`)
  const sig = new IDL.ActorInterface({
    hello: IDL.Message(IDL.Arr(IDL.Obj({who: IDL.Text, what: IDL.Text, count: IDL.Int})), IDL.Text)
  })
  const a = await sig.new(client, wasm)
  const output = await a.hello([
    { who: 'Turing', what: 'beer', count: 3 },
    { who: 'Church', what: 'wine', count: 5 }
  ])
  t.equals(output, 'Reporting this:\nTuring drank 3 bottles of beer.\nChurch drank 5 bottles of wine.\n', 'got expected result')
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

  const wasm = await fs.readFileSync(`${__dirname}/../actors/transpose.wasm`)
  const sig = new IDL.ActorInterface({
    transpose: IDL.Message(IDL.Arr(IDL.Tuple(IDL.Int, IDL.Text)), IDL.Obj({'ints': IDL.Arr(IDL.Int), 'txts': IDL.Arr(IDL.Text)}))
  })
  const a = await sig.new(client, wasm)
  const output = await a.transpose([
    [5, 'Hi'],
    [6, 'Ho']
  ])
  t.deepEquals(output, {'ints': [5, 6], 'txts': ['Hi', 'Ho']}, 'got expected result')

  try {
    await a.transpose()
    t.fail()
  } catch (e) {
    t.equal(e.message, 'Wrong number of message arguments', 'Arg length')
  }
  t.end()
})

tape('counter, multiple ingress msgs', async t => {
  const client = new Client()
  try {
    await client.connect()
  } catch (e) {
    console.log(e)
    console.log('run local testnet for Client tests!')
    process.exit()
  }

  const wasm = await fs.readFileSync(`${__dirname}/../actors/counter.wasm`)
  const sig = new IDL.ActorInterface({
    inc: IDL.Message(),
    get: IDL.Message([], IDL.Int)
  })
  const a = await sig.new(client, wasm)
  const x = await a.batch()([
    a.inc(),
    a.get(),
    a.inc(),
    a.inc(),
    a.get(),
    a.get()
  ])
  t.equals(x.length, 6, 'expected number of returns')
  t.equals(x[1], 1)
  t.equals(x[4], 3)
  t.equals(x[5], 3)
  t.end()
})

tape('option', async t => {
  const client = new Client()
  try {
    await client.connect()
  } catch (e) {
    console.log(e)
    console.log('run local testnet for Client tests!')
    process.exit()
  }

  const wasm = await fs.readFileSync(`${__dirname}/../actors/option.wasm`)
  const sig = new IDL.ActorInterface({
    get: IDL.Message(IDL.Opt(IDL.Int), IDL.Opt(IDL.Int))
  })
  const a = await sig.new(client, wasm)
  const x = await a.get(2)
  t.equals(x, 2)
  const x2 = await a.get(null)
  t.equals(x2, null)
  t.end()
})
