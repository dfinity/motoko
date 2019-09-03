const cbor = require('borc')
const crypto = require('crypto')
const fs = require('fs')
const tape = require('tape')
const { Types, Client } = require('../../')

tape('client', async t => {
  const client = new Client({ debug: true, secretKey: Buffer.from('bf827d7cb181df0c960fdce8b3afcd2e6fda98c60215f22aee1cf656b606de33', 'hex') })
  const publicKey = 'd3e45f43d121501d87cfa88e6f08f4b9238e1652a2a536485a96cfc88d34fc10'

  try {
    await client.connect()
  } catch (e) {
    console.log(e)
    console.log('run local testnet for Client tests!')
    process.exit()
  }

  const getterWasm = fs.readFileSync(`${__dirname}/../actors/getter.wasm`)
  const [output] = await client.newAndCallResult('getter', getterWasm, 'get_data')
  t.ok(output.includes('DATA1\nDATA2\nDATA3'), 'got output')

  const aId = await client.new('getter2', getterWasm)
  const aId2 = client.getActor('getter2')
  t.equals(aId2, aId, 'new actor id')
  const [output2] = await client.callResult(aId2, 'get_data')
  t.ok(output2.includes('DATA1\nDATA2\nDATA3'), 'got output2')

  const namedWasm = Buffer.from('0061736d01000000010401600000030201000707010361646400000a040102000b000b046e616d65000461626364', 'hex')
  const inputArgs = Types.parseWasmArgs([
    2147483647,
    9007199254740991, // Number.MAX_SAFE_INTEGER
    'console_module',
    'console_actor',
    'default_console',
    new Types.FuncRef({
      _actorId: 'console_actor',
      _wasmArgTypes: [new Types.WasmI32Type()],
      _funcName: 'logI32'
    }),
    '0xdeadbeef',
    '',
    new Types.ElemBuf(Types.parseWasmArgs([123, 456])),
    new Types.UserIdentity()
  ])
  const outputArgs = Types.parseWasmArgs([
    2147483647,
    9007199254740991, // Number.MAX_SAFE_INTEGER
    new Types.ModuleRef(Buffer.from('console_module')),
    new Types.ActorRef(new Types.RegularActor(Buffer.from(publicKey, 'hex'))),
    new Types.ActorRef(new Types.ActorID('default_console')),
    new Types.FuncRef({
      _actorId: 'console_actor',
      _wasmArgTypes: [new Types.WasmI32Type()],
      _funcName: 'logI32'
    }),
    '0xdeadbeef',
    '',
    new Types.ElemBuf(Types.parseWasmArgs([123, 456])),
    Buffer.from(publicKey)
  ])

  await client.new('named', namedWasm)

  const aId4 = await client.new('getter3', getterWasm)

  const input = new Types.ElemBuf(inputArgs)
  const outputElems = new Types.ElemBuf(outputArgs)
  const data = await client.call(aId4, 'get_elem', ['console_actor', input])
  const [refResult] = client.formatOutput(data)
  t.deepEquals(refResult, Buffer.from('82079f82001a7fffffff82011b001fffffffffffff82034e636f6e736f6c655f6d6f64756c65820482015820d3e45f43d121501d87cfa88e6f08f4b9238e1652a2a536485a96cfc88d34fc10820482014f64656661756c745f636f6e736f6c65840682014d636f6e736f6c655f6163746f729f8100ff466c6f67493332820244deadbeef82024082079f8200187b82001901c8ff8202584064336534356634336431323135303164383763666138386536663038663462393233386531363532613261353336343835613936636663383864333466633130ff', 'hex'), 'got elembuf')
  const decoded = Types.decodeWasmArg(refResult)
  t.deepEquals(cbor.encode(decoded), cbor.encode(outputElems), 'serialise elembuf')

  t.end()
})

tape('proxy', async t => {
  const client = new Client({ debug: true })
  try {
    await client.connect()
  } catch (e) {
    console.log(e)
    console.log('run local testnet for Client tests!')
    process.exit()
  }

  const proxyWasm = fs.readFileSync(`${__dirname}/../actors/proxy.wasm`)
  const proxyId = await client.newRoot(proxyWasm)
  t.equals(proxyId, 'root', 'got root id')

  const getterWasm = fs.readFileSync(`${__dirname}/../actors/getter.wasm`)
  const [output2] = await client.newAndCallResult('getter', getterWasm, 'get_data', [], crypto.randomBytes(32), {}, true)
  t.ok(!!output2 && output2.includes('DATA1\nDATA2\nDATA3'), 'got output with newAndCallResult from key 2')

  await client.new(null, getterWasm, undefined, {}, true)
  const [output3] = await client.callResult(null, 'get_data', [], crypto.randomBytes(32), {}, true)
  t.ok(!!output3 && output3.includes('DATA1\nDATA2\nDATA3'), 'got output with callResult from key 3')

  t.end()
})
