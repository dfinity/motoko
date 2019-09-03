const cbor = require('borc')
const EdDSA = require('elliptic').eddsa
const ec = new EdDSA('ed25519')
const tape = require('tape')
const { DfinityTx, Types } = require('../../')

tape('transaction', async t => {
  const expectedRaw = Buffer.from('8386008200008200009f84008200838281014b8200480061736d010000005820b9d9acc1828fe5c40298fb324de3e1b972ab598dc3838f9bfaafd5f713eeaf7258404afa61341c94da1bfcbe48107ebfaf86b6c766d2877752fe116c3d8dbf2c069b4737810f7d7b9be77609da65eada93a0fa600ad8440deba05975199748ffd105820000820000ff820000105820b9d9acc1828fe5c40298fb324de3e1b972ab598dc3838f9bfaafd5f713eeaf725840f58dc7506c646c9e52b5283ba251f126bc81872352f2d3d458e1d74f1fc63946b3debeccc5f1bb6017ba4a902718b6aabb3658cd8cfd67eb0cd520476d8d2c0e', 'hex')
  const expectedTxHash = Buffer.from('4af311c4da8bc7ad88ea0cdb9be9050e6d98020a4412f1849e88363bb0a85b6c', 'hex')
  const expectedSignedHash = Buffer.from('0f431da1ea966fd15308b63dd97b8dac61926800e65018a13715c5ff6cceb20f', 'hex')

  const decodedTx = DfinityTx.decode(expectedRaw)
  t.deepEquals(decodedTx.serialise(), expectedRaw, 'serialise')
  t.deepEquals(decodedTx.txHash, expectedTxHash, 'hash raw')
  t.deepEquals(decodedTx.hash(), decodedTx.txHash, 'hash')
  t.deepEquals(decodedTx.signedHash(), expectedSignedHash, 'signed hash')
  t.equals(DfinityTx.verify(expectedRaw), true, 'verify')

  const sk = Buffer.from('9f445367456d5619314a42a3da86b001947afe599c2ba99c79a6347e3b226e5a', 'hex')
  const pk = Buffer.from(ec.keyFromSecret(sk).getPublic())
  const newTx = DfinityTx.fromNewMessage(Buffer.from('0061736d01000000', 'hex'), sk, { _txExpiresAt: 16 })
  const newPayment = newTx.transaction._txPayments[0]
  t.deepEquals(cbor.encode(newPayment), cbor.encode(decodedTx.transaction._txPayments[0]), 'directly serialize payment')
  t.deepEquals(cbor.encode(newTx), cbor.encode(decodedTx.transaction), 'directly serialize tx')

  const newImsg = newPayment.__vals._paymentMessage
  t.equals(newImsg.__val[0][0][0].__tag, 1, 'data: UserProcess')
  t.equals(newPayment._paymentTicks.__val, 0, 'data: _paymentTicks')
  t.equals(newPayment._paymentTicksPrice.__val, 0, 'data: _paymentTicksPrice')
  t.equals(newTx.transaction._txAccountId.__val, 0, 'data: _txAccountId')
  t.equals(newTx.transaction._txNonce.__val, 0, 'data: _txNonce')
  t.equals(newTx.transaction._txVersion.__val, 0, 'data: _txVersion')
  t.equals(newTx.transaction._txExpiresAt.__val, 16, 'data: _txExpiresAt')

  const signed = newTx.sign(sk)
  t.equals(DfinityTx.verify(signed), true, 'verify new signed tx')
  t.deepEquals(signed, expectedRaw, 'serialise new signed tx')
  const tx2 = DfinityTx.decode(signed)
  t.deepEqual(tx2.publicKey, pk, 'decode public key')
  t.deepEquals(tx2.serialise(), signed, 'serialize 2')

  t.end()
})

const wasmArgs = Types.parseWasmArgs([
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

tape('ingress msg', async t => {
  const raw = Buffer.from('8386008200008200009f8400820083828101587984018201413141619f82001a7fffffff82011b001fffffffffffff82034e636f6e736f6c655f6d6f64756c658105820482014f64656661756c745f636f6e736f6c65840682014d636f6e736f6c655f6163746f729f8100ff466c6f67493332820244deadbeef82024082079f8200187b82001901c8ff8109ff5820b9d9acc1828fe5c40298fb324de3e1b972ab598dc3838f9bfaafd5f713eeaf725840d93eb86969c8b268e4118688f40ab496f43e4a407b4db288fb7a6bfc4404d93bb5f09391ca161157e411f131a0b32f32b4afac7947a74e19ec49210222debf08820000820000ff820000165820b9d9acc1828fe5c40298fb324de3e1b972ab598dc3838f9bfaafd5f713eeaf725840b80a3cc6f7e2afc9c6324bd9daa8952af435a19a4eccfd7fe6e589e685fd995d7cb5382cb96bad2135d4e7a4e76a7f8b4122385a0795e33cfd9dc703036b7606', 'hex')
  const vmMessage = new Types.ActorCallMessage({
    _actorId: '1',
    _funcName: 'a',
    _wasmArgs: wasmArgs
  })

  const decodedTx = DfinityTx.decode(raw)
  const ingressMsg = decodedTx.transaction._txPayments[0]._paymentMessage.__val[0][0][1]
  t.deepEquals(cbor.encode(vmMessage), ingressMsg, 'serialise vmMessage')
  t.deepEquals(decodedTx.serialise(), raw, 'serialise tx')
  t.end()
})

tape('multiple payments', async t => {
  const raw = Buffer.from('8386008200008200009f840082008382810141305820b9d9acc1828fe5c40298fb324de3e1b972ab598dc3838f9bfaafd5f713eeaf72584017c19c54e7093e93c70e2329cb80729db95ee6c1a79aa2d4f5546212dec6c6be5ba432c38828ea559a945647fb31c004bb3857f601dbcc1b18ce4cf2f6e25605820000820000840082008382810141315820b9d9acc1828fe5c40298fb324de3e1b972ab598dc3838f9bfaafd5f713eeaf7258400d02b3863fb7b622bee38ed5df6cbec5c3d1f6e4a0fb7cbf5b32660faf218200025406d599242051788c1c1d91044d199310b09a589d3837ed3ebbdfd89cb20d820000820000840082008382810141325820b9d9acc1828fe5c40298fb324de3e1b972ab598dc3838f9bfaafd5f713eeaf725840a7189c162c337169bfae959f9645717dee9a132fd72ed09c3228fa5743d27013e6766398e61d8fd1e37e1c4beb3ccaa5dd00d8c74b6f0dd4c85b8f65133ca501820000820000ff820000015820b9d9acc1828fe5c40298fb324de3e1b972ab598dc3838f9bfaafd5f713eeaf725840474ef1c63542673d27ceb67ee6754d06286966e78798617f299e1aacb5775c1c2c2df2a3e74e4cb7294a9cbc5771fbb087d44756d85fd284278d371ab2b47c0c', 'hex')

  const decodedTx = DfinityTx.decode(raw)
  const payments = decodedTx.transaction._txPayments
  t.equals(decodedTx.transaction._txPayments.length, 3, '3 payments')
  t.deepEquals(payments[0]._paymentMessage.__val[0][0][1], Buffer.from('0'), 'ingress msg 0 data')
  t.deepEquals(payments[1]._paymentMessage.__val[0][0][1], Buffer.from('1'), 'ingress msg 1 data')
  t.deepEquals(payments[2]._paymentMessage.__val[0][0][1], Buffer.from('2'), 'ingress msg 2 data')
  t.end()
})
