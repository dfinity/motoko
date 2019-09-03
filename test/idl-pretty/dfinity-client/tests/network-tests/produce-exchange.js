const crypto = require('crypto')
const tape = require('tape')
const fs = require('fs')
const { Client, IDL } = require('../../')

const UserId = IDL.Int
const RegionId = IDL.Int
const ReservedRouteId = IDL.Int
const RetailerId = IDL.Int
const RouteId = IDL.Int
const RegionInfo = IDL.Obj({
  id: RegionId,
  short_name: IDL.Text,
  description: IDL.Text
})
const ReservedRouteInfo = IDL.Obj({
  id: ReservedRouteId,
  retailer: RetailerId,
  route: RouteId
})
const pxInterface = new IDL.ActorInterface({
  registrarAddRegion: IDL.Message([IDL.Text, IDL.Text], IDL.Opt(RegionId)),
  allRegionInfo: IDL.Message(undefined, IDL.Arr(RegionInfo)),
  registrarAddUser: IDL.Message([
    IDL.DfinityUserId,
    IDL.Text,
    IDL.Text,
    RegionId,
    IDL.Bool,
    IDL.Bool,
    IDL.Bool,
    IDL.Bool
  ], IDL.Opt(UserId)),
  transporterAllReservationInfo: IDL.Message([IDL.DfinityUserId, UserId], IDL.Opt(IDL.Arr(ReservedRouteInfo)))
})

tape('produce-exchange', async t => {
  const secretKey = crypto.randomBytes(32)
  const client0 = new Client({ secretKey })
  try {
    await client0.connect()
  } catch (e) {
    console.log(e)
    console.log('run local testnet for Client tests!')
    process.exit()
  }

  const proxyWasm = fs.readFileSync(`${__dirname}/../actors/proxy.wasm`)
  await client0.newRoot(proxyWasm)

  const wasm = fs.readFileSync(`${__dirname}/../actors/ProduceExchange.wasm`)

  const count = 20
  const data = Array.from({length: count}).map((_, i) => ({ description: `Region ${i}`, id: i, short_name: String(i) }))
  const byId = (a, b) => a.id - b.id

  const px0 = await pxInterface.newProxied(client0, wasm, crypto.randomBytes(32))

  const ids = await px0.batch()(data.map(d => px0.registrarAddRegion(d.short_name, d.description)))
  const info = await px0.allRegionInfo()

  t.deepEquals(ids, data.map(d => d.id), `Add ${count} regions, expected ids`)
  t.deepEquals(info.sort(byId), data, `Add ${count} regions, expected data`)

  const userId0 = await px0.registrarAddUser(null, 'user0', '', 0, true, true, true, true)
  t.equal(userId0, 0, 'Add user 0')

  const userId0Reservations = await px0.transporterAllReservationInfo(null, userId0)
  t.deepEquals(userId0Reservations, [], 'User 0 reservations')

  const client1 = new Client({ secretKey: crypto.randomBytes(32) })
  await client1.connect()
  const px1 = await pxInterface.fromProxy(client1)
  const info2 = await px1.allRegionInfo()
  t.deepEquals(info2.sort(byId), data, 'New interface from proxy')

  const userId1 = await px1.registrarAddUser(null, 'user1', '', 0, true, true, true, false)
  t.equal(userId1, 1, 'Add user 1')

  try {
    await px1.transporterAllReservationInfo(null, userId0)
    t.fail('should have thrown')
  } catch (e) {
    t.ok(e.message.match(/unreachable/), 'signer 1 should fail with user id 0')
  }
  const fail1 = await px1.transporterAllReservationInfo(null, userId1)
  t.equal(fail1, null, 'user 1 is not a transporter')

  t.end()
})
