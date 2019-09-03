const tape = require('tape')
const { IDL, Types } = require('../../')

const testEncode = (tape, typ, val, hex, str) => {
  tape.deepEquals(typ.encode(val), Buffer.from(hex, 'hex'), `Encode ${str}`)
}

const testDecode = (tape, typ, val, hex, str) => {
  tape.deepEquals(typ.decode(Buffer.from(hex, 'hex')), val, `Decode ${str}`)
}

const test = (tape, typ, val, hex, str) => {
  testEncode(tape, typ, val, hex, str)
  testDecode(tape, typ, val, hex, str)
}

tape('IDL hash', t => {
  const testHash = (string, hash) => {
    t.equals(IDL.idlHash(string), hash, `IDL Hash of ${string}`)
  }

  testHash("", 0)
  testHash("id", 23515)
  testHash("description", 1595738364)
  testHash("short_name", 3261810734)
  testHash("Hi ☃", 1419229646)

  t.end()
})

tape('IDL encoding', t => {
  // Wrong magic number
  t.throws(() => IDL.Nat.decode(Buffer.from('2a')), /Message length smaller than magic number/, 'No magic number')
  t.throws(() => IDL.Nat.decode(Buffer.from('4449444d2a')), /Wrong magic number:/, 'Wrong magic number')
  
  // None
  t.throws(() => IDL.None.encode(), /None cannot appear as a function argument/, 'None cannot appear as a function argument')
  t.throws(() => IDL.None.decode(Buffer.from('DIDL')), /None cannot appear as an output/, 'None cannot appear as an output')

  // Unit
  test(t, IDL.Unit, null, '4449444c007f', 'Unit value')

  // Text
  test(t, IDL.Text, 'Hi ☃\n', '486920e298830a', 'Text with unicode')
  test(t, IDL.Opt(IDL.Text), 'Hi ☃\n', '4449444c016e71000107486920e298830a', 'Nested text with unicode')
  t.throws(() => IDL.Text.encode(0), /Invalid Text argument/, 'Wrong Text type')
  t.throws(() => IDL.Text.encode(null), /Invalid Text argument/, 'Wrong Text type')

  // Int
  test(t, IDL.Int, 42, '4449444c007c2a', 'Int')
  test(t, IDL.Int, 1234567890, '4449444c007cd285d8cc04', 'Positive Int')
  test(t, IDL.Int, -1234567890, '4449444c007caefaa7b37b', 'Negative Int')
  
  // Nat
  test(t, IDL.Nat, 42, '4449444c007d2a', 'Nat')
  test(t, IDL.Nat, 1234567890, '4449444c007dd285d8cc04', 'Positive Nat')
  t.throws(() => IDL.Nat.encode(-1), /Invalid Nat argument/, 'Wrong Negative Nat')
  
  // Tuple
  test(t, IDL.Tuple(IDL.Int, IDL.Text), [42, '💩'], '4449444c016c02007c0171002a04f09f92a9', 'Pairs')
  t.throws(() => IDL.Tuple(IDL.Int, IDL.Text).encode([0]), /Tuple argument has wrong length/, 'Wrong Tuple length')

  // Array
  test(t, IDL.Arr(IDL.Int), [0, 1, 2, 3], '4449444c016d7c000400010203', 'Array of Ints')
  t.throws(() => IDL.Arr(IDL.Int).encode(0), /Invalid Arr argument/, 'Wrong Array type')
  t.throws(() => IDL.Arr(IDL.Int).encode(['fail']), /Invalid Int argument/, 'Wrong Array type')

  // Array of Tuple
  test(t, IDL.Arr(IDL.Tuple(IDL.Int, IDL.Text)), [[42, 'text']], '4449444c026c02007c01716d0001012a0474657874', 'Arr of Tuple')

  // Nested Tuples
  test(t, IDL.Tuple(IDL.Tuple(IDL.Tuple(IDL.Tuple(IDL.Unit)))), [[[[null]]]], '4449444c046c01007f6c0100006c0100016c01000203', 'Nested Tuples')

  // Object
  test(t, IDL.Obj({}), {}, '4449444c016c0000', 'Empty object')
  t.throws(() => IDL.Obj({a: IDL.Text}).encode({b: 'b'}), /Obj is missing key/, 'Obj is missing key')

  // UserIdentity
  t.deepEquals(IDL.DfinityUserId.encode(), new Types.UserIdentity(), 'UserIdentity')
  t.throws(() => IDL.Obj({a: IDL.DfinityUserId}).encode({a: null}), /DfinityUserId not supported in nested position/, 'DfinityUserId not supported in nested position')

  // Test that additional keys are ignored
  testEncode(t, IDL.Obj({foo: IDL.Text, bar: IDL.Int}), {foo: '💩', bar: 42, baz: 0}, '4449444c016c02d3e3aa027c868eb70271002a04f09f92a9', 'Object')
  testDecode(t, IDL.Obj({foo: IDL.Text, bar: IDL.Int}), {foo: '💩', bar: 42}, '4449444c016c02d3e3aa027c868eb70271002a04f09f92a9', 'Object')

  // Bool
  test(t, IDL.Bool, true, '4449444c007e01', 'true')
  test(t, IDL.Bool, false, '4449444c007e00', 'false')
  t.throws(() => IDL.Bool.encode(0), /Invalid Bool argument/, 'Wrong Bool type')
  t.throws(() => IDL.Bool.encode('false'), /Invalid Bool argument/, 'Wrong Bool type')

  // Variants
  const Result = IDL.Variant({ ok: IDL.Text, err: IDL.Text })
  test(t, Result, { ok: 'good' }, '4449444c016b029cc20171e58eb40271000004676f6f64', 'Result ok')
  test(t, Result, { err: 'uhoh' }, '4449444c016b029cc20171e58eb4027100010475686f68', 'Result err')
  t.throws(() => Result.encode({}), /Variant has no data/, 'Empty Variant')
  t.throws(() => Result.encode({ ok: 'ok', err: 'err' }), /Variant has extra key/, 'Invalid Variant')
  t.throws(() => Result.decode(Error('Call retailerQueryAll exception: Uncaught RuntimeError: memory access out of bounds')), /Uncaught RuntimeError/, 'Decode error')

  // Test that nullary constructors work as expected
  test(t, IDL.Variant({ foo: IDL.Unit }), { foo: null }, '4449444c016b01868eb7027f0000', 'Nullary constructor in variant')

  // Test that None within variants works as expected
  test(t, IDL.Variant({ ok: IDL.Text, err: IDL.None }), { ok: 'good' }, '4449444c016b029cc20171e58eb4026f000004676f6f64', 'None within variants')
  t.throws(() => IDL.Variant({ ok: IDL.Text, err: IDL.None }).encode({ err: 'uhoh' }), /None cannot appear as a function argument/, 'None cannot appear as a function argument')

  // Test for option
  test(t, IDL.Opt(IDL.Nat), null, '4449444c016e7d0000', 'Null option')
  test(t, IDL.Opt(IDL.Nat), 1, '4449444c016e7d000101', 'Non-null option')

  // Type description sharing
  test(t, IDL.Tuple(IDL.Arr(IDL.Int), IDL.Arr(IDL.Nat), IDL.Arr(IDL.Int), IDL.Arr(IDL.Nat)), [[],[],[],[]], '4449444c036d7c6d7d6c0400000101020003010200000000', 'Type sharing')

  // Test for recursive types
  const List = IDL.Rec()
  t.throws(() => List.encode(null), /Recursive type uninitialized/, 'Uninitialized recursion')
  List.fill(IDL.Opt(IDL.Obj({head: IDL.Int, tail: List})))
  test(t, List, null, '4449444c026e016c02a0d2aca8047c90eddae704000000', 'Empty list')
  test(t, List, {head: 1, tail: {head: 2, tail: null} }, '4449444c026e016c02a0d2aca8047c90eddae70400000101010200', 'List')

  // Mutual recursion
  const List1 = IDL.Rec()
  const List2 = IDL.Rec()
  List1.fill(IDL.Opt(List2))
  List2.fill(IDL.Obj({head: IDL.Int, tail: List1}))
  test(t, List1, null, '4449444c026e016c02a0d2aca8047c90eddae704000000', 'Empty list')
  test(t, List1, {head: 1, tail: {head: 2, tail: null} }, '4449444c026e016c02a0d2aca8047c90eddae70400000101010200', 'List')  

  t.end()
})
