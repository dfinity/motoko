const cbor = require('borc')
const constants = require('borc/src/constants')

const hexToBuffer = b => typeof b === 'string' ? Buffer.from(b.replace(/^0x/, ''), 'hex') : Buffer.from(b)

const type = o => Object.prototype.toString.call(o).slice(8, -1)

// CBOR wrappers
class Synonym {
  constructor (val) {
    this.__val = val
  }

  encodeCBOR (gen) {
    return gen.pushAny(this.__val)
  }
}

class BufferSynonym extends Synonym {
  constructor (val) {
    super(Buffer.from(val))
  }
}

class TaggedArray {
  constructor (tag, val = []) {
    this.__tag = tag
    this.__val = val
  }

  encodeCBOR (gen) {
    const arr = type(this.__val) === 'Array' ? this.__val : [this.__val]
    const data = [this.__tag, ...arr]
    return gen.pushAny(data)
  }
}

class IndefiniteLengthArray extends Array {
  constructor (val, ...rest) {
    if (val === undefined) {
      super()
    } else if (Array.isArray(val)) {
      super(...val)
    } else {
      super(val, ...rest)
    }
  }

  // borc uses Object.prototype.toString to determine type, so override this to use our encodeCBOR method
  get [Symbol.toStringTag] () {
    return 'Object'
  }

  encodeCBOR (gen) {
    if (!gen._pushUInt8(constants.MT.ARRAY << 5 | constants.NUMBYTES.INDEFINITE)) {
      return false
    }
    for (let [, i] of this.entries()) {
      if (!gen.pushAny(i)) {
        return false
      }
    }
    return gen._pushUInt8(0xff)
  }
}

const wrap = (val, Cls) => val !== null && typeof val === 'object' && val.constructor.name === Cls.name ? val : new Cls(val)

class Record extends TaggedArray {
  constructor (tag, vals = {}) {
    super(tag)

    this.__vals = {}
    const defaults = this.constructor.defaults
    const merged = Object.assign(defaults, vals)

    this.constructor.fields.forEach((cls, key) => {
      Object.defineProperty(this, key, {
        get: function () {
          return this.__vals[key]
        },
        set: function (val) {
          if (cls != null) {
            this.__vals[key] = wrap(val, cls)
          } else {
            this.__vals[key] = val
          }
        }
      })

      this[key] = merged[key]
    })
    Object.assign(this, vals)
  }

  encodeCBOR (gen) {
    this.__val = Array.from(this.constructor.fields.keys()).map(f => this.__vals[f])
    return super.encodeCBOR(gen)
  }

  static get fields () {
    return new Map()
  }

  static get defaults () {
    return {}
  }
}

// VM
class ActorNewMessage extends Record {
  constructor (val) { super(0, val) }
  static get fields () {
    return new Map([
      [ '_wasm', WasmModule ]
    ])
  }
  static get defaults () {
    return {
      _wasm: []
    }
  }
}

class ActorCallMessage extends Record {
  constructor (val) { super(1, val) }
  static get fields () {
    return new Map([
      [ '_actorId', ActorID ],
      [ '_funcName', FunctionName ],
      [ '_wasmArgs', IndefiniteLengthArray ]
    ])
  }
  static get defaults () {
    return {
      _actorId: '',
      _funcName: '',
      _wasmArgs: []
    }
  }
}

class ActorNewAndCallMessage extends Record {
  constructor (val) { super(2, val) }
  static get fields () {
    return new Map([
      [ '_wasm', WasmModule ],
      [ '_funcName', FunctionName ],
      [ '_wasmArgs', IndefiniteLengthArray ]
    ])
  }
  static get defaults () {
    return {
      _wasm: [],
      _funcName: '',
      _wasmArgs: []
    }
  }
}

class NewRootActorMessage extends Record {
  constructor (val) { super(3, val) }
  static get fields () {
    return new Map([
      [ '_wasm', WasmModule ]
    ])
  }
  static get defaults () {
    return {
      _wasm: []
    }
  }
}

// ActorID
class ActorID {
  constructor (val) {
    if (typeof val === 'object') {
      switch (val.constructor.name) {
        case 'RootActor':
        case 'RegularActor':
          return val
      }
    } else {
      switch (val) {
        case 'root':
          return new RootActor()
        default:
          return new RegularActor(Buffer.from(val))
      }
    }
  }
}
class RootActor extends TaggedArray { constructor () { super(0) } }
class RegularActor extends TaggedArray { constructor (val) { super(1, val) } }

class ModuleID extends BufferSynonym {}
class WasmModule extends BufferSynonym {}
class FunctionName extends BufferSynonym {}

// WasmArgType
class WasmI32Type extends TaggedArray { constructor () { super(0) } }
class WasmI64Type extends TaggedArray { constructor () { super(1) } }
class DataBufType extends TaggedArray { constructor () { super(2) } }
class ModuleRefType extends TaggedArray { constructor () { super(3) } }
class ActorRefType extends TaggedArray { constructor () { super(4) } }
class FuncRefType extends TaggedArray { constructor () { super(5) } }
class ElemBufType extends TaggedArray { constructor () { super(6) } }
class AnyRefType extends TaggedArray { constructor () { super(7) } }

// WasmArg
class WasmI32 extends TaggedArray { constructor (val) { super(0, val) } }
class WasmI64 extends TaggedArray { constructor (val) { super(1, val) } }
class DataBuf extends TaggedArray { constructor (val) { super(2, val) } }
class ModuleRef extends TaggedArray { constructor (val) { super(3, val) } }
class ActorRef extends TaggedArray { constructor (val) { super(4, val) } }
class ActorConsole extends TaggedArray { constructor () { super(5) } }
class FuncRef extends Record {
  constructor (val) { super(6, val) }
  static get fields () {
    return new Map([
      [ '_actorId', ActorID ],
      [ '_wasmArgTypes', IndefiniteLengthArray ],
      [ '_funcName', FunctionName ]
    ])
  }
  static get defaults () {
    return {
      _actorId: new RootActor(),
      _wasmArgTypes: [],
      _funcName: ''
    }
  }
}
class ElemBuf extends TaggedArray { constructor (val) { super(7, new IndefiniteLengthArray(val)) } }
class ConsoleLog extends TaggedArray { constructor () { super(8) } }
class UserIdentity extends TaggedArray { constructor () { super(9) } }

// Types
class UserId extends Synonym {}
class UserSecret extends Synonym {}
class SignedTransaction extends Synonym {}
class RawSignedTransaction extends TaggedArray { constructor (val) { super(0, val) } }
class AccountId extends TaggedArray { constructor (val) { super(0, val) } }
class Nonce extends TaggedArray { constructor (val) { super(0, val) } }
class IngressMessage extends TaggedArray { constructor (val) { super(0, val) } }
class ProcessId extends TaggedArray { constructor (val) { super(0, val) } }
class UserProcess extends TaggedArray { constructor (val) { super(1, val) } }
class Ticks extends TaggedArray { constructor (val) { super(0, val) } }
class TicksPrice extends TaggedArray { constructor (val) { super(0, val) } }
class Version extends TaggedArray { constructor (val) { super(0, val) } }
class Height extends Synonym {}

class Transaction extends Record {
  constructor (vals) {
    super(0, vals)
  }

  static get fields () {
    return new Map([
      [ '_txAccountId', AccountId ],
      [ '_txNonce', Nonce ],
      [ '_txPayments', IndefiniteLengthArray ],
      [ '_txVersion', Version ],
      [ '_txExpiresAt', Height ]
    ])
  }

  static get defaults () {
    return {
      _txAccountId: new AccountId(0),
      _txNonce: new Nonce(0),
      _txPayments: new IndefiniteLengthArray(),
      _txVersion: new Version(0),
      _txExpiresAt: new Height(0)
    }
  }
}

class Payment extends Record {
  constructor (vals) {
    super(0, vals)
  }

  static get fields () {
    return new Map([
      [ '_paymentMessage', IngressMessage ],
      [ '_paymentTicks', Ticks ],
      [ '_paymentTicksPrice', TicksPrice ]
    ])
  }

  static get defaults () {
    return {
      _paymentMessage: new IngressMessage(Buffer.from([])),
      _paymentTicks: new Ticks(0),
      _paymentTicksPrice: new TicksPrice(0)
    }
  }
}

const I32_MAX = Math.pow(2, 31) - 1
const I32_MIN = -Math.pow(2, 31)
const consoleActorName = 'console_actor'
const consoleModuleName = 'console_module'
const defaultConsoleName = 'default_console'

const parseWasmArgs = args => args.map(a => {
  if (Number.isInteger(a)) {
    if (a >= I32_MIN && a <= I32_MAX) {
      return new WasmI32(a)
    } else {
      return new WasmI64(a)
    }
  } else if (typeof a === 'string') {
    switch (a) {
      case consoleActorName:
        return new ActorConsole()
      case consoleModuleName:
        return new ModuleRef(Buffer.from(a))
      case defaultConsoleName:
        return new ActorRef(new RegularActor(Buffer.from(a)))
      default:
        if (a.startsWith('0x')) {
          return new DataBuf(hexToBuffer(a))
        } else {
          return new DataBuf(Buffer.from(a))
        }
    }
  } else if (Buffer.isBuffer(a)) {
    return new DataBuf(a)
  } else if (Array.isArray(a)) {
    return new ElemBuf(parseWasmArgs(a))
  } else if (typeof a === 'object') {
    switch (a.constructor.name) {
      case 'WasmI32':
      case 'WasmI64':
      case 'DataBuf':
      case 'ModuleRef':
      case 'ActorRef':
      case 'ActorConsole':
      case 'ConsoleLog':
      case 'FuncRef':
      case 'ElemBuf':
      case 'UserIdentity':
        return a
    }
  }

  console.log(a)
  throw Error('invalid argument type: ' + a)
})

const txExpiryNear = h => h + 16

const decodeWasmArg = arg => {
  const decoded = cbor.decodeFirst(arg)

  const coerceActorID = a => {
    if (!Array.isArray(a)) {
      throw Error('invalid ActorID: ', a)
    }

    switch (a[0]) {
      case 0: return new RootActor()
      case 1: return new RegularActor(a[1])
    }

    throw Error('invalid ActorID: ', a)
  }

  const coerceArgType = a => {
    if (!Array.isArray(a)) {
      throw Error('invalid WasmArgType: ', a)
    }

    switch (a[0]) {
      case 0: return new WasmI32Type()
      case 1: return new WasmI64Type()
      case 2: return new DataBufType()
      case 3: return new ModuleRefType()
      case 4: return new ActorRefType()
      case 5: return new FuncRefType()
      case 6: return new ElemBufType()
      case 7: return new AnyRefType()
    }

    throw Error('invalid WasmArgType: ', a)
  }

  const coerceArg = a => {
    if (!Array.isArray(a)) {
      throw Error('invalid WasmArg: ', a)
    }

    switch (a[0]) {
      case 0: return new WasmI32(a[1])
      case 1: return new WasmI64(a[1])
      case 2: return new DataBuf(a[1])
      case 3: return new ModuleRef(a[1])
      case 4: return new ActorRef(coerceActorID(a[1]))
      case 5: return new ActorConsole()
      case 6: return new FuncRef({ _actorId: coerceActorID(a[1]), _wasmArgTypes: a[2].map(coerceArgType), _funcName: a[3] })
      case 7: return new ElemBuf(a[1].map(coerceArg))
      case 8: return new ConsoleLog()
      case 9: return new UserIdentity()
    }

    throw Error('invalid WasmArg: ', a)
  }

  return coerceArg(decoded)
}

module.exports = {
  Record,
  IndefiniteLengthArray,
  Synonym,
  TaggedArray,
  wrap,
  hexToBuffer,
  type,

  ActorNewMessage,
  ActorCallMessage,
  ActorNewAndCallMessage,
  NewRootActorMessage,
  ActorID,
  RootActor,
  RegularActor,
  ModuleID,
  FunctionName,
  WasmModule,
  WasmI32Type,
  WasmI64Type,
  DataBufType,
  ModuleRefType,
  ActorRefType,
  FuncRefType,
  ElemBufType,
  AnyRefType,
  WasmI32,
  WasmI64,
  DataBuf,
  ModuleRef,
  ActorRef,
  ActorConsole,
  ConsoleLog,
  FuncRef,
  ElemBuf,
  UserIdentity,
  UserId,
  UserSecret,
  SignedTransaction,
  RawSignedTransaction,
  Transaction,
  AccountId,
  Nonce,
  Payment,
  IngressMessage,
  ProcessId,
  UserProcess,
  Ticks,
  TicksPrice,
  Version,
  Height,

  consoleActorName,
  consoleModuleName,
  defaultConsoleName,
  parseWasmArgs,
  txExpiryNear,
  decodeWasmArg
}
