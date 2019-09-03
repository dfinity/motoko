const crypto = require('crypto')
const EdDSA = require('elliptic').eddsa
const ec = new EdDSA('ed25519')
const cbor = require('borc')

const {
  ActorNewMessage,
  ActorCallMessage,
  ActorNewAndCallMessage,
  NewRootActorMessage,
  UserProcess,
  Payment,
  Transaction,
  parseWasmArgs
} = require('./Types')

/**
 * This is a wrapper class for creating and signing Dfinity Transactions.
 * The 'make' functions should be used to sign and wrap ingress messages.
 */
module.exports = class DfinityTx {
  constructor (opts = {}) {
    this.transaction = opts.transaction
    this.txHash = opts.txHash
    this.publicKey = opts.publicKey
    this.signature = opts.signature
  }

  /**
   * Make a Transaction containing an ActorNewMessage
   * @param {Buffer} [wasm] - wasm Module
   * @param {Buffer} [secretKey] - ed25519 secret key to sign ingress message with
   * @param {Object} [opts] - additional options to pass to Payment or Transaction
   * @return {DfinityTx} an unsigned DfinityTx containing the signed Payment
   */
  static fromNewMessage (wasm, secretKey, opts = {}) {
    return DfinityTx.from(DfinityTx.makeNewMessage(wasm, secretKey, opts), opts)
  }

  /**
   * Make a Transaction containing an NewRootActorMessage
   * @param {Buffer} [wasm] - wasm Module
   * @param {Buffer} [secretKey] - ed25519 secret key to sign ingress message with
   * @param {Object} [opts] - additional options to pass to Payment or Transaction
   * @return {DfinityTx} an unsigned DfinityTx containing the signed Payment
   */
  static fromNewRootActorMessage (wasm, secretKey, opts = {}) {
    return DfinityTx.from(DfinityTx.makeNewRootActorMessage(wasm, secretKey, opts), opts)
  }

  /**
   * Make a Transaction containing an ActorCallMessage
   * @param {String} [actorId] - actor ID
   * @param {String} [funcName] - name of exported function to call
   * @param {Array}  [wasmArgs] - function arguments
   * @param {Buffer} [secretKey] - ed25519 secret key to sign ingress message with
   * @param {Object} [opts] - additional options to pass to Payment or Transaction
   * @return {DfinityTx} an unsigned DfinityTx containing the signed Payment
   */
  static fromCallMessage (actorId, funcName, wasmArgs = [], secretKey, opts = {}) {
    return DfinityTx.from(DfinityTx.makeCallMessage(actorId, funcName, wasmArgs, secretKey, opts), opts)
  }

  /**
   * Make a Transaction containing an ActorNewAndCallMessage
   * @param {Buffer} [wasm] - wasm Module
   * @param {String} [funcName] - name of exported function to call
   * @param {Array}  [wasmArgs] - function arguments
   * @param {Buffer} [secretKey] - ed25519 secret key to sign ingress message with
   * @param {Object} [opts] - additional options to pass to Payment or Transaction
   * @return {DfinityTx} an unsigned DfinityTx containing the signed Payment
   */
  static fromNewAndCallMessage (wasm, funcName, wasmArgs = [], secretKey, opts = {}) {
    return DfinityTx.from(DfinityTx.makeNewAndCallMessage(wasm, funcName, wasmArgs, secretKey, opts), opts)
  }

  /**
   * Make a Payment containing an ActorNewMessage
   * @param {Buffer} [wasm] - wasm Module
   * @param {Buffer} [secretKey] - ed25519 secret key to sign ingress message with
   * @param {Object} [opts] - additional options to pass to Payment
   * @return {Payment} a Payment containing the signed ingress message
   */
  static makeNewMessage (wasm, secretKey, opts = {}) {
    const vmMessage = new ActorNewMessage({ _wasm: wasm })

    const ingressMessage = DfinityTx._sign(
      [
        new UserProcess(),
        cbor.encode(vmMessage)
      ],
      secretKey
    )

    return new Payment({
      ...opts,
      _wasm: wasm,
      _paymentMessage: [ingressMessage]
    })
  }

  /**
   * Make a Payment containing an NewRootActorMessage
   * @param {Buffer} [wasm] - wasm Module
   * @param {Buffer} [secretKey] - ed25519 secret key to sign ingress message with
   * @param {Object} [opts] - additional options to pass to Payment or Transaction
   * @return {Payment} a Payment containing the signed ingress message
   */
  static makeNewRootActorMessage (wasm, secretKey, opts = {}) {
    const vmMessage = new NewRootActorMessage({ _wasm: wasm })

    const ingressMessage = DfinityTx._sign(
      [
        new UserProcess(),
        cbor.encode(vmMessage)
      ],
      secretKey
    )

    return new Payment({
      ...opts,
      _wasm: wasm,
      _paymentMessage: [ingressMessage]
    })
  }

  /**
   * Make a Payment containing a ActorCallMessage
   * @param {String} [actorId] - actor ID
   * @param {String} [funcName] - name of exported function to call
   * @param {Array}  [wasmArgs] - function arguments
   * @param {Buffer} [secretKey] - ed25519 secret key to sign ingress message with
   * @param {Object} [opts] - additional options to pass to Payment
   * @return {DfinityTx} an unsigned DfinityTx containing the signed Payment
   */
  static makeCallMessage (actorId, funcName, wasmArgs = [], secretKey, opts = {}) {
    const _wasmArgs = parseWasmArgs(wasmArgs)

    const vmMessage = new ActorCallMessage({
      _actorId: actorId,
      _funcName: funcName,
      _wasmArgs
    })

    const ingressMessage = DfinityTx._sign(
      [
        new UserProcess(),
        cbor.encode(vmMessage)
      ],
      secretKey
    )

    return new Payment({
      ...opts,
      _actorId: actorId,
      _funcName: funcName,
      _wasmArgs,
      _paymentMessage: [ingressMessage]
    })
  }

  /**
   * Make a Payment containing a ActorNewAndCallMessage
   * @param {Buffer} [wasm] - wasm Module
   * @param {String} [funcName] - name of exported function to call
   * @param {Array}  [wasmArgs] - function arguments
   * @param {Buffer} [secretKey] - ed25519 secret key to sign ingress message with
   * @param {Object} [opts] - additional options to pass to Payment
   * @return {DfinityTx} an unsigned DfinityTx containing the signed Payment
   */
  static makeNewAndCallMessage (wasm, funcName, wasmArgs = [], secretKey, opts = {}) {
    const _wasmArgs = parseWasmArgs(wasmArgs)

    const vmMessage = new ActorNewAndCallMessage({
      _wasm: wasm,
      _funcName: funcName,
      _wasmArgs
    })

    const ingressMessage = DfinityTx._sign(
      [
        new UserProcess(),
        cbor.encode(vmMessage)
      ],
      secretKey
    )

    return new Payment({
      ...opts,
      _wasm: wasm,
      _funcName: funcName,
      _wasmArgs,
      _paymentMessage: [ingressMessage]
    })
  }

  /**
   * Make a Transaction from one or more Payments
   * @param {Array<Payment>} payment(s)
   * @param {Object} [opts] - additional options to pass to Transaction
   * @return {DfinityTx} an unsigned DfinityTx containing the Payments
   */
  static from (payments = [], opts = {}) {
    return new DfinityTx({
      transaction: new Transaction({
        ...opts,
        _txPayments: payments
      })
    })
  }

  /**
   * Add a Payment to an existing Transaction
   * @param {Payment} payment
   * @return {DfinityTx} this DfinityTx with the updated payments array
   */
  addMessage (payment) {
    this.transaction._txPayments = this.transaction._txPayments.concat([payment])
    return this
  }

  /**
   * Sign the transaction using the Ed25519 secret key
   * @param {Buffer} secretKey - a 32 bytes buffer to use as a secret key
   * @return {Buffer} serialized array of `[ transaction, publicKey, signature ]`
   */
  sign (secretKey) {
    const txHash = DfinityTx._hash(cbor.encode(this.transaction))
    const [, publicKey, signature] = DfinityTx._sign(this.transaction, secretKey)
    this.txHash = txHash
    this.publicKey = publicKey
    this.signature = signature
    return this.serialise()
  }

  /**
   * Sign the data using the Ed25519 secret key
   * @param {Buffer} data - the data to sign
   * @param {Buffer} secretKey - a 32 bytes buffer to use as a secret key
   * @return {Array} array of `[ data, publicKey, signature ]`
   */
  static _sign (data, secretKey) {
    const hashed = DfinityTx._hash(cbor.encode(data))
    const key = ec.keyFromSecret(secretKey.toString('hex'))
    const signature = key.sign(hashed)
    return [
      data,
      Buffer.from(key.getPublic()),
      Buffer.from(signature.toBytes())
    ]
  }

  /**
   * Verify the signature of a serialized and signed DfinityTx message.
   * @return {Bool}
   */
  static verify (raw) {
    const tx = DfinityTx.decode(raw)
    return ec.verify(tx.txHash.toString('hex'), tx.signature.toString('hex'), tx.publicKey.toString('hex'))
  }

  /**
   * Deserialise the transaction
   * @param {Buffer} raw - the serialized signed transaction
   * @return {DfinityTx} a new instance of `DfinityTx`
   */
  static decode (raw) {
    const [decoded, publicKey, signature] = cbor.decodeFirst(raw)
    const [, _accountId, _nonce, _payments, _version, _txExpiresAt] = decoded
    const _txPayments = _payments.map(p => new Payment({
      _paymentMessage: [p[1][1]],
      _paymentTicks: p[2][1],
      _paymentTicksPrice: p[3][1]
    }))

    const transaction = new Transaction({
      _txAccountId: _accountId[1],
      _txNonce: _nonce[1],
      _txPayments,
      _txVersion: _version[1],
      _txExpiresAt
    })

    const txHash = DfinityTx._hash(cbor.encode(transaction))

    return new DfinityTx({
      txHash,
      publicKey,
      signature,
      transaction
    })
  }

  /**
   * Gets the SHA-256 hash of the serialized transaction
   * @param {number} length - the number of bytes of the hash to return. must be <= 32
   * @returns {Buffer} the hashed tx
   */
  hash (length = 32) {
    return DfinityTx._hash(cbor.encode(this), length)
  }

  /**
   * Gets the SHA-256 hash of the serialized signed transaction
   * @param {number} length - the number of bytes of the hash to return. must be <= 32
   * @returns {Buffer} the hashed signed tx
   */
  signedHash (length = 32) {
    if (!this.isSigned()) {
      throw Error('not signed')
    }
    return DfinityTx._hash(this.serialise(), length)
  }

  /**
   * Gets the SHA-256 hash of the data
   * @param {number} length - the number of bytes of the hash to return. must be <= 32
   * @returns {Buffer} the hash
   */
  static _hash (data, length = 32) {
    const hash = crypto.createHash('sha256')
    hash.update(data)
    return hash.digest().slice(0, length)
  }

  encodeCBOR (gen) {
    return gen.pushAny(this.transaction)
  }

  /**
   * Returns true if the transaction is signed
   * @returns {boolean}
   */
  isSigned () {
    return this.publicKey != null && this.signature != null
  }

  /**
   * Serialises the signed or unsigned transaction
   * @return {Buffer} serialized array of `[ transaction, publicKey, signature ]` if signed or just the transaction if not.
   */
  serialise () {
    return cbor.encode(this.isSigned() ? [
      this,
      this.publicKey,
      this.signature
    ] : this)
  }
}
