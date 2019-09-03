const crypto = require('crypto')
const fetch = require('isomorphic-fetch')

const DfinityTx = require('./DfinityTx')
const { RootActor, txExpiryNear, ConsoleLog, type } = require('./Types')

const ROOT_ACTOR = new RootActor()

/**
 * Submits a Transaction to a DFINITY node
 * @param {DfinityTx} [dfinityTx] - the signed `DfinityTx`
 * @param {string} [host] - host URL
 * @returns {Promise<Object>} promise with the output, or error
 */
const submit = (dfinityTx, host, opts) => new Promise(async (resolve, reject) => {
  if (!dfinityTx.isSigned()) {
    throw Error('tx is not signed')
  }

  try {
    const res = await fetch(`${host}/api/v1/run`, {
      body: JSON.stringify(dfinityTx.serialise().toString('hex')),
      method: 'POST',
      headers: { 'Content-Type': 'application/json' }
    })
    if (!res.ok) {
      if (opts.debug) { console.error(res.status, res.statusText) }
      reject(res)
    } else {
      const data = await res.json()
      if (data) {
        if (opts.debug) { console.log('run:', data) }
        resolve(data)
      } else {
        if (opts.debug) { console.log(res.status, res.statusText) }
        resolve(null)
      }
    }
  } catch (err) {
    if (opts.debug) { console.error(err.toString()) }
    reject(err)
  }
})

/**
 * Gets the current height
 * @param {string} [host] - host URL
 * @returns {Promise<response>} promise with the http response
 */
const getHeight = host =>
  fetch(`${host}/api/v1/height`, {
    method: 'GET',
    headers: { 'Content-Type': 'application/json' }
  })

/**
 * Gets the current state
 * @param {string} [host] - host URL
 * @returns {Promise<response>} promise with the http response
 */
const getState = host =>
  fetch(`${host}/api/v1/debug/state`, {
    method: 'GET',
    headers: { 'Content-Type': 'application/json' }
  })

/**
 * A DFINITY client that create actors, call functions, and wait for output from the console stream
 * @param {Object} [opts] - additional options
 * @param {boolean} [opts.debug] - log debug messages to console
 * @param {string} [opts.host] - host URL
 * @param {Map} [opts.actorIds] - map of actor names to ids
 * @param {Buffer} [opts.secretKey] - ed25519 secret key to sign messages/transactions with
 * @param {Number} [opts.nonce] - initial nonce
 * @param {Number} [opts.timeout] - how long to wait for a result (sec)
 */
class Client {
  constructor (opts = {}) {
    const defaults = this.constructor.defaults
    this.opts = Object.assign(defaults, opts)
    this.log = this.opts.debug ? console.log : Function.prototype
  }

  static get defaults () {
    return {
      debug: false,
      host: 'http://localhost:4200',
      actorIds: new Map(),
      secretKey: crypto.randomBytes(32),
      nonce: 0,
      timeout: 60
    }
  }

  /**
   * Test the network connection by getting the height
   * @returns {Promise<string>} promise with the height response
   */
  connect () {
    return getHeight(this.opts.host)
  }

  /**
   * Batch multiple payments into a transaction
   * @param {Array<Payment>} payments - array of payments
   * @return {Promise<Array<Buffer>>} promise with the results of each ingress message
   */
  async batch (payments, opts = {}) {
    const tx = DfinityTx.from(payments, opts)
    const data = await this.signAndSubmit(tx, opts)
    return this.formatOutput(data)
  }

  /**
   * Deploy a new root actor named "root", to be used as a proxy
   * @param {Buffer} wasm - wasm Module
   * @param {Object} [opts] - additional options to pass to Payment or Transaction
   * @returns {Promise<string>} promise with the new actor id
   */
  async newRoot (wasm, opts = {}) {
    const tx = DfinityTx.fromNewRootActorMessage(wasm, this.opts.secretKey, opts)
    const data = await this.signAndSubmit(tx, opts)
    return this.getNewActorId(data, 'root')
  }

  /**
   * Deploy a new actor
   * @param {string} actorName - local name to save actor as, which is _not_ sent to the network
   * @param {Buffer} wasm - wasm Module
   * @param {Buffer} [secretKey] - ed25519 secret key to sign Payment with
   * @param {Object} [opts] - additional options to pass to Payment or Transaction
   * @param {Boolean} [proxy] - use the proxy actor
   * @returns {Promise<string>} promise with the new actor id
   */
  async new (actorName, wasm, secretKey, opts = {}, proxy = false) {
    const tx = DfinityTx.from(this.makeNew(wasm, secretKey, opts, proxy))
    const data = await this.signAndSubmit(tx, opts)
    // The proxy actor currently doesn't log "createActor"; also, we don't need the actor ID
    if (proxy) { return data }
    return this.getNewActorId(data, actorName)
  }

  /**
   * Deploy a new actor and call a function. Appends a ConsoleLog to the args, and wait for the tx output
   * @param {string} actorName - local name to save actor as, which is _not_ sent to the network
   * @param {Buffer} wasm - wasm Module
   * @param {string} funcName - name of exported function to call
   * @param {Array}  [wasmArgs] - function arguments
   * @param {Buffer} [secretKey] - ed25519 secret key to sign Payment with
   * @param {Object} [opts] - additional options to pass to Payment or Transaction
   * @param {Boolean} [proxy] - use the proxy actor
   * @returns {Promise<string>} promise with the new actor id
   */
  async newAndCall (actorName, wasm, funcName, wasmArgs = [], secretKey, opts = {}, proxy = false) {
    const tx = DfinityTx.from(this.makeNewAndCall(wasm, funcName, wasmArgs, secretKey, opts, proxy))
    const data = await this.signAndSubmit(tx, opts)
    // The proxy actor currently doesn't log "createActor"; also, we don't need the actor ID
    if (proxy) { return data }
    return this.getNewActorId(data, actorName)
  }

  /**
   * Deploy a new actor, call a function, and return result. Appends a ConsoleLog to the args, and wait for the tx output
   * @param {string} actorName - local name to save actor as, which is _not_ sent to the network
   * @param {Buffer} wasm - wasm Module
   * @param {string} funcName - name of exported function to call
   * @param {Array}  [wasmArgs] - function arguments
   * @param {Buffer} [secretKey] - ed25519 secret key to sign Payment with
   * @param {Object} [opts] - additional options to pass to Payment or Transaction
   * @param {Boolean} [proxy] - use the proxy actor
   * @returns {Promise<Buffer>} promise that resolves with the call result, or rejects with an exception
   */
  async newAndCallResult (actorName, wasm, funcName, wasmArgs = [], secretKey, opts = {}, proxy = false) {
    const tx = DfinityTx.from(this.makeNewAndCallResult(wasm, funcName, wasmArgs, secretKey, opts, proxy))
    const data = await this.signAndSubmit(tx, opts)
    if (!proxy) { this.getNewActorId(data, actorName) }
    return this.formatOutput(data)
  }

  /**
   * Calls a function on an existing actor.
   * @param {string} actorId - actor ID
   * @param {string} funcName - name of exported function to call
   * @param {Array}  [wasmArgs] - function arguments
   * @param {Buffer} [secretKey] - ed25519 secret key to sign Payment with
   * @param {Object} [opts] - additional options to pass to Payment or Transaction
   * @param {Boolean} [proxy] - use the proxy actor
   * @returns {Promise<string>} promise that resolves with the output, or rejects with an exception
   */
  async call (actorId, funcName, wasmArgs = [], secretKey, opts = {}, proxy = false) {
    const tx = DfinityTx.from(this.makeCall(actorId, funcName, wasmArgs, secretKey, opts, proxy))
    return this.signAndSubmit(tx, opts)
  }

  /**
   * Call a function on an existing actor, and return result. Appends a ConsoleLog to the args, and wait for the tx output
   * @param {string} actorId - actor ID
   * @param {string} funcName - name of exported function to call
   * @param {Array}  [wasmArgs] - function arguments
   * @param {Buffer} [secretKey] - ed25519 secret key to sign Payment with
   * @param {Object} [opts] - additional options to pass to Payment or Transaction
   * @param {Boolean} [proxy] - use the proxy actor
   * @returns {Promise<Buffer>} promise that resolves with the call result, or rejects with an exception
   */
  async callResult (actorId, funcName, wasmArgs = [], secretKey, opts = {}, proxy = false) {
    const tx = DfinityTx.from(this.makeCallResult(actorId, funcName, wasmArgs, secretKey, opts, proxy))
    const data = await this.signAndSubmit(tx, opts)
    return this.formatOutput(data)
  }

  makeNew (wasm, secretKey = this.opts.secretKey, opts = {}, proxy = false) {
    return proxy
      ? DfinityTx.makeCallMessage(ROOT_ACTOR, 'new', [wasm], secretKey, opts)
      : DfinityTx.makeNewMessage(wasm, secretKey, opts)
  }

  makeNewAndCall (wasm, funcName, wasmArgs = [], secretKey = this.opts.secretKey, opts = {}, proxy = false) {
    return proxy
      ? DfinityTx.makeCallMessage(ROOT_ACTOR, 'newAndCall', [wasm, funcName, wasmArgs], secretKey, opts)
      : DfinityTx.makeNewAndCallMessage(wasm, funcName, wasmArgs, secretKey, opts)
  }

  makeNewAndCallResult (wasm, funcName, wasmArgs = [], secretKey, opts = {}, proxy = false) {
    return this.makeNewAndCall(wasm, funcName, this.withLogArg(wasmArgs), secretKey, opts, proxy)
  }

  makeCall (actorId, funcName, wasmArgs = [], secretKey = this.opts.secretKey, opts = {}, proxy = false) {
    return proxy
      ? DfinityTx.makeCallMessage(ROOT_ACTOR, 'call', [funcName, wasmArgs], secretKey, opts)
      : DfinityTx.makeCallMessage(actorId, funcName, wasmArgs, secretKey, opts)
  }

  makeCallResult (actorId, funcName, wasmArgs = [], secretKey, opts = {}, proxy = false) {
    return this.makeCall(actorId, funcName, this.withLogArg(wasmArgs), secretKey, opts, proxy)
  }

  async signAndSubmit (tx, opts) {
    const opts1 = await this.withExpiryHeight(opts)
    const opts2 = this.withNextNonce(opts1)
    Object.assign(tx.transaction, opts2)
    tx.sign(this.opts.secretKey)
    return submit(tx, this.opts.host, { debug: this.opts.debug })
  }

  withLogArg (wasmArgs = []) {
    return wasmArgs.concat([ new ConsoleLog() ])
  }

  async withExpiryHeight (opts) {
    if (opts._txExpiresAt != null) {
      return opts
    }

    const res = await getHeight(this.opts.host)
    const text = await res.text()
    const _txExpiresAt = txExpiryNear(Number(text))
    return Object.assign({}, opts, { _txExpiresAt })
  }

  withNextNonce (opts) {
    if (opts._txNonce != null) {
      return opts
    }

    const _txNonce = this.opts.nonce++
    return Object.assign({}, opts, { _txNonce })
  }

  /**
   * Get actor id from local cache
   * @param {string} actorName - actor name
   * @returns {string} actorId - actor ID, or null if not found
   */
  getActor (actorName) {
    if (this.opts.actorIds.has(actorName)) {
      return this.opts.actorIds.get(actorName)
    } else {
      return null
    }
  }

  /**
   * Lookup actor id from network state
   * @param {string} actorName - actor name
   * @returns {Promise<Array<Object>>} actors - actors with matching name
   */
  async lookupActor (actorName) {
    const res = await getState(this.opts.host)
    const state = await res.json()
    return state.actors
      .filter(a => a.name === actorName)
      .map(({ store, ...ac }) => ac)
      .sort((a, b) => Number(b.moduleId) - Number(a.moduleId))
  }

  getNewActorId (data, name) {
    const re = new RegExp(/createActor: module .+, actor "(\w+)(?: \((.+)\))?"/)
    const [output] = this.formatOutput(data, re)
    if (type(output) === 'Error') {
      this.log(output.toString())
      throw output
    } else {
      const [ aId, defaultName ] = output
      this.log('saved actor', name || defaultName, 'as', aId)
      this.opts.actorIds.set(name || defaultName, aId)
      return aId
    }
  }

  formatOutput (data, re = null) {
    const exceptionRe = new RegExp(/exception:.*/, 'i')
    const ret = []
    Object.entries(data).map(([_actor, actorOut]) => {
      const [tx] = Object.keys(actorOut)
      Object.entries(actorOut[tx]).map(([msgIdx, out]) => {
        const idx = Buffer.from(msgIdx.substring(2), 'hex').readUInt16BE(0)

        const buf = Buffer.from(out.substring(2), 'hex')
        const str = buf.toString()

        // TODO: Handle different console actors within same message
        if (ret[idx]) {
          console.warn(`existing output at msg index ${idx}: ${ret[idx]}`)
        }
        if (str.match(exceptionRe)) {
          ret[idx] = Error(str)
        } else if (re) {
          const match = str.match(re)
          if (match) {
            ret[idx] = match.slice(1)
          }
        } else {
          ret[idx] = buf
        }
      })
    })
    return ret
  }
}

module.exports = {
  Client,
  submit,
  getHeight,
  getState
}
