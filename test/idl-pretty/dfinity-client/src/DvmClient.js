const util = require('util');
const execFile = util.promisify(require('child_process').execFile);
const { withFile } = require('tmp-promise');
const fs = require('fs').promises;

const { hexToBuffer, UserIdentity } = require('./Types')


/**
 * A stub version of Client.js that does not actually use the network
 * but rather uses the `dvm` command line tool, to be used for 
 * automated tests
 */
class DvmClient {
  constructor (opts = {}) {
    const defaults = this.constructor.defaults
  }

  /**
   * Test the network connection by getting the height
   * @returns {Promise<string>} promise with the height response
   */
  async connect () {
    const { stderr, stdout } = await execFile( 'dvm', [ 'reset' ] );
    const out = JSON.parse(stdout);
    if (!out['stateRoot']) {
      throw Error('dvm reset output does not contain state root')
    }
    return 1; // fake block height
  }

  async batch (payments, opts = {}) {
    throw Error('batch not implemented')
  }

  async new (actorName, wasm, secretKey, opts = {}, proxy = false) {
    return withFile(async ({path, fd}) => {
      await fs.writeFile(path, wasm);
      const { stderr, stdout } = await execFile('dvm', [ 'new', path]);
      const out = JSON.parse(stdout);
      const idRe = new RegExp(/^(\d+)/)
      const match = idRe.exec(out['id'])
      if (!match) {
        throw Error('dvm new did not return an actor id')
      }
      const id = match[0];
      return id;
    }, { prefix: 'actor_' + actorName + '_', postfix: '.wasm'});
  }

  async newAndCall (actorName, wasm, funcName, wasmArgs = [], secretKey, opts = {}, proxy = false) {
    const actorId = await this.new(actorName, wasm)
    await this.call(actorId, funcName, wasmArgs, secretKey, opts, proxy)
    return actorId
  }

  async call (actorId, funcName, wasmArgs = [], secretKey, opts = {}, proxy = false) {
    const { stderr, stdout } = await execFile('dvm',
      [ 'run', actorId, funcName ]
        .concat(this.argsToDvmArgs(wasmArgs))
    )
  }

  async callResult (actorId, funcName, wasmArgs = [], secretKey, opts = {}, proxy = false) {
    const { stderr, stdout } = await execFile('dvm',
      [ '--disable-colors', 'run', actorId, funcName ]
        .concat(this.withLogArg(this.argsToDvmArgs(wasmArgs)))
    )
    // last line is the JSON-decoded output (I hope)
    // console.log(stdout)
    const last_line = stdout.trim().split(/\r?\n/).slice(-1)[0]
    // console.log("last line:", last_line)
    const unescaped = JSON.parse(last_line)
    const buf =
      unescaped.match(/^0x/) ?
      Buffer.from(unescaped.replace(/^0x/,''), 'hex') :
      Buffer.from(unescaped);
    // console.log(buf)
    return buf
  }

  makeCall (actorId, funcName, wasmArgs = [], secretKey = this.opts.secretKey, opts = {}, proxy = false) {
    throw Error('makeCall not implemented')
  }

  makeCallResult (actorId, funcName, wasmArgs = [], secretKey, opts = {}, proxy = false) {
    throw Error('makeCallResult not implemented')
  }

  argToDvmArg (x) {
    if (typeof x === 'object' && x instanceof UserIdentity) {
      return 'user_id'
    } else if (typeof x === 'object' && x instanceof Buffer) {
      return '0x' + x.toString('hex')
    } else {
      throw Error('argToDvmArg:', x)
    }
  }

  argsToDvmArgs (wasmArgs = []) {
    return wasmArgs.map(this.argToDvmArg)
  }

  withLogArg (wasmArgs = []) {
    return wasmArgs.concat([ 'default_console:log' ])
  }
}

module.exports = {
  DvmClient,
}
