var barebonesWASI = function() {
  var moduleInstanceExports = null;

  var WASI_ESUCCESS = 0;
  var WASI_EBADF = 8;
  var WASI_EINVAL = 28;
  var WASI_ENOSYS = 52;

  var WASI_STDOUT_FILENO = 1;

  function setModuleInstance(instance) {

    moduleInstanceExports = instance.exports;
  }

  function getModuleMemoryDataView() {
    // call this any time you'll be reading or writing to a module's memory
    // the returned DataView tends to be dissaociated with the module's memory buffer at the will of the WebAssembly engine
    // cache the returned DataView at your own peril!!

    return new DataView(moduleInstanceExports.memory.buffer);
  }

  function fd_prestat_get(fd, bufPtr) {

    return WASI_EBADF;
  }

  function fd_prestat_dir_name(fd, pathPtr, pathLen) {

    return WASI_EINVAL;
  }

  function environ_sizes_get(environCount, environBufSize) {

    var view = getModuleMemoryDataView();

    view.setUint32(environCount, 0, !0);
    view.setUint32(environBufSize, 0, !0);

    return WASI_ESUCCESS;
  }

  function environ_get(environ, environBuf) {

    return WASI_ESUCCESS;
  }

  function args_sizes_get(argc, argvBufSize) {

    var view = getModuleMemoryDataView();

    view.setUint32(argc, 0, !0);
    view.setUint32(argvBufSize, 0, !0);

    return WASI_ESUCCESS;
  }

  function args_get(argv, argvBuf) {

    return WASI_ESUCCESS;
  }

  function fd_fdstat_get(fd, bufPtr) {

    var view = getModuleMemoryDataView();

    view.setUint8(bufPtr, fd);
    view.setUint16(bufPtr + 2, 0, !0);
    view.setUint16(bufPtr + 4, 0, !0);

    function setBigUint64(byteOffset, value, littleEndian) {

      var lowWord = value;
      var highWord = 0;

      view.setUint32(littleEndian ? 0 : 4, lowWord, littleEndian);
      view.setUint32(littleEndian ? 4 : 0, highWord, littleEndian);
    }

    setBigUint64(bufPtr + 8, 0, !0);
    setBigUint64(bufPtr + 8 + 8, 0, !0);

    return WASI_ESUCCESS;
  }

  function fd_write(fd, iovs, iovsLen, nwritten) {

    var view = getModuleMemoryDataView();

    var written = 0;
    var bufferBytes = [];

    function getiovs(iovs, iovsLen) {
      // iovs* -> [iov, iov, ...]
      // __wasi_ciovec_t {
      //   void* buf,
      //   size_t buf_len,
      // }
      var buffers = Array.from({
        length: iovsLen
      }, function(_, i) {
        var ptr = iovs + i * 8;
        var buf = view.getUint32(ptr, !0);
        var bufLen = view.getUint32(ptr + 4, !0);

        return new Uint8Array(moduleInstanceExports.memory.buffer, buf, bufLen);
      });

      return buffers;
    }

    var buffers = getiovs(iovs, iovsLen);

    function writev(iov) {

      for (var b = 0; b < iov.byteLength; b++) {

        bufferBytes.push(iov[b]);
      }

      written += b;
    }

    buffers.forEach(writev);

    if (fd === WASI_STDOUT_FILENO) {
      document.getElementById("output").value += String.fromCharCode.apply(null, bufferBytes);
    }

    view.setUint32(nwritten, written, !0);

    return WASI_ESUCCESS;
  }

  function poll_oneoff(sin, sout, nsubscriptions, nevents) {

    return WASI_ENOSYS;
  }

  function proc_exit(rval) {

    return WASI_ENOSYS;
  }

  function fd_close(fd) {

    return WASI_ENOSYS;
  }

  function fd_seek(fd, offset, whence, newOffsetPtr) {

  }

  function fd_close(fd) {

    return WASI_ENOSYS;
  }

  return {
    setModuleInstance: setModuleInstance,
    environ_sizes_get: environ_sizes_get,
    args_sizes_get: args_sizes_get,
    fd_prestat_get: fd_prestat_get,
    fd_fdstat_get: fd_fdstat_get,
    fd_write: fd_write,
    fd_prestat_dir_name: fd_prestat_dir_name,
    environ_get: environ_get,
    args_get: args_get,
    poll_oneoff: poll_oneoff,
    proc_exit: proc_exit,
    fd_close: fd_close,
    fd_seek: fd_seek
  }
}


var runWasmModule = () => {
  window.alert("Module not (yet) loaded")
};

var memory = null;

function importWasmModule(moduleName, wasiPolyfill) {

  const moduleImports = {
    wasi_unstable: wasiPolyfill,
    env: {},
  };

  (async () => {
    var module = null;

    if (WebAssembly.compileStreaming) {
      module = await WebAssembly.compileStreaming(fetch(moduleName));
    } else {
      const response = await fetch(moduleName);
      const buffer = await response.arrayBuffer();
      module = await WebAssembly.compile(buffer);
    }

    runWasmModule = (async () => {
      const instance = await WebAssembly.instantiate(module, moduleImports);
      wasiPolyfill.setModuleInstance(instance);
      memory = instance.exports.memory;

      document.getElementById("output").value = "Running _start()\n";
      instance.exports._start();
      document.getElementById("output").value += "\nstart() finished";
    });
    await runWasmModule()
  })();
}

// From https://github.com/bma73/hexdump-js, with fixes
const hexdump = (function () {
    var _fillUp = function (value, count, fillWith) {
            var l = count - value.length;
            var ret = "";
            while (--l > -1)
                ret += fillWith;
            return ret + value;
        },
        hexdump = function (arrayBuffer, offset, length) {

            var view = new DataView(arrayBuffer);
            offset = offset || 0;
            length = length || arrayBuffer.byteLength;

            var out = _fillUp("Offset", 8, " ") + "  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\n";
            var row = "";
            for (var i = 0; i < length; i += 16) {
                row += _fillUp(offset.toString(16).toUpperCase(), 8, "0") + "  ";
                var n = Math.min(16, length - offset);
                var string = "";
                for (var j = 0; j < 16; ++j) {
                    if (j < n) {
                        var value = view.getUint8(offset);
                        string += value >= 32 && value < 0x7f ? String.fromCharCode(value) : ".";
                        row += _fillUp(value.toString(16).toUpperCase(), 2, "0") + " ";
                        offset++;
                    }
                    else {
                        row += "   ";
                        string += " ";
                    }
                }
                row += " " + string + "\n";
            }
            out += row;
            return out;
        };

    return hexdump;
})();

var wasiPolyfill = new barebonesWASI();

// load files from directory listing
(async () => {
  var ok = false;
  try {
    const dir = await fetch('/run/_out/').then(resp => resp.text());
    const select = document.getElementById("test");
    for (const match of dir.matchAll(/href="([^"]+.wasm)"/g)) {
      const el = document.createElement("option");
      el.textContent = match[1];
      el.value = match[1];
      select.appendChild(el);
      ok = true;
    }
  } finally {
    if (!ok) {
      window.alert("Could not find any wasm files. Did you start this as instructed in test/README.md?")
    }
  }
})();

function loadTest() {
  const test = document.getElementById("test").value;
  if (test == "none") return;
  document.getElementById("output").value = "Loading " + test + "… (see console for errors)";
  importWasmModule("run/_out/" + test, wasiPolyfill);
}

function updateHexDump() {
  document.getElementById("memory").value = "Loading…";
  if (memory) {
    document.getElementById("memory").value = hexdump(memory.buffer);
  } else {
    document.getElementById("memory").value = "No memory yet";
  }
}

function getUint32(view, p) {
  return view.getUint32(p, true)
}

function decodeOBJ(view, p) {
  let size = getUint32(view, p + 4);
  let m = new Map();
  let h = getUint32(view, p + 8) + 1; //unskew
  let q = p + 12;
  for(var i = 0; i < size; i++) {
    let hash = getUint32(view, h);
    //TODO: convert hash to label
    m[hash] = decode(view, getUint32(view, q));
    q += 4;
    h += 4;
  }
  return m;
}

function decodeARRAY(view, p) {
  let size = getUint32(view, p + 4);
  let a = new Array(size);
  let q = p + 8;
  for(var i = 0; i < size; i++) {
    a[i] = decode(view, getUint32(view, q));
    q += 4;
  }
  return a;
}

function decodeMUTBOX(view, p) {
  let a = decode(view, getUint32(view, p+4));
  return { mut: a };
}

function decodeOBJ_IND(view, p) {
  let a = decode(view, getUint32(view, p+4));
  return { ind: a };
}

function decodeCONCAT(view, p) {
  let size = 2;
  let a = new Array(size);
  let q = p + 8; // skip n_bytes
  for(var i = 0; i < size; i++) {
    a[i] = decode(view, getUint32(view, q));
    q += 4;
  }
  return a;
}

function decodeBLOB(view, p) {
  let size = getUint32(view, p + 4);
  let a = new Uint8Array(size);
  let q = p + 8;
  for(var i = 0; i < size; i++) {
    a[i] = view.getUint8(q);
    q += 1;
  };
  try {
    let textDecoder = new TextDecoder('utf-8', { fatal: true }); // hoist and reuse?
    return textDecoder.decode(a);
  }
  catch(err) {
    return a;
  }
}

function decode(view, v) {
  if ((v & 1) === 0) return v >> 1;
  let p = v + 1;
  let tag = getUint32(view, p);
  switch (tag) {
    case 1 : return decodeOBJ(view, p);
    case 2 : return decodeOBJ_IND(view, p);
    case 3 : return decodeARRAY(view, p);
    //    case 4 :
    case 5 : return "BITS64";
    case 6 : return decodeMUTBOX(view, p);
    case 7 : return "CLOSURE";
    case 8 : return "SOME";
    case 9 : return "VARIANT";
    case 10 : return decodeBLOB(view, p);
    case 11 : return "FWD_PTR";
    case 12 : return "BITS32";
    case 13 : return "BIGINT";
    case 14 : return decodeCONCAT(view, p);
    case 15 : return null;
    default : return "UNKOWN";
  };
}

function show(v) {
    const view = new DataView(memory.buffer);
    return decode(view, v);
}

// https://www.mattzeunert.com/2016/02/19/custom-chrome-devtools-object-formatters.html
/*
window.devtoolsFormatters = [{
    header: function(obj){
        return ["div", {}, "value"]
    },
    hasBody: function(){
        return false;
    }
}]
*/
