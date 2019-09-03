var x = require("./test-did.js")
var fs = require("fs")

var gInstance = {"0": true, "1": 422}

var resultBuffer = x.g.encode(gInstance)

fs.writeFileSync("bytes", resultBuffer)
