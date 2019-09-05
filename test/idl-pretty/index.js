var x = require("./test-did.js")
var fs = require("fs")

var gInstance = {"0": true, "1": 422, "2": -422, "3": -422}
console.log(x.g)
var resultBuffer = x.g.encode(gInstance)

fs.writeFileSync("bytes", resultBuffer)
