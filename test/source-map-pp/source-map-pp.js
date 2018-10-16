#! /usr/bin/env node
var fs = require('fs');
var re = /^(.*\.wasm):0x([0-9a-f]+):(.*)$/;

async function processLine (line) {
  if (match = line.match(re)) {
    var file = match[1];
    var pos  = parseInt(match[2], 16);
    var rest = match[3];

    var map_file = file + ".map";

    if (fs.existsSync(map_file)) {
      var map_data = fs.readFileSync(map_file, 'utf8');
      var sourceMap = require('source-map');
      const consumer = await new sourceMap.SourceMapConsumer(map_data);
      var orig = consumer.originalPositionFor({line:1, column:pos});
      if (orig.source && orig.line) {
        process.stdout.write(orig.source + ":" + orig.line + "." + orig.column + ":" + rest + "\n");
        consumer.destroy();
        return
      }
      consumer.destroy();
    }
  }
  // fall through
  process.stdout.write(line + "\n");
}

process.stdin.pipe(require('split')(null, null, { trailing: false })).on('data', processLine)
