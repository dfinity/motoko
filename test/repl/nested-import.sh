#!/bin/bash
${MOC:-$(dirname "$BASH_SOURCE")/../../src/moc} -v -i <<__END__ | grep Parsing
import "lib/nested";
__END__
