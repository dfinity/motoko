#!/bin/bash
${ASC:-$(dirname "$BASH_SOURCE")/../../src/asc} -v -i <<__END__ | grep Parsing
import "lib/nested.as";
__END__
