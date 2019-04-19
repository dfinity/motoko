#!/bin/bash
${ASC:-$(dirname "$BASH_SOURCE")/../../src/asc} -v -i <<__END__
import "lib/b.as";
import "lib/a.as";
import "lib/c.as";
import "lib/triangle.as";
__END__
