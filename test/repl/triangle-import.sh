#!/bin/bash
${MOC:-$(dirname "$BASH_SOURCE")/../../src/moc} -v -i <<__END__
import "lib/b.mo";
import "lib/a.mo";
import "lib/c.mo";
import "lib/triangle.mo";
__END__
