#!/usr/bin/env bash
${MOC:-$(dirname "$BASH_SOURCE")/../../src/moc} -v -i <<__END__
import "lib/empty.mo";
import "lib/empty.mo";
__END__
