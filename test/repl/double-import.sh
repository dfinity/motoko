#!/usr/bin/env bash
${MOC:-$(dirname "$BASH_SOURCE")/../../src/moc} -v -i <<__END__
import "lib/empty";
import "lib/empty";
__END__
