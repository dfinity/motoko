#!/usr/bin/env bash
${ASC:-$(dirname "$BASH_SOURCE")/../../src/asc} -v -i <<__END__
import "lib/empty.as";
import "lib/empty.as";
__END__
