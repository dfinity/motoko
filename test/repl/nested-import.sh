#!/usr/bin/env bash
moc -v -i <<__END__ | grep Parsing
import _ = "lib/nested";
__END__
