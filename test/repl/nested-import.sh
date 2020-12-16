#!/usr/bin/env bash
moc -v -i <<__END__ | grep Parsing
import "lib/nested";
__END__
