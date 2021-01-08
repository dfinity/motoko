#!/usr/bin/env bash
moc -v -i <<__END__
import "lib/b";
import "lib/a";
import "lib/c";
import "lib/triangle";
__END__
