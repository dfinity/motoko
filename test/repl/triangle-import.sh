#!/usr/bin/env bash
moc -v -i <<__END__
import _ = "lib/b";
import _ = "lib/a";
import _ = "lib/c";
import _ = "lib/triangle";
__END__
