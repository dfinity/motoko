#!/usr/bin/env bash
# Tests that shorthand variants come out without parens
${MOC:-$(dirname "$BASH_SOURCE")/../../src/moc} -i <<__END__
#bar;
#foo(#bar);
[#Monday, #Tuesday, #Wednesday, #Thursday, #Friday, #Saturday, #Sunday];
__END__
