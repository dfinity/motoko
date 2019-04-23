#!/usr/bin/env bash
# Tests that shorthand variants come out without parens
${ASC:-$(dirname "$BASH_SOURCE")/../../src/asc} -i <<__END__
#bar;
#foo (#bar);
[#Monday, #Tuesday, #Wednesday, #Thursday, #Friday, #Saturday, #Sunday];
__END__
