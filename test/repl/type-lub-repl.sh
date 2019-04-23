#!/usr/bin/env bash
# Tests that correct lub types are inferred when values appear in arrays
${ASC:-$(dirname "$BASH_SOURCE")/../../src/asc} -i ../run/type-lub.as <<__END__
opts;
nulls;
incompatible_objs;
objs;
obj_arrs;
obj_texts;
arr_texts;
obj_arr_texts;
tups;
tup1s;
arrs;
incompatible_funcs;
funcs;
funcs[0]([1, 2, 3]);
funcs[1]([1, 2, 3]);
__END__
