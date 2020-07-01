import Prim "mo:prim";

assert(Prim.charIsWhitespace(' '));
assert(not Prim.charIsWhitespace('x'));
// 12288 (U+3000) = ideographic space
assert(Prim.charIsWhitespace(Prim.word32ToChar(12288)));

assert(Prim.charToUpper('ö') == 'Ö');
assert(Prim.charToUpper('σ') == 'Σ');
assert(Prim.charToUpper('💩') == '💩');
