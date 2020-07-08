import Prim "mo:prim";

//
// charIsWhitespace
//

assert(Prim.charIsWhitespace(' '));

assert(not Prim.charIsWhitespace('x'));

// 12288 (U+3000) = ideographic space
assert(Prim.charIsWhitespace(Prim.word32ToChar(12288)));

assert(Prim.charIsWhitespace('\t'));

// Vertical tab ('\v')
assert(Prim.charIsWhitespace(Prim.word32ToChar(0x0B)));

// Form feed ('\f')
assert(Prim.charIsWhitespace(Prim.word32ToChar(0x0C)));

assert(Prim.charIsWhitespace('\r'));

//
// charToUpper
//

assert(Prim.charToUpper('ö') == 'Ö');
assert(Prim.charToUpper('σ') == 'Σ');
assert(Prim.charToUpper('💩') == '💩');

// charIsAlphabetic

assert(Prim.charIsAlphabetic('a'));
assert(Prim.charIsAlphabetic('京'));
assert(not Prim.charIsAlphabetic('㋡'));
