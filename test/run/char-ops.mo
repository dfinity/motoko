import Prim "mo:⛔";

//
// charToUpper
//

assert(Prim.charToUpper('ö') == 'Ö');
assert(Prim.charToUpper('σ') == 'Σ');
assert(Prim.charToUpper('💩') == '💩');

//
// charToLower
//

assert(Prim.charToLower('Ö') == 'ö');
assert(Prim.charToLower('Σ') == 'σ');
assert(Prim.charToLower('💩') == '💩');

//
// charIsWhitespace
//

assert(Prim.charIsWhitespace(' '));

assert(not Prim.charIsWhitespace('x'));

// 12288 (U+3000) = ideographic space
assert(Prim.charIsWhitespace(Prim.nat32ToChar(12288)));

assert(Prim.charIsWhitespace('\t'));

// Vertical tab ('\v')
assert(Prim.charIsWhitespace(Prim.nat32ToChar(0x0B)));

// Form feed ('\f')
assert(Prim.charIsWhitespace(Prim.nat32ToChar(0x0C)));

assert(Prim.charIsWhitespace('\r'));

//
// charIsLowercase
//

assert(Prim.charIsLowercase('x'));
assert(not Prim.charIsLowercase('X'));

//
// charIsUppercase
//

assert(Prim.charIsUppercase('X'));
assert(not Prim.charIsUppercase('x'));

//
// charIsAlphabetic
//

assert(Prim.charIsAlphabetic('a'));
assert(Prim.charIsAlphabetic('京'));
assert(not Prim.charIsAlphabetic('㋡'));
