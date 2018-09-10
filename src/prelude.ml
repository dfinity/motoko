let prelude =
{|
type Any = prim "Any";
type Null = prim "Null";
type Bool = prim "Bool";
type Nat = prim "Nat";
type Int = prim "Int";
type Word8 = prim "Word8";
type Word16 = prim "Word16";
type Word32 = prim "Word32";
type Word64 = prim "Word64";
type Float = prim "Float";
type Char = prim "Char";
type Text = prim "Text";

let abs : Int -> Nat = prim "abs";

// TODO: make this a method
let length : <T_> T_[] -> Nat = prim "length";
|}
