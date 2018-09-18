let prelude =
{|
type Any = prim "Any";
type None = prim "None";
type Class = prim "Class";
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

type Iter<T_> = {next : () -> T_?};

let abs : Int -> Nat = prim "abs";

func ignore (_ : Any) {};

class range(x : Nat, y : Nat) {
  private var i = x;
  next() : Nat? { if (i > y) null else {let j = i; i += 1; j} };
};

class revrange(x : Nat, y : Nat) {
  private var i = x + 1;
  next() : Nat? { if (i <= y) null else {i -= 1; i} };
};
|}
