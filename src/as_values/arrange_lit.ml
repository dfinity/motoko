open As_types
open Lit
open Wasm.Sexpr

let ($$) head inner = Node (head, inner)

let lit (l:lit) = match l with
  | NullLit       -> Atom "NullLit"
  | BoolLit true  -> "BoolLit"   $$ [ Atom "true" ]
  | BoolLit false -> "BoolLit"   $$ [ Atom "false" ]
  | NatLit n      -> "NatLit"    $$ [ Atom (Value.Nat.to_string n) ]
  | IntLit i      -> "IntLit"    $$ [ Atom (Value.Int.to_string i) ]
  | Word8Lit w    -> "Word8Lit"  $$ [ Atom (Value.Word8.to_string_u w) ]
  | Word16Lit w   -> "Word16Lit" $$ [ Atom (Value.Word16.to_string_u w) ]
  | Word32Lit w   -> "Word32Lit" $$ [ Atom (Value.Word32.to_string_u w) ]
  | Word64Lit w   -> "Word64Lit" $$ [ Atom (Value.Word64.to_string_u w) ]
  | FloatLit f    -> "FloatLit"  $$ [ Atom (Value.Float.to_string f) ]
  | CharLit c     -> "CharLit"   $$ [ Atom (string_of_int c) ]
  | TextLit t     -> "TextLit"   $$ [ Atom t ]
  | PreLit (s,p)  -> "PreLit"    $$ [ Atom s; Arrange_type.prim p ]
