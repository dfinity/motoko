import Prim = "mo:⛔";
actor {

  public type None = Prim.Types.None;
  public type Null = Prim.Types.Null;
  public type Bool = Prim.Types.Bool;
  public type Nat = Prim.Types.Nat;
  public type Nat8 = Prim.Types.Nat8;
  public type Nat16 = Prim.Types.Nat16;
  public type Nat32 = Prim.Types.Nat32;
  public type Nat64 = Prim.Types.Nat64;
  public type Int = Prim.Types.Int;
  public type Int8 = Prim.Types.Int8;
  public type Int16 = Prim.Types.Int16;
  public type Int32 = Prim.Types.Int32;
  public type Int64 = Prim.Types.Int64;
  public type Float = Prim.Types.Float;
  public type Char = Prim.Types.Char;
  public type Text = Prim.Types.Text;
  public type Blob = Prim.Types.Blob;
  public type Error = Prim.Types.Error;
  public type Principal = Prim.Types.Principal;


  type T = (
    None,
    Null,
    Bool,
    Nat,
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Float,
    Char,
    Text,
    Blob,
    //Error,
    Principal
  );

  stable var t = [] : [T];
}
