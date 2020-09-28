// RUN: llvm-dwarfdump %.wasm -debug-info | FileCheck %.mo -check-prefix=DWARF

// DWARF:  DW_TAG_typedef

type List = ?(Int, List);

func head(l : List) : ?Int = switch l {
  case (?(h, _)) ?h;
  case null null
};

assert ?42 == head (?(42, ?(25, null)));


type ListV = { #empty; #cons : { hd : Int; tl : ListV } };


func headV(l : ListV) : ?Int = switch l {
  case (#cons { hd }) ?hd;
  case (#empty) null
};

assert ?42 == headV (#cons { hd = 42; tl = #cons { hd = 25; tl = #empty } });
