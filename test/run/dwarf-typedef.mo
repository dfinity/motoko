// RUN: llvm-dwarfdump %.wasm -debug-info | FileCheck %.mo -check-prefix=DWARF

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

// argument types get normalised, so have one typedef returned too

// DWARF:      DW_TAG_typedef
// DWARF-NEXT:   DW_AT_name	("ListV/1")
// DWARF-NEXT:   DW_AT_type	(0x{{[0-9a-f]*}} "VARIANT")

func tailV(l : ListV) : ListV = switch l {
  case (#cons { tl }) tl;
  case (#empty) #empty
};

assert ?42 == headV (#cons { hd = 42; tl = #cons { hd = 25; tl = #empty } });
assert #empty == tailV (#cons { hd = 42; tl = #empty });

//MOC-FLAG -g
