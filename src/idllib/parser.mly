%{

open Syntax
open Source

module Uint32 = Lib.Uint32

(* Position handling *)

let position_to_pos position =
  (* TBR: Remove assertion once the menhir bug is fixed. *)
  assert (Obj.is_block (Obj.repr position));
  { file = position.Lexing.pos_fname;
    line = position.Lexing.pos_lnum;
    column = position.Lexing.pos_cnum - position.Lexing.pos_bol
  }

let positions_to_region position1 position2 =
  { left = position_to_pos position1;
    right = position_to_pos position2
  }

let at (startpos, endpos) = positions_to_region startpos endpos

let _anon sort at = "anon-" ^ sort ^ "-" ^ string_of_pos at.left

let prim_typs = ["nat", Nat; "nat8", Nat8; "nat16", Nat16; "nat32", Nat32; "nat64", Nat64;
                 "int", Int; "int8", Int8; "int16", Int16; "int32", Int32; "int64", Int64;
                 "float32", Float32; "float64", Float64; "bool", Bool; "text", Text;
                 "null", Null; "reserved", Reserved; "empty", Empty]
let is_prim_typs t = List.assoc_opt t prim_typs

let hash = IdlHash.idl_hash
let record_fields fs =
  let open Uint32 in
  let rec go start fs =
    match fs with
    | [] -> []
    | hd :: tl ->
       let field = hd start in
       let next =
         (match field.it.label.it with
         | Id n -> succ n
         | Named name -> succ (hash name)
         | Unnamed n -> succ n) in
       field :: (go next tl)
  in go zero fs
%}

%token EOF

%token LPAR RPAR LCURLY RCURLY
%token ARROW
%token FUNC TYPE SERVICE IMPORT PRINCIPAL
%token SEMICOLON COMMA COLON EQ
%token NOTCOLON EQQ NOTEQ
%token OPT VEC RECORD VARIANT BLOB ONEWAY QUERY
%token<string> NAT
%token<string> ID
%token<string> TEXT

%start<string -> Syntax.prog> parse_prog
%start<string -> Syntax.tests> parse_tests

%%

(* Helpers *)

endlist(X, SEP) :
  | (* empty *) { [] }
  | x=X SEP xs=seplist(X, SEP) { x::xs }

seplist(X, SEP) :
  | (* empty *) { [] }
  | x=X { [x] }
  | x=X SEP xs=seplist(X, SEP) { x::xs }

(* Basics *)

%inline text :
 | s=TEXT
   { try ignore (Wasm.Utf8.decode s); s
     with Wasm.Utf8.Utf8 -> raise (ParseError (at $sloc, "Invalid UTF-8"))
   }

%inline id :
  | id=ID { id @@ at $sloc }

%inline name :
  | id=ID { id @@ at $sloc }
  | text=text { text @@ at $sloc }

(* Types *)

prim_typ :
  | x=id
    { (match is_prim_typs x.it with
         None -> VarT x
       | Some t -> PrimT t) @@ at $sloc }

ref_typ :
  | FUNC t=func_typ { t }
  | SERVICE ts=actor_typ { ServT ts @@ at $sloc }
  | PRINCIPAL { PrincipalT @@ at $sloc }

field_typ :
  | n=NAT COLON t=data_typ
    { { label = Id (Uint32.of_string n) @@ at $loc(n); typ = t } @@ at $sloc } 
  | name=name COLON t=data_typ
    { { label = Named name.it @@ at $loc(name); typ = t } @@ at $sloc } 

record_typ :
  | f=field_typ { fun _ -> f }
  | t=data_typ
    { fun x -> { label = Unnamed x @@ no_region; typ = t } @@ at $sloc }

variant_typ :
  | f=field_typ { f }
  | name=name
    { { label = Named name.it @@ at $loc(name); typ = PrimT Null @@ no_region } @@ at $sloc } 
  | n=NAT
    { { label = Id (Uint32.of_string n) @@ at $loc(n); typ = PrimT Null @@ no_region } @@ at $sloc }

record_typs :
  | LCURLY fs=seplist(record_typ, SEMICOLON) RCURLY
    { record_fields fs }

variant_typs :
  | LCURLY fs=seplist(variant_typ, SEMICOLON) RCURLY { fs }

cons_typ :
  | OPT t=data_typ { OptT t @@ at $sloc }
  | VEC t=data_typ { VecT t @@ at $sloc }
  | RECORD fs=record_typs { RecordT fs @@ at $sloc }
  | VARIANT fs=variant_typs { VariantT fs @@ at $sloc }
  | BLOB { BlobT @@ at $sloc }

data_typ :
  | t=cons_typ { t }
  | t=ref_typ { t }
  | t=prim_typ { t }

param_typs :
  | LPAR fs=seplist(param_typ, COMMA) RPAR
    { fs }

param_typ :
  | t=data_typ { t }
  | name COLON t=data_typ { t }

func_mode :
  | ONEWAY { Oneway @@ at $sloc }
  | QUERY { Query @@ at $sloc }

func_modes_opt :
  | (* empty *) { [] }
  | m=func_mode ms=func_modes_opt { m::ms }

func_typ :
  | t1=param_typs ARROW t2=param_typs ms=func_modes_opt
    { FuncT(ms, t1, t2) @@ at $sloc }

meth_typ :
  | x=name COLON t=func_typ
    { { var = x; meth = t } @@ at $sloc }
  | x=name COLON id=id
    { { var = x; meth = VarT id @@ at $sloc } @@ at $sloc }

actor_typ :
  | LCURLY ds=seplist(meth_typ, SEMICOLON) RCURLY
    { ds }

(* Declarations *)

def :
  | TYPE x=id EQ t=data_typ
    { TypD(x, t) @@ at $sloc }
  (* TODO enforce all imports to go first in the type definitions  *)
  | IMPORT file=text
    { ImportD (file, ref "") @@ at $sloc }

id_opt :
  | (* empty *) { }
  | id { }

actor :
  | (* empty *) { None }
  | SERVICE id_opt COLON tys=actor_typ
    { Some (ServT tys @@ at $loc(tys)) }
  | SERVICE id_opt COLON x=id
    { Some (VarT x @@ x.at) }

(* Programs *)

parse_prog :
  | ds=seplist(def, SEMICOLON) actor=actor EOF
    { fun filename -> { it = {decs=ds; actor=actor}; at = at $sloc; note = filename} }

(* Test suite *)

input :
  | text=text { TextualInput text }
  | BLOB text=TEXT { BinaryInput text }

assertion :
  | input=input COLON    { ParsesAs (true,  input) }
  | input=input NOTCOLON { ParsesAs (false, input) }
  | i1=input EQQ   i2=input COLON { ParsesEqual (true,  i1, i2) }
  | i1=input NOTEQ i2=input COLON { ParsesEqual (false, i1, i2) }

test :
  | id=id assertion=assertion tys=param_typs desc=text?
    { if id.it != "assert" then raise (ParseError (at $loc(id), "Expect an assert"))
      else { ttyp=tys; assertion; desc } @@ at $sloc }

parse_tests :
  | tdecs=endlist(def, SEMICOLON) tests=seplist(test, SEMICOLON) EOF
    { fun filename -> { it = {tdecs; tests}; at = at $sloc; note = filename} }

%%
