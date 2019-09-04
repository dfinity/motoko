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

let func_modes = ["oneway", Oneway; "pure", Pure]
let get_func_mode m = List.assoc m func_modes               

let hash = IdlHash.idl_hash

let record_fields fs =
  let open Uint32 in
  let rec go start fs =
    match fs with
    | [] -> []
    | hd :: tl ->
       let field = hd start in
       let next = succ field.it.id in
       field :: (go next tl)
  in go zero fs
%}

%token EOF

%token LPAR RPAR LCURLY RCURLY
%token ARROW
%token FUNC TYPE SERVICE IMPORT
%token SEMICOLON COMMA COLON EQ
%token OPT VEC RECORD VARIANT BLOB
%token<string> NAT
%token<string> ID
%token<string> TEXT

%start<string -> Syntax.prog> parse_prog

%%

(* Helpers *)

seplist(X, SEP) :
  | (* empty *) { [] }
  | x=X { [x] }
  | x=X SEP xs=seplist(X, SEP) { x::xs }

(* Basics *)

%inline id :
  | id=ID { id @@ at $sloc }

%inline name :
  | id=ID { id @@ at $sloc }
  | text=TEXT { text @@ at $sloc }

(* Types *)

prim_typ :
  | x=id
    { (match is_prim_typs x.it with
         None -> VarT x
       | Some t -> PrimT t) @@ at $sloc }

ref_typ :
  | FUNC t=func_typ { t }
  | SERVICE ts=actor_typ { ServT ts @@ at $sloc }

field_typ :
  | n=NAT COLON t=data_typ
    { { id = Uint32.of_string n; name = n @@ at $loc(n); typ = t } @@ at $sloc }
  | name=name COLON t=data_typ
    { { id = hash name.it; name = name; typ = t } @@ at $sloc }

record_typ :
  | f=field_typ { fun _ -> f }
  | t=data_typ
    { fun x -> { id = x; name = Uint32.to_string x @@ no_region; typ = t } @@ at $sloc }

variant_typ :
  | f=field_typ { f }
  | name=name
    { { id = hash name.it; name = name; typ = PrimT Null @@ no_region } @@ at $sloc }
  | n=NAT
    { { id = Uint32.of_string n; name = n @@ at $loc(n); typ = PrimT Null @@ no_region } @@ at $sloc }

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
  | BLOB { VecT (PrimT Nat8 @@ no_region) @@ at $sloc }

data_typ :
  | t=cons_typ { t }
  | t=ref_typ { t }
  | t=prim_typ { t }

param_typs :
  | LPAR fs=seplist(record_typ, COMMA) RPAR
    { record_fields fs }

func_mode :
  | m=id
    { get_func_mode m.it @@ at $sloc }

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
  | IMPORT file=TEXT
    { ImportD (file, ref "") @@ at $sloc }

actor :
  | (* empty *) { None }
  | SERVICE id=id tys=actor_typ
    { Some (ActorD(id, ServT tys @@ at $loc(tys)) @@ at $sloc) }
  | SERVICE id=id COLON x=id
    { Some (ActorD(id, VarT x @@ x.at) @@ at $sloc) }

(* Programs *)

parse_prog :
  | ds=seplist(def, SEMICOLON) actor=actor EOF
    { fun filename -> { it = {decs=ds; actor=actor}; at = at $sloc; note = filename} }

%%
