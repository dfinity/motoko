%{

open Syntax_idl
open Source

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

let (@?) it at = {it; at; note = empty_typ_note}
let (@!) it at = {it; at; note = Type.Pre}
let (@=) it at = {it; at; note = None}

let anon sort at = "anon-" ^ sort ^ "-" ^ string_of_pos at.left

let prim_typs = ["nat"; "nat8"; "nat16"; "nat32"; "nat64";
                 "int"; "int8"; "int16"; "int32"; "int64";
                 "float32"; "float64"; "bool"; "text";
                 "null"; "unavailable"]
let is_prim_typs t = List.find_opt (fun x -> x=t) prim_typs         
%}

%token EOF

%token LPAR RPAR LBRACKET RBRACKET LCURLY RCURLY
%token ARROW
%token FUNC TYPE SERVICE SENSITIVE PURE UPDATE
%token SEMICOLON SEMICOLON_EOL COMMA COLON EQ
%token OPT VEC RECORD VARIANT ENUM BLOB
%token<string> NAT
%token<string> ID
%token<string> TEXT

%left COLON

%start<Syntax_idl.prog> parse_prog
%start<Syntax_idl.prog> parse_prog_interactive

%%

(* Helpers *)

seplist(X, SEP) :
  | (* empty *) { [] }
  | x=X { [x] }
  | x=X SEP xs=seplist(X, SEP) { x::xs }

(* Basics *)

%inline semicolon :
  | SEMICOLON
  | SEMICOLON_EOL { () }

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
       | Some t -> PrimT t) @! at $sloc }

ref_typ :
  | FUNC t=func_typ { RefFuncT t @! at $sloc }
  | SERVICE ts=actor_typ { RefServT ts @! at $sloc }

field_typ :
  | n=NAT COLON t=data_typ
    { { id = Stdint.Uint64.of_string n; name = n @@ no_region; typ = t } @@ at $sloc }
  | name=name COLON t=data_typ
    (* TODO find a better hash function *)
    { { id = Stdint.Uint64.of_int (Hashtbl.hash name.it); name = name; typ = t } @@ at $sloc }
  | t=data_typ
    { let name = anon "field" t.at @@ t.at in 
      { id = Stdint.Uint64.of_int (Hashtbl.hash name.it); name = name; typ = t } @@ at $sloc }

field_typs :
  | LCURLY fs=seplist(field_typ, semicolon) RCURLY { fs }

enums :
  | LCURLY es=seplist(name, semicolon) RCURLY { }

cons_typ :
  | OPT t=data_typ { OptT t @! at $sloc }
  | VEC t=data_typ { VecT t @! at $sloc }
  | RECORD fs=field_typs { RecordT fs @! at $sloc }
  | VARIANT fs=field_typs { VariantT fs @! at $sloc }
  | BLOB { VecT (PrimT "nat8" @! no_region) @! at $sloc }
  (* TODO add enums  *)
  | ENUM enums { PrimT "nat64" @! at $sloc }

data_typ :
  | t=cons_typ { t }
  | t=ref_typ { t }
  | t=prim_typ { t }

param_typ :
 | name COLON t=data_typ { t }
 | t=data_typ { t }

param_typs :
  | t = data_typ { t }
  | LPAR ts=seplist(param_typ, COMMA) RPAR
    { match ts with [t] -> t | _ -> TupT(ts) @! at $sloc }

func_mode :
  | SENSITIVE { Type_idl.Sensitive @@ at $sloc }
  | PURE { Type_idl.Pure @@ at $sloc }
  | UPDATE { Type_idl.Updatable @@ at $sloc }

func_modes_opt :
  | (* empty *) { [] }
  | LBRACKET ms=seplist(func_mode, COMMA) RBRACKET { ms }

func_typ :
  | t1=param_typs ARROW ms=func_modes_opt t2=param_typs
    { FuncT(ms, t1, t2) @! at $sloc }

meth_typ :
  | x=name COLON t=func_typ
    { { var = x; bound = t } @! at $sloc }
  | x=name COLON id=id
    { { var = x; bound = VarT id @! at $sloc } @! at $sloc }

actor_typ :
  | LCURLY ds=seplist(meth_typ, semicolon) RCURLY
    { ds }

(* Declarations *)

def :
  | TYPE x=id EQ t=data_typ
    { TypD(x, t) @? at $sloc }

actor :
  | SERVICE id=id tys=actor_typ
    { ActorD(id, tys) @? at $sloc }

dec :
  | d=def { d }
  | d=actor { d }

(* Programs *)

parse_prog :
  | ds=seplist(dec, semicolon) EOF { ds @@ at $sloc }

parse_prog_interactive :
  | ds=seplist(dec, SEMICOLON) SEMICOLON_EOL { ds @@ at $sloc }

%%
