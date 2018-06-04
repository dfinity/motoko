%{
open Types
open Syntax
open Source 
(* Error handling *)

exception SyntaxError of region * string


let error at msg = raise (SyntaxError (at, msg))

let parse_error msg =
  error Source.no_region
    (if msg = "syntax error" then "unexpected token" else msg)


(* Position handling *)

let position_to_pos position =
  { file = position.Lexing.pos_fname;
    line = position.Lexing.pos_lnum;
    column = position.Lexing.pos_cnum - position.Lexing.pos_bol
  }

let positions_to_region position1 position2 =
  { left = position_to_pos position1;
    right = position_to_pos position2
  }

let at () =
  positions_to_region (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())
let ati i =
  positions_to_region (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i)


(* Literals *)

let literal f s =
  try f s with Failure _ -> error s.at "constant out of range"

let nat s at =
  try
    let n = int_of_string s in
    if n >= 0 then n else raise (Failure "")
  with Failure _ -> error at "integer constant out of range"

(*
let nat32 s at =
  try I32.of_string_u s with Failure _ -> error at "i32 constant out of range"

let name s at =
  try Utf8.decode s with Utf8.Utf8 -> error at "invalid UTF-8 encoding"

*)
%}

%token MUT LET VAR
%token LPAR RPAR LBRACKET RBRACKET LCURLY RCURLY
%token IF THEN ELSE SWITCH LOOP WHILE FOR LABEL BREAK RETURN CONTINUE ASYNC
%token AWAIT 
%token FUNC TYPE ACTOR CLASS PRIVATE
%token SEMICOLON COLON COMMA DOT
%token ASSERT
%token EOF
%token ADDOP SUBOP MULOP DIVOP MODOP ANDOP OROP XOROP NOTOP SHIFTLOP SHIFTROP
%token ROTLOP ROTROP
%token CATOP
/* comparisons? */
%token LT GT
%token<string> NAT
%token<string> INT
%token<string> FLOAT
%token<string> STRING
%token<string> ID
(* %token<string Source.phrase -> Ast.instr' * Values.value> CONST *)

%start<Syntax.typ> typ
%%

/* Helpers */

option(V) :
  | /* empty */ { None @@ at() }
  | v=V { Some v @@ at() } 


list(V) :
  | /* empty */ { [] @@ at() }
  | v = V vs = list(V) { (v::vs.it) @@ at() } 

/* Types */

id :
  | id = ID { id @@ at()}

typ : 
  |  id = id ots = typ_args? {
     	VarT (id,
	      match ots.it with
	       Some ts -> ts | None -> [] )
	@@ at()
     }
  | a = actor? LCURLY tfs = list(tf = typ_field SEMICOLON {tf}) RCURLY
    {
       let actor =
       	   (match a.it with Some _ -> Actor | _ -> Object)
            @@ a.at
       in
          ObjT (actor,tfs.it)
          @@ at()
    }

actor:
  | ACTOR { Actor @@ at()}

typ_args :
  | LT ts = list(t = typ COMMA {t}) GT { ts.it }

typ_params :
  | LT ts = list(id = id COMMA {id}) GT { ts.it }

typ_field :
  | id = id COLON t=typ
     { {var = id; typ = t; mut = ConstMut @@ no_region} @@ at() }
  | VAR id = id COLON t=typ
     { {var = id; typ = t; mut = VarMut @@ no_region} @@ at() }
//  | id = id typ_params? <params>+ COLON t=typ
//     { {var = id; typ = t; mut = VarMut @@ no_region} @@ at() }   

%%
