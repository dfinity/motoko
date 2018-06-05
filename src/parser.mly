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
  try i32.of_string_u s with Failure _ -> error at "i32 constant out of range"


let name s at =
  try Utf8.decode s with Utf8.Utf8 -> error at "invalid UTF-8 encoding"

*)

(* bogus bound *)
let anyT = TupT []

%}

%token MUT LET VAR
%token LPAR RPAR LBRACKET RBRACKET LCURLY RCURLY
%token IF THEN ELSE SWITCH LOOP WHILE FOR LABEL LIKE BREAK RETURN CONTINUE ASYNC
%token AWAIT ARROW
%token FUNC TYPE ACTOR CLASS PRIVATE
%token SEMICOLON COLON COMMA DOT
%token ASSERT
%token EOF
%token ADDOP SUBOP MULOP DIVOP MODOP ANDOP OROP XOROP NOTOP SHIFTLOP SHIFTROP
%token ROTLOP ROTROP
%token CATOP
/* comparisons? */
%token LT GT
%token NULL
%token<string> NAT
%token<int>  INT
%token<float>  FLOAT
%token<char>   CHAR
%token<string> STRING
%token<int>  WORD
%token<bool>   BOOL
%token<string> ID
%token<string> TEXT
(* %token<string Source.phrase -> Ast.instr' * Values.value> CONST *)

%left ADDOP SUBOP
%left MULOP DIVOP
%left OROP
%left ANDOP
%left XOROP
%left CATOP  //?
%nonassoc USUBOP


%start<Syntax.typ> typ
%%

/* Helpers */

option(V) :
  | /* empty */ { None @@ at() }
  | v=V { (Some v) @@ at() } 


list(V) :
  | /* empty */ { [] @@ at() }
  | v = V vs = list(V) { (v::vs.it) @@ at() }

seplist(V,Sep) :
  | /* empty */ { [] @@ at() }
  | v = V Sep vs = list(V) { (v::vs.it) @@ at() } 
  | v = V { [v] @@ at() }
 
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
  | a = actor? LCURLY tfs = seplist(typ_field,SEMICOLON ) RCURLY
    {
       let actor =
       	   (match a.it with Some _ -> Actor | _ -> Object)
            @@ a.at
       in
          ObjT (actor,tfs.it)
          @@ at()
    }
  | vo = VAR? t = typ LBRACKET RBRACKET
    {
	ArrayT((match vo.it with
	        | Some _ -> VarMut @@ vo.at
	        | None -> ConstMut @@ vo.at),
	       t)
	@@ at()
    }
  | tpo = typ_params? t1 = typ ARROW t2 = typ 
    {
	FuncT((match tpo.it with
	       | Some tp -> tp
	       | None -> []),
	      t1,
	      t2)
	@@ at()
    }
  | LPAR ts = seplist(typ_item,COMMA) RPAR {TupT (ts.it) @@ at()}
  | ASYNC t = typ {  AsyncT t @@ at() }
  | LIKE t = typ {  LikeT t @@ at() }


typ_item :
  | ID COLON t=typ { t }
  | t=typ { t }
   
actor:
  | ACTOR { Actor @@ at()}

typ_args :
  | LT ts = seplist(typ,COMMA) GT { ts.it }

typ_params :
  | LT ts = seplist(typ_bind,COMMA) GT { ts.it }

typ_bind :
  | id = id { {var=id; bound = anyT @@ at()} @@ at() }
//| ID :> typ 
typ_field :
  | id = id COLON t=typ
     { {var = id; typ = t; mut = ConstMut @@ no_region} @@ at() }
  | VAR id = id COLON t=typ
     { {var = id; typ = t; mut = VarMut @@ no_region} @@ at() }
//TODO
//  | id = id typ_params? <params>+ COLON t=typ
//     { {var = id; typ = t; mut = VarMut @@ no_region} @@ at() }


lit :
    | NULL	  { NullLit}
    | i = INT     { IntLit i}
    | b = BOOL	  { BoolLit b}
    | n = NAT	  { NatLit n}
    | f = FLOAT   { FloatLit f}
    | w = WORD    { WordLit w}
    | c = CHAR	  { CharLit c}
    | t = TEXT    { TextLit t}

unop :
    | ADDOP { PosOp }
    | SUBOP { NegOp } 
    | NOTOP { NotOp }

%inline binop :
    | ADDOP { AddOp}
    | SUBOP { SubOp}
    | DIVOP { DivOp}
    | MODOP { ModOp}
    | ANDOP { AndOp}
    | OROP  { OrOp }
    | XOROP { XorOp }
    | SHIFTLOP { ShiftLOp }
    | SHIFTROP { ShiftROp }
    | ROTLOP   { RotLOp }
    | ROTROP   { RotROp }
    | CATOP    { CatOp }

expr :
      id = id { VarE id @@ at() }
    | l = lit { LitE l @@ at() }
    | uop = unop e = expr { UnE (uop,e) @@ at()}
    | e1 = expr bop = binop { BinOp (e1, binop, e2) @@ at() }
    

%%
