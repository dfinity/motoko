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
%token AWAIT ASYNC BREAK CASE CONTINUE IF IN IS THEN ELSE SWITCH LOOP WHILE FOR LABEL LIKE RETURN 
%token ARROW ASSIGN
%token FUNC TYPE ACTOR CLASS PRIVATE
%token SEMICOLON COLON COMMA DOT
%token AND OR NOT 
%token ASSERT
%token EOF
%token ADDOP SUBOP MULOP DIVOP MODOP ANDOP OROP XOROP NOTOP SHIFTLOP SHIFTROP
%token ROTLOP ROTROP
%token CATOP
/* comparisons? */
%token EQ LT GT
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

%token UNDERSCORE

%left ADDOP SUBOP
%left MULOP DIVOP
%left OROP
%left ANDOP
%left XOROP
%left CATOP  //?
%nonassoc USUBOP


%nonassoc IFX
%nonassoc ELSE

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

expr_rest :
    | RPAR { fn e -> e }

expr :
      id = id { VarE id @@ at() }
    | l = lit { LitE l @@ at() }
    | uop = unop e = expr { UnE (uop,e) @@ at()}
    | e1 = expr bop = binop { BinOp (e1, binop, e2) @@ at() }
    | LPAR es  = seplist(expr,COMMA) RPAR
      { match es.it with
        | [e] -> e
        | es -> TupE(es) @@ at()
      }
    | e=expr DOT n = NAT {ProjE (e,n) @@ at() }
    | actor? id? LCURLY seplist(expr_field,SEMICOLON) RCURLY
      {
         
      }
    | e=expr DOT id = id {DotE (e,id) @@ at() }
    | e1 = expr ASSIGN e2 = expr { AssignE(e1,e2) @@ at()}
    | LBRACKET es = seplist(expr,SEMICOLON) RBRACKET { Array(es) @@ at() }
    | e1=expr LBRACKET e2=expr RBRACKET { ArrayE(e1,e2) @@ at() }
    | e1=expr e2=expr { CallE(e1,e2) @@ at() }
    | LCURLY es = seplist(expr,SEMICOLON) RCURLY { BlockE(es) @@ at() }
    | NOT e = expr { NotE e @@ at() }
    | e1 = expr AND e2 = expr { NotE(e1,e2) @@ at() }
    | e1 = expr OR e2 = expr { AndE(e1,e2) @@ at() }
    | IF b=expr THEN e1=expr %prec IFX { IfE (b,e1,BlockE([]) @@ noRegion) @@ at() }
    | IF b=expr THEN e1=expr ELSE e2=expr { IfE(b,e1,e2) @@ at() }
    | SWITCH e=expr cs = case+  { SwitchE(e,cs) @@ at()}
    | WHILE b=expr e=expr { WhileE(b,e) @@ at() }
    | LOOP e=expr eo=expr? { LoopE(e,eo.it) @@ at() }
    | FOR e0=expr IN e1=expr e2=expr { ForE(e0,e1,e2) @@ at() }
    | LABEL id = id e = expr { LabelE (id,e) @@ at() }
    | BREAK id = id eo= expr? { BreakE (id,e) @@ at() }
    | RETURN eo= expr? { ReturnE eo @@ at() }
    | ASYNC e = expr { AsyncE e @@ at() }
    | ASSERT e = expr { AssertE e @@ at() }
    | e = expr IS t = typ { IsE(e,t) @@ at() }
    | e = expr COLON t = typ { AnnotE(e,t) @@ at() }
    | d=dec { DecE d @@ at() }
    
case : 
  | CASE p=pat e=expr { {pat = p; e=expr} @@ at() }

atpat :
  | p = pat COLON t=typ { AnnotP(p,t) @@ at() }
  | p = pat { p }
  
pat :
  | UNDERSCORE { WildP @@ at() }
  | id { VarP(it) @@ at() }
  | LPAR ps  = seplist(atpat,COMMA) RPAR
      { match ps.it with
        | [p] -> p
        | ps -> TupP(es) @@ at()
      }

init :
  | EQ e=expr { e }

return_typ :
  | COLON t=typ { t }

dec :
  | LET p = pat e = expr { LetD (p,e) @@ at() }
  | VAR id = id COLON t=typ eo = init?
    { VarD(id,t,
           match eo.it with
	   | Some e -> Some e
	   | None -> None)
      @@ at() } 
//TBR: do we want func id _ ... func id x ... or just func id (x,...).
// if t is NONE, should it default to unit or is it inferred from expr?
  | FUNC id = id tpo = typ_params? p = pat t = return_typ? EQ e=expr 
    {	let tps = match typ with
    	    	  | Some tp -> tp
		  | None -> [] @@ noRegion in
        let t = match t with
	        | Some t -> t
	        | None -> TupT([]) @@ noRegion in
	FuncD(id,tps,p,t,e)
	@@ at()
    }
  | TYPE id = id tpo = typ_params? EQ t=typ
    {	let tps = match typ with
    	    	  | Some tp -> tp
		  | None -> [] @@ noRegion in
        TypD(id,tps,t)
	@@ at()
    }

  | a = actor? CLASS id = id tpo = typ_params? p=pat EQ e=expr
    {
        let actor =
       	   (match a.it with Some _ -> Actor | _ -> Object)
            @@ a.at
	let tps = match typ with
    	    	  | Some tp -> tp
		  | None -> [] @@ noRegion in
        ClassT(actor,id,tps,p,e)
	@@ at()
    }


 


%%
