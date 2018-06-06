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
%token<Types.nat> NAT
%token<int>  INT
%token<float>  FLOAT
%token<Types.unicode>   CHAR
%token<string> STRING
%token<Types.word>  WORD
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

%start<Syntax.prog> prog
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

//TBR
param:
  | t = typ { t }

//TODO:  the informal grammar is actually, but I'm not sure how to understand <params>+
//  | id = id typ_params? <params>+ COLON t=typ
//     { {var = id; typ = t; mut = VarMut @@ no_region} @@ at() }
fun_spec :
  | id = id tpo = typ_params? p = param t = return_typ 
    {	let tps = match tpo.it with
    	    	  | Some tp -> tp
		  | None -> [] in
	(id,(FuncT(tps,p,t) @@ {left = tpo.at.left;right=t.at.right}))
    }

typ_bind :
  | id = id { {var=id; bound = anyT @@ at()} @@ at() }
//| ID :> typ 
typ_field :
  | id = id COLON t=typ
     { {var = id; typ = t; mut = ConstMut @@ no_region} @@ at() }
  | VAR id = id COLON t=typ
     { {var = id; typ = t; mut = VarMut @@ no_region} @@ at() }
  | fs = fun_spec
    { let (id,t) = fs in
      {var = id; typ = t; mut = ConstMut @@ no_region} @@ at() 
    }
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
    // wouldn't it be better for BinE to have to exp arguments?
    | e1 = expr bop = binop e2 = expr { BinE (bop,TupE [e1;e2] @@ no_region) @@ at() }
    | LPAR es = seplist(expr,COMMA) RPAR
      { match es.it with
        | [e] -> e
        | es -> TupE(es) @@ at()
      }
    | e=expr DOT n = NAT {ProjE (e,n) @@ at() }
    | a = actor? ido = id? LCURLY es = seplist(expr_field,SEMICOLON) RCURLY
      {
         let actor =
       	   (match a.it with Some _ -> Actor | _ -> Object)
            @@ a.at in
	 let id =  
       	    match ido.it with Some id -> Some id | _ -> None  in
	 ObjE(actor,id,es.it) @@ at()
      }
    | e=expr DOT id = id {DotE (e,id) @@ at() }
    | e1 = expr ASSIGN e2 = expr { AssignE(e1,e2) @@ at()}
    | LBRACKET es = seplist(expr,SEMICOLON) RBRACKET { ArrayE(es.it) @@ at() }
    | e1=expr LBRACKET e2=expr RBRACKET { IdxE(e1,e2) @@ at() }
    | e1=expr e2=expr { CallE(e1,e2) @@ at() }
    | LCURLY es = seplist(expr,SEMICOLON) RCURLY { BlockE(es.it) @@ at() }
    | NOT e = expr { NotE e @@ at() }
    | e1 = expr AND e2 = expr { AndE(e1,e2) @@ at() }
    | e1 = expr OR e2 = expr { OrE(e1,e2) @@ at() }
    | IF b=expr THEN e1=expr %prec IFX { IfE (b,e1,BlockE([]) @@ no_region) @@ at() }
    | IF b=expr THEN e1=expr ELSE e2=expr { IfE(b,e1,e2) @@ at() }
    | SWITCH e=expr cs = case+  { SwitchE(e,cs) @@ at()}
    | WHILE b=expr e=expr { WhileE(b,e) @@ at() }
    | LOOP e=expr eo=expr? { LoopE(e,eo.it) @@ at() }
    | FOR p = pat IN e1=expr e2=expr { ForE(p,e1,e2) @@ at() }
    | LABEL id = id e = expr { LabelE (id,e) @@ at() }
    | BREAK id = id eo = expr?
      {
        let es =
	    match eo.it with
	    | Some e -> [e]
	    | None -> [] in	    
      	BreakE (id,es) @@ at()
      }
    | CONTINUE id = id { ContE id @@ at() }
    | RETURN eo = expr?
      {
        let es =
	    match eo.it with
	    | Some e -> [e]
	    | None -> [] in	    
      	RetE es @@ at()
      }
    | ASYNC e = expr { AsyncE e @@ at() }
    | AWAIT e = expr { AwaitE e @@ at() }
    | ASSERT e = expr { AssertE e @@ at() }
    | e = expr IS t = typ { IsE(e,t) @@ at() }
    | e = expr COLON t = typ { AnnotE(e,t) @@ at() }
    | d=dec { DecE d @@ at() }
    
case : 
  | CASE p=pat e=expr { {pat = p; exp = e} @@ at() }

typ_annot :
  | COLON t=typ { t }

privacy :
  | PRIVATE { Private @@ at() }
  | /* empty */ { Public @@ at() }

mutability :
  | VAR { VarMut @@ at() }
  | /* empty */ { ConstMut @@ at() }

expr_field:
  | p=privacy m=mutability id = id ot = typ_annot? EQ e=expr
    {
	let e = match ot.it with
                | Some t -> AnnotE(e,t) @@ {left=t.at.left;right=e.at.right}
                | None -> e in
	{  var = id; mut = m; priv=p; exp = e}
	@@ at()
    }
// TBR: should a func_def abbreviate a dec or block {dec;id}? *)
  | p=privacy fd=func_def
     {
    	let (id,tps,pat,t,e) = fd.it in
	let dec = FuncD(id,tps,pat,t,e) @@ fd.at in
	let exp = DecE(dec) @@ fd.at in 
	{  var = id; mut = ConstMut @@ no_region; priv = p; exp = exp}
	@@ at() 
     }

atpat :
  | p = pat COLON t=typ { AnnotP(p,t) @@ at() }
  | p = pat { p }
  
pat :
  | UNDERSCORE { WildP @@ at() }
  | id = id { VarP(id) @@ at() }
  | LPAR ps  = seplist(atpat,COMMA) RPAR
      { match ps.it with
        | [p] -> p
        | ps -> TupP(ps) @@ at()
      }

init :  
  | EQ e=expr { e }

return_typ :
  | COLON t=typ { t }

//TBR: do we want id _ ... d x ... or id (x,...).
// if t is NONE, should it default to unit or is it inferred from expr?
func_def :
  | id = id tpo = typ_params? p = pat t = return_typ? EQ e=expr 
    {	let tps = match tpo.it with
    	    	  | Some tp -> tp 
		  | None -> [] in
        let t = match t.it with
	        | Some t -> t
	        | None -> TupT([]) @@ no_region in
	(id,tps,p,t,e)
	@@ at()
    }

dec :
  | LET p = pat e = expr { LetD (p,e) @@ at() }
  | VAR id = id COLON t=typ eo = init?
    { VarD(id,t,
           match eo.it with
	   | Some e -> Some e
	   | None -> None)
      @@ at() } 
  | FUNC fd = func_def
    {	let (id,tps,p,t,e) = fd.it in
	FuncD(id,tps,p,t,e) @@ at()
    }
  | TYPE id = id tpso = typ_params? EQ t=typ
    {	let tps = match tpso.it with
    	    	  | Some tps -> tps
		  | None -> [] in
        TypD(id,tps,t)
	@@ at()
    }

  | a = actor? CLASS id = id tpso = typ_params? p=pat EQ e=expr
    {
        let actor =
       	   (match a.it with Some _ -> Actor | _ -> Object)
            @@ a.at in
	let tps = match tpso.it with
    	    	  | Some tps -> tps
		  | None -> [] in
        ClassD(actor,id,tps,p,e)
	@@ at()
    }

prog:
    ds = dec* { ds }

 


%%
