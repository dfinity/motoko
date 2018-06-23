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

let at(symbolstartpos,endpos) =
  positions_to_region (symbolstartpos) (endpos)

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

%token EOF

%token MUT LET VAR
%token LPAR RPAR LBRACKET RBRACKET LCURLY RCURLY
%token AWAIT ASYNC BREAK CASE CONTINUE IF IN IS THEN ELSE SWITCH LOOP WHILE FOR LABEL LIKE RETURN 
%token ARROW ASSIGN
%token FUNC TYPE ACTOR CLASS PRIVATE
%token SEMICOLON COLON COMMA DOT
%token AND OR NOT 
%token ASSERT
%token OBJECT //TBR delete me?
%token ADDOP SUBOP MULOP DIVOP MODOP ANDOP OROP XOROP NOTOP SHIFTLOP SHIFTROP
%token ROTLOP ROTROP
%token<Types.binop> BINUPDATE 
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

%token<Types.prim> PRIM

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
//%start<Syntax.typ> typ

%%

/* Helpers */

option(V) :
  | /* empty */ { None @@ at($symbolstartpos,$endpos) }
  | v=V { (Some v) @@ at($symbolstartpos,$endpos) } 


list(V) :
  | /* empty */ { [] @@ at($symbolstartpos,$endpos) }
  | v = V vs = list(V) { (v::vs.it) @@ at($symbolstartpos,$endpos) }

seplist(V,Sep) :
  | /* empty */ { [] @@ at($symbolstartpos,$endpos) }
  | v = V Sep vs = seplist(V,Sep) { (v::vs.it) @@ at($symbolstartpos,$endpos) } 
  | v = V { [v] @@ at($symbolstartpos,$endpos) }
 
id :
  | id = ID { id @@ at($symbolstartpos,$endpos)}

sort:
  | ACTOR  { Some () @@ at($symbolstartpos,$endpos) }
  | OBJECT { None @@ at($symbolstartpos,$endpos) }


/* Types */

atomic_typ:
  | p=PRIM { PrimT p @@ at($symbolstartpos,$endpos) }
/*  | id = id  { VarT (id,[]) @@ at($symbolstartpos,$endpos) }  
  | id = id LT ts = seplist(typ,COMMA) GT { VarT (id,ts.it) @@ at($symbolstartpos,$endpos) }
 */
  | LPAR ts = seplist(typ_item,COMMA) RPAR {TupT (ts.it) @@ at($symbolstartpos,$endpos)}
  | ASYNC t = typ {  AsyncT t @@ at($symbolstartpos,$endpos) }
  | LIKE t = typ {  LikeT t @@ at($symbolstartpos,$endpos) }
  |  id = id ots = typ_args? {
     	VarT (id,
	      match ots.it with
	       Some ts -> ts | None -> [] )
	@@ at($symbolstartpos,$endpos)
     }

typ :
  | t = atomic_typ {t}
  | a = sort LCURLY tfs = seplist(typ_field,SEMICOLON) RCURLY
    {
       let actor =
       	   (match a.it with Some _ -> Actor | _ -> Object)
            @@ a.at
       in
          ObjT (actor,tfs.it)
          @@ at($symbolstartpos,$endpos)
    }
/*
  | vo = VAR? t = atomic_typ LBRACKET RBRACKET
    {
	ArrayT((match vo.it with
	        | Some _ -> VarMut @@ vo.at
	        | None -> ConstMut @@ vo.at),
	       t)
	@@ at($symbolstartpos,$endpos)
    }
 */
  | tpo = typ_params? t1 = atomic_typ ARROW t2 = typ 
    {
	FuncT((match tpo.it with
	       | Some tp -> tp
	       | None -> []),
	      t1,
	      t2)
	@@ at($symbolstartpos,$endpos)
    }

typ_item :
  | ID COLON t=typ { t }
  | t=typ { t }
   
actor:
  | ACTOR { Some () @@ at($symbolstartpos,$endpos)}

typ_args :
  | LT ts = seplist(typ,COMMA) GT { ts.it }

typ_params :
  | LT ts = seplist(typ_bind,COMMA) GT { ts.it }

//TBR
param:
  | id = id COLON t = typ { AnnotP(VarP(id) @@ id.at,t) @@ at($symbolstartpos,$endpos)  }

params :
  | LPAR ps = seplist(param,COMMA) RPAR {
    (match ps.it with
    | [p] -> p.it
    | ps -> TupP(ps)) @@ at($symbolstartpos,$endpos)}

//TBR:  the informal grammar is actually as below but I'm not sure how to understand <params>+
//  | id = id typ_params? <params>+ COLON t=typ
//     { {var = id; typ = t; mut = VarMut @@ no_region} @@ at($symbolstartpos,$endpos) }
fun_spec :
  | id = id tpo = typ_params? t1 = typ t2 = return_typ 
    {	let tps = match tpo.it with
    	    	  | Some tp -> tp
		  | None -> [] in
	(id,(FuncT(tps,t1,t2) @@ {left = tpo.at.left;right=t2.at.right}))
    }

typ_bind :
  | id = id { {var=id; bound = anyT @@ at($symbolstartpos,$endpos)} @@ at($symbolstartpos,$endpos) }
//| ID :> typ 
typ_field :
  | id = id COLON t=typ
     { {var = id; typ = t; mut = ConstMut @@ no_region} @@ at($symbolstartpos,$endpos) }
  | VAR id = id COLON t=typ
     { {var = id; typ = t; mut = VarMut @@ no_region} @@ at($symbolstartpos,$endpos) }
  | fs = fun_spec
    { let (id,t) = fs in
      {var = id; typ = t; mut = ConstMut @@ no_region} @@ at($symbolstartpos,$endpos) 
    }
//  | id = id typ_params? <params>+ COLON t=typ
//     { {var = id; typ = t; mut = VarMut @@ no_region} @@ at($symbolstartpos,$endpos) }


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

block_expr :
    | LCURLY es = seplist(expr,SEMICOLON) RCURLY { BlockE(es.it) @@ at($symbolstartpos,$endpos) }

atomic_expr :
    | id = id { VarE id @@ at($symbolstartpos,$endpos) }
    | l = lit { LitE l @@ at($symbolstartpos,$endpos) }
    | LPAR es = seplist(expr,COMMA) RPAR
      { match es.it with
        | [e] -> e
        | es -> TupE(es) @@ at($symbolstartpos,$endpos)
      }

    | a = sort ido = id? LCURLY es = seplist(expr_field,SEMICOLON) RCURLY
      {
         let actor =
       	   (match a.it with Some _ -> Actor | _ -> Object)
            @@ a.at in
	 let id =  
       	    match ido.it with Some id -> Some id | _ -> None  in
	 ObjE(actor,id,es.it) @@ at($symbolstartpos,$endpos)
      }

    | LBRACKET es = seplist(expr,SEMICOLON) RBRACKET { ArrayE(es.it) @@ at($symbolstartpos,$endpos) }
    | e=atomic_expr DOT n = NAT {ProjE (e,n) @@ at($symbolstartpos,$endpos) }
    | e=atomic_expr DOT id = id {DotE (e,id) @@ at($symbolstartpos,$endpos) }
    | e1=atomic_expr e2=atomic_expr { CallE(e1,e2) @@ at($symbolstartpos,$endpos) }
    | e=block_expr { e }
    | LABEL id = id e = expr { LabelE (id,e) @@ at($symbolstartpos,$endpos) }
    | BREAK id = id eo = expr?
      {
        let es =
	    match eo.it with
	    | Some e -> [e]
	    | None -> [] in	    
      	BreakE (id,es) @@ at($symbolstartpos,$endpos)
      }
    | CONTINUE id = id { ContE id @@ at($symbolstartpos,$endpos) }
  
expr :
    | e=atomic_expr { e } 
    | e1 = expr bop = binop e2 = expr { BinE (bop,TupE [e1;e2] @@ no_region) @@ at($symbolstartpos,$endpos) }
    | e1 = expr ASSIGN e2 = expr { AssignE(e1,e2) @@ at($symbolstartpos,$endpos)}
    | e1 = expr binop=BINUPDATE e2 = expr {
        AssignE(e1,BinE(binop,TupE [e1;e2] @@ no_region) @@ no_region) @@ at($symbolstartpos,$endpos)}
    | e1=expr LBRACKET e2=expr RBRACKET { IdxE(e1,e2) @@ at($symbolstartpos,$endpos) }
    | NOT e = expr { NotE e @@ at($symbolstartpos,$endpos) }
    | e1 = expr AND e2 = expr { AndE(e1,e2) @@ at($symbolstartpos,$endpos) }
    | e1 = expr OR e2 = expr { OrE(e1,e2) @@ at($symbolstartpos,$endpos) }
    | IF b=expr THEN e1=expr %prec IFX { IfE (b,e1,BlockE([]) @@ no_region) @@ at($symbolstartpos,$endpos) }
    | IF b=expr THEN e1=expr ELSE e2=expr { IfE(b,e1,e2) @@ at($symbolstartpos,$endpos) }
    | SWITCH e=expr cs = case+  { SwitchE(e,cs) @@ at($symbolstartpos,$endpos)}
    | WHILE LPAR b=expr RPAR e=expr { WhileE(b,e) @@ at($symbolstartpos,$endpos) }
    | LOOP e=expr { LoopE(e,None) @@ at($symbolstartpos,$endpos) }
    | LOOP e=expr WHILE LPAR b=expr RPAR { LoopE(e,Some b) @@ at($symbolstartpos,$endpos) }
    | FOR p = pat IN e1=expr e2=expr { ForE(p,e1,e2) @@ at($symbolstartpos,$endpos) }
    | RETURN eo = expr?
      {
        let es =
	    match eo.it with
	    | Some e -> [e]
	    | None -> [] in	    
      	RetE es @@ at($symbolstartpos,$endpos)
      }
    | ASYNC e = expr { AsyncE e @@ at($symbolstartpos,$endpos) }
    | AWAIT e = expr { AwaitE e @@ at($symbolstartpos,$endpos) }
    | ASSERT e = expr { AssertE e @@ at($symbolstartpos,$endpos) }
    | e = expr IS t = typ { IsE(e,t) @@ at($symbolstartpos,$endpos) }
    | e = expr COLON t = typ { AnnotE(e,t) @@ at($symbolstartpos,$endpos) }
    | d=dec { DecE d @@ at($symbolstartpos,$endpos) }
    
case : 
  | CASE p=pat e=expr { {pat = p; exp = e} @@ at($symbolstartpos,$endpos) }

typ_annot :
  | COLON t=typ { t }

%inline privacy :
  | PRIVATE { Private @@ at($symbolstartpos,$endpos) }
  | /* empty */ { Public @@ at($symbolstartpos,$endpos) }

%inline mutability :
  | VAR { VarMut @@ at($symbolstartpos,$endpos) }
  | /* empty */ { ConstMut @@ at($symbolstartpos,$endpos) }

expr_field:
  | p=privacy  m=mutability id = id ot = typ_annot? EQ e=expr
    {
	let e = match ot.it with
                | Some t -> AnnotE(e,t) @@ {left=t.at.left;right=e.at.right}
                | None -> e in
	{  var = id; mut = m; priv=p; exp = e}
	@@ at($symbolstartpos,$endpos)
    }
// TBR: should a func_def abbreviate a dec or block {dec;id}? *)
  | p=privacy fd=func_def
     {
    	let (id,tps,pat,t,e) = fd.it in
	let dec = FuncD(id,tps,pat,t,e) @@ fd.at in
	let exp = DecE(dec) @@ fd.at in 
	{  var = id; mut = ConstMut @@ no_region; priv = p; exp = exp}
	@@ at($symbolstartpos,$endpos) 
     }
atpat :
  | p = pat COLON t=typ { AnnotP(p,t) @@ at($symbolstartpos,$endpos) }
  | p = pat { p }
  | l = lit { LitP l @@ at($symbolstartpos,$endpos) }
  
pat :
  | UNDERSCORE { WildP @@ at($symbolstartpos,$endpos) }
  | id = id { VarP(id) @@ at($symbolstartpos,$endpos) }
  | LPAR ps  = seplist(atpat,COMMA) RPAR
      { match ps.it with
        | [p] -> p
        | ps -> TupP(ps) @@ at($symbolstartpos,$endpos)
      }

init :  
  | EQ e=expr { e }

return_typ :
  | COLON t=typ { t }

//TBR: do we want id _ ... d x ... or id (x,...).
// if t is NONE, should it default to unit or is it inferred from expr?
func_def :
  | id = id tpo = typ_params? p = params t = return_typ? fb=func_body
    {	let tps = match tpo.it with
    	    	  | Some tp -> tp 
		  | None -> [] in
        let t = match t.it with
	        | Some t -> t
	        | None -> TupT([]) @@ no_region in

        (* this is a hack to support async method declarations *)
	let e = match fb with
	        | (false,e) -> (* body declared as EQ e *)
		   e
	        | (true,e) -> (* body declared as immediate block *)
		  (match t.it with
		   | AsyncT _ -> AsyncE(e) @@ no_region
		   | _ -> e)
	in
	(id,tps,p,t,e)
	@@ at($symbolstartpos,$endpos)
    }

func_body :
   | EQ e = expr { (false,e) }	  // acc. to grammar
   | e = block_expr { (true,e) } // acc. to example bank.as 

dec :
  | LET p = pat EQ e = expr { LetD (p,e) @@ at($symbolstartpos,$endpos) }
  | VAR id = id COLON t=typ eo = init?
    { VarD(id,t,
           match eo.it with
	   | Some e -> Some e
	   | None -> None)
      @@ at($symbolstartpos,$endpos) } 
  | FUNC fd = func_def
    {	let (id,tps,p,t,e) = fd.it in
	FuncD(id,tps,p,t,e) @@ at($symbolstartpos,$endpos)
    }
  | TYPE id = id tpso = typ_params? EQ t=typ
    {	let tps = match tpso.it with
    	    	  | Some tps -> tps
		  | None -> [] in
        TypD(id,tps,t)
	@@ at($symbolstartpos,$endpos)
    }

/* TBR: Syntax.md specifies EQ expr but the examples allow a expr_field* (sans EQ), shall we allow both?
  | a = sort CLASS id = id tpso = typ_params? p=pat EQ e=expr
    {
        let actor =
       	   (match a.it with Some _ -> Actor | _ -> Object)
            @@ a.at in
	let tps = match tpso.it with
    	    	  | Some tps -> tps
		  | None -> [] in
        ClassD(actor,id,tps,p,e)
	@@ at($symbolstartpos,$endpos)
    }
*/
  | a = sort CLASS id = id tpso = typ_params? p=params LCURLY efs = seplist(expr_field,SEMICOLON) RCURLY
    {
        let actor =
       	   (match a.it with Some _ -> Actor | _ -> Object)
            @@ a.at in
	let tps = match tpso.it with
    	    	  | Some tps -> tps
		  | None -> [] in
        ClassD(actor,id,tps,p,efs.it)
	@@ at($symbolstartpos,$endpos)
    }


prog:
    ds = seplist(dec,SEMICOLON) EOF { ds }

%%
