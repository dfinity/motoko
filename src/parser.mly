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
    let _ = String.iter (function '0'..'9' -> () | _ -> failwith "non-numeric digit") s in
    let n = int_of_string s in
    if n >= 0 then n else raise (Failure "")
  with Failure _ -> error at "integer constant out of range"

(* 
let nat32 s at =
  try i32.of_string_u s with Failure _ -> error at "i32 constant out of range"


let name s at =
  try Utf8.decode s with Utf8.Utf8 -> error at "invalid UTF-8 encoding"

*)

let (@?) x region = {it = x; at = region; note = Types.AnyT}
let (@!) x region = {it = x; at = region; note = Types.ConstMut}

%}

%token EOF

%token LET VAR
%token LPAR RPAR LBRACKET RBRACKET LCURLY RCURLY
%token AWAIT ASYNC BREAK CASE CONTINUE LABEL IF IN IS ELSE SWITCH LOOP WHILE FOR LIKE RETURN 
%token ARROW ASSIGN
%token FUNC TYPE ACTOR CLASS PRIVATE
%token SEMICOLON COLON COMMA DOT QUEST
%token AND OR NOT 
%token ASSERT
%token ADDOP SUBOP MULOP DIVOP MODOP
%token ANDOP OROP XOROP SHLOP SHROP ROTLOP ROTROP
%token EQOP NEQOP LEOP LTOP GTOP GEOP
%token CATOP
%token EQ LT GT
%token PLUSASSIGN MINUSASSIGN MULASSIGN DIVASSIGN MODASSIGN CATASSIGN
%token ANDASSIGN ORASSIGN XORASSIGN SHLASSIGN SHRASSIGN ROTLASSIGN ROTRASSIGN
%token NULL
%token<string> NAT
%token<string> INT
%token<float> FLOAT
%token<Types.unicode> CHAR
%token<bool> BOOL
%token<string> ID
%token<string> TEXT
// %token<string Source.phrase -> Ast.instr' * Values.value> CONST

%token<Types.prim> PRIM

%token UNDERSCORE

%nonassoc IFX
%nonassoc ELSE

%right ASSIGN PLUSASSIGN MINUSASSIGN MULASSIGN DIVASSIGN MODASSIGN CATASSIGN ANDASSIGN ORASSIGN XORASSIGN SHLASSIGN SHRASSIGN ROTLASSIGN ROTRASSIGN
%left IS COLON
%left OR
%left AND
%nonassoc EQOP NEQOP LEOP LTOP GTOP GEOP
%left ADDOP SUBOP CATOP
%left MULOP DIVOP MODOP
%left OROP
%left ANDOP
%left XOROP
%nonassoc SHLOP SHROP ROTLOP ROTROP
    
%type<Syntax.exp> expr expr_nullary
%start<Syntax.prog> prog

%%

(* Helpers *)

option(X) :
  | (* empty *) { None @@ at($symbolstartpos,$endpos) }
  | x=X { Some x @@ at($symbolstartpos,$endpos) } 


list(X) :
  | (* empty *) { [] @@ at($symbolstartpos,$endpos) }
  | x=X xs=list(X) { (x::xs.it) @@ at($symbolstartpos,$endpos) }

seplist(X, SEP) :
  | (* empty *) { [] @@ at($symbolstartpos,$endpos) }
  | x=X SEP xs=seplist(X, SEP) { (x::xs.it) @@ at($symbolstartpos,$endpos) } 
  | x=X { [x] @@ at($symbolstartpos,$endpos) }


(* Basics *)

%inline id :
  | id=ID { id @@ at($symbolstartpos,$endpos)}

%inline var_ref :
  | id=ID { id @! at($symbolstartpos,$endpos) }

%inline var :
  | VAR { VarMut @@ at($symbolstartpos,$endpos) }

%inline var_opt :
  | (* empty *) { ConstMut @@ at($symbolstartpos,$endpos) }
  | VAR { VarMut @@ at($symbolstartpos,$endpos) }

%inline actor_opt :
  | (* empty *) { Object @@ at($symbolstartpos,$endpos) }
  | ACTOR { Actor @@ at($symbolstartpos,$endpos) }


(* Types *)

typ_nullary :
  | p=PRIM
    { PrimT(p) @@ at($symbolstartpos,$endpos) }
(*
  | x=id 
    { VarT(x, []) @@ at($symbolstartpos,$endpos) }  
  | x=id LT ts=seplist(typ, COMMA) GT
    { VarT(x, ts.it) @@ at($symbolstartpos,$endpos) }
*)
  | LPAR ts=seplist(typ_item, COMMA) RPAR
    { match ts.it with [t] -> t | ts -> TupT(ts) @@ at($symbolstartpos,$endpos) }
  | x=id tso=typ_args?
    {	VarT(x, Lib.Option.get tso.it []) @@ at($symbolstartpos,$endpos) }
  | a=actor_opt LCURLY tfs=seplist(typ_field, SEMICOLON) RCURLY
    { ObjT(a, tfs.it) @@ at($symbolstartpos,$endpos) }

typ_post :
  | t=typ_nullary
    { t }
  | t=typ_post LBRACKET RBRACKET
    { ArrayT(ConstMut @@ no_region, t) @@ at($symbolstartpos,$endpos) }
  | t=typ_post QUEST
    { OptT(t) @@ at($symbolstartpos,$endpos) }

typ_pre :
  | t=typ_post
    { t }
  | ASYNC t=typ
    { AsyncT(t) @@ at($symbolstartpos, $endpos) }
  | LIKE t=typ
    { LikeT(t) @@ at($symbolstartpos, $endpos) }
  | mut=var t=typ_nullary LBRACKET RBRACKET
    { ArrayT(mut, t) @@ at($symbolstartpos,$endpos) }

typ :
  | t=typ_pre
    { t }
  | tps=typ_params_opt t1=typ_pre ARROW t2=typ 
    { FuncT(tps, t1, t2) @@ at($symbolstartpos,$endpos) }

typ_item :
  | id COLON t=typ { t }
  | t=typ { t }

typ_args :
  | LT ts=seplist(typ, COMMA) GT { ts.it }

%inline typ_params_opt :
  | (* empty *) { [] }
  | LT ts=seplist(typ_bind, COMMA) GT { ts.it }

typ_field :
  | mut=var_opt x=id COLON t=typ
    { {var = x; typ = t; mut} @@ at($symbolstartpos,$endpos) }
  | x=id tps=typ_params_opt t1=typ t2=return_typ 
    { let t = FuncT(tps, t1, t2) @@ span x.at t2.at in
      {var = x; typ = t; mut = ConstMut @@ no_region} @@ at($symbolstartpos,$endpos) }

typ_bind :
  | x=id
    { {var = x; bound = AnyT @@ at($symbolstartpos,$endpos)} @@ at($symbolstartpos,$endpos) }
// | ID :> typ 



(* Expressions *)

lit :
  | NULL { NullLit }
  | s=NAT { PreLit s }
  | s=INT { PreLit s }
  | b=BOOL { BoolLit b }
  | f=FLOAT { FloatLit (Float.of_float f)}
  | c=CHAR { CharLit c }
  | t=TEXT { TextLit t }

%inline unop :
  | ADDOP { PosOp }
  | SUBOP { NegOp }
  | XOROP { NotOp }

%inline binop :
  | ADDOP { AddOp }
  | SUBOP { SubOp }
  | MULOP { MulOp }
  | DIVOP { DivOp }
  | MODOP { ModOp }
  | ANDOP { AndOp }
  | OROP  { OrOp }
  | XOROP { XorOp }
  | SHLOP { ShiftLOp }
  | SHROP { ShiftROp }
  | ROTLOP { RotLOp }
  | ROTROP { RotROp }
  | CATOP { CatOp }

%inline relop :
  | EQOP  { EqOp }
  | NEQOP { NeqOp }
  | LTOP  { LtOp }
  | LEOP  { LeOp }
  | GTOP  { GtOp }
  | GEOP  { GeOp }

%inline unassign :
  | PLUSASSIGN { PosOp }
  | MINUSASSIGN { NegOp }
  | XORASSIGN { NotOp }

%inline binassign :
  | PLUSASSIGN { AddOp }
  | MINUSASSIGN { SubOp }
  | MULASSIGN { MulOp }
  | DIVASSIGN { DivOp }
  | MODASSIGN { ModOp }
  | ANDASSIGN { AndOp }
  | ORASSIGN { OrOp }
  | XORASSIGN { XorOp }
  | SHLASSIGN { ShiftLOp }
  | SHRASSIGN { ShiftROp }
  | ROTLASSIGN { RotLOp }
  | ROTRASSIGN { RotROp }
  | CATASSIGN { CatOp }


expr_nullary :
  | x=var_ref
    { VarE(x) @? at($symbolstartpos,$endpos) }
  | l=lit
    { LitE(ref l) @? at($symbolstartpos,$endpos) }
  | LPAR es = seplist(expr, COMMA) RPAR
    { match es.it with [e] -> e | es -> TupE(es) @? at($symbolstartpos,$endpos) }
  | LCURLY es=seplist(expr, SEMICOLON) RCURLY
    { BlockE(es.it) @? at($symbolstartpos,$endpos) }
  | a=actor_opt xo=id? LCURLY es=seplist(expr_field, SEMICOLON) RCURLY
    { ObjE(a, xo.it, es.it) @? at($symbolstartpos,$endpos) }

expr_post :
  | e=expr_nullary
    { e }
  | LBRACKET mut=var_opt es=seplist(expr, COMMA) RBRACKET
    { ArrayE(mut, es.it) @? at($symbolstartpos,$endpos) }
  | e1=expr_post LBRACKET e2=expr RBRACKET
    { IdxE(e1, e2) @? at($symbolstartpos,$endpos) }
  | e=expr_post DOT s=NAT
    { ProjE (e, Natural.of_string s) @? at($symbolstartpos,$endpos) }
  | e=expr_post DOT x=var_ref
    { DotE(e, x) @? at($symbolstartpos,$endpos) }
  | e1=expr_post tso=typ_args? e2=expr_nullary
    { CallE(e1, Lib.Option.get tso.it [], e2) @? at($symbolstartpos,$endpos) }

expr_pre :
  | e=expr_post
    { e } 
  | op=unop e=expr_pre
    { UnE(op ,e) @? at($symbolstartpos,$endpos) }
  | op=unassign e=expr_pre
    (* TODO: this is incorrect, since it duplicates e *)
    { AssignE(e, UnE(op, e) @? at($symbolstartpos,$endpos)) @? at($symbolstartpos,$endpos) }
  | NOT e=expr_pre
    { NotE e @? at($symbolstartpos,$endpos) }

expr_infix :
  | e=expr_pre
    { e } 
  | e1=expr_infix op=binop e2=expr_infix
    { BinE(e1, op, e2) @? at($symbolstartpos,$endpos) }
  | e1=expr_infix op=relop e2=expr_infix
    { RelE(e1, op, e2) @? at($symbolstartpos,$endpos) }
  | e1=expr_infix ASSIGN e2=expr_infix
    { AssignE(e1, e2) @? at($symbolstartpos,$endpos)}
  | e1=expr_infix op=binassign e2=expr_infix
    (* TODO: this is incorrect, since it duplicates e1 *)
    { AssignE(e1, BinE(e1, op, e2) @? at($symbolstartpos,$endpos)) @? at($symbolstartpos,$endpos) }
  | e1=expr_infix AND e2=expr_infix
    { AndE(e1, e2) @? at($symbolstartpos,$endpos) }
  | e1=expr_infix OR e2=expr_infix
    { OrE(e1, e2) @? at($symbolstartpos,$endpos) }
  | e=expr_infix IS t=typ
    { IsE(e, t) @? at($symbolstartpos,$endpos) }
  | e=expr_infix COLON t=typ
    { AnnotE(e, t) @? at($symbolstartpos,$endpos) }

expr :
  | e=expr_infix
    { e } 
  | LABEL x=id e=expr
    { let x' = ("continue " ^ x.it) @@ x.at in
      let e' =
        match e.it with
        | WhileE (e1, e2) -> WhileE (e1, LabelE (x', e2) @? e2.at) @? e.at
        | LoopE (e1, eo) -> LoopE (LabelE (x', e1) @? e1.at, eo) @? e.at
        | ForE (p, e1, e2) -> ForE (p, e1, LabelE (x', e2) @? e2.at) @? e.at
        | _ -> e
      in LabelE(x, e') @? at($symbolstartpos,$endpos) }
  | BREAK x=id eo=expr_nullary?
    { let e = Lib.Option.get eo.it (TupE([]) @? no_region) in
      BreakE(x, e) @? at($symbolstartpos,$endpos) }
  | CONTINUE x=id
    { let x' = ("continue " ^ x.it) @@ x.at in
      BreakE(x', TupE([]) @? no_region) @? at($symbolstartpos,$endpos) }
  | IF b=expr_nullary e1=expr %prec IFX
    { IfE(b, e1, TupE([]) @? no_region) @? at($symbolstartpos,$endpos) }
  | IF b=expr_nullary e1=expr ELSE e2=expr
    { IfE(b, e1, e2) @? at($symbolstartpos,$endpos) }
  | SWITCH e=expr_nullary cs=case+
    { SwitchE(e, cs) @? at($symbolstartpos,$endpos) }
  | WHILE e1=expr_nullary e2=expr
    { WhileE(e1, e2) @? at($symbolstartpos,$endpos) }
  | LOOP e=expr
    { LoopE(e, None) @? at($symbolstartpos,$endpos) }
  | LOOP e1=expr WHILE e2=expr
    { LoopE(e1, Some e2) @? at($symbolstartpos,$endpos) }
  | FOR p=pat IN e1=expr_nullary e2=expr
    { ForE(p, e1, e2) @? at($symbolstartpos,$endpos) }
  | RETURN eo=expr?
    { let e = Lib.Option.get eo.it (TupE([]) @? eo.at) in
    	RetE(e) @? at($symbolstartpos,$endpos) }
  | ASYNC e=expr 
    { AsyncE(e) @? at($symbolstartpos,$endpos) }
  | AWAIT e=expr
    { AwaitE(e) @? at($symbolstartpos,$endpos) }
  | ASSERT e=expr
    { AssertE(e) @? at($symbolstartpos,$endpos) }
  | d=dec
    { DecE(d) @? at($symbolstartpos,$endpos) }
      
    
case : 
  | CASE p=pat e=expr
    { {pat = p; exp = e} @@ at($symbolstartpos,$endpos) }

%inline private_opt :
  | (* empty *) { Public @@ at($symbolstartpos,$endpos) }
  | PRIVATE { Private @@ at($symbolstartpos,$endpos) }

expr_field :
  | p=private_opt m=var_opt x=id EQ e=expr
    { {var = x; mut = m; priv = p; exp = e} @@ at($symbolstartpos,$endpos) }
  | p=private_opt m=var_opt x=id COLON t=typ EQ e=expr
    { {var = x; mut = m; priv = p; exp = AnnotE(e, t) @? span t.at e.at}
	    @@ at($symbolstartpos,$endpos) }
  // TBR: should a func_def abbreviate a dec or block {dec;id}? *)
  | priv=private_opt fd=func_def
    { let (x, tps, p, t, e) = fd.it in
      let d = FuncD(x, tps, p, t, e) @@ fd.at in
      let e' = DecE(d) @? fd.at in  
        (* let e' = BlockE([DecE(d)@? fd.at;(VarE (x.it @! fd.at)) @? fd.at]) @? fd.at in  *)
      {var = x; mut = ConstMut @@ no_region; priv; exp = e'}
      @@ at($symbolstartpos,$endpos) }

// TBR
param :
  | x=id COLON t=typ
    { AnnotP(VarP(x) @@ x.at, t) @@ at($symbolstartpos,$endpos) }

params :
  | LPAR ps=seplist(param, COMMA) RPAR
    { match ps.it with [p] -> p | ps -> TupP(ps) @@ at($symbolstartpos,$endpos) }


(* Patterns *)

pat :
  | p=pat COLON t=typ
    { AnnotP(p, t) @@ at($symbolstartpos,$endpos) }
  | l=lit
    { LitP(ref l) @@ at($symbolstartpos,$endpos) }
  | UNDERSCORE
    { WildP @@ at($symbolstartpos,$endpos) }
  | x=id
    { VarP(x) @@ at($symbolstartpos,$endpos) }
  | LPAR ps=seplist(pat, COMMA) RPAR
    { match ps.it with [p] -> p | ps -> TupP(ps) @@ at($symbolstartpos,$endpos) }

init :  
  | EQ e=expr { e }

return_typ :
  | COLON t=typ { t }

//TBR: do we want id _ ... d x ... or id (x,...).
// if t is NONE, should it default to unit or is it inferred from expr?
func_def :
  | x=id tps=typ_params_opt ps=params rt=return_typ? fb=func_body
    {	let t = Lib.Option.get rt.it (TupT([]) @@ rt.at) in
      (* This is a hack to support async method declarations. *)
	    let e = match fb with
	      | (false, e) -> e (* body declared as EQ e *)
	      | (true, e) -> (* body declared as immediate block *)
		      match t.it with
		      | AsyncT _ -> AsyncE(e) @? e.at
		      | _ -> e
	    in (x, tps, ps, t, e) @@ at($symbolstartpos,$endpos) }

func_body :
   | EQ e=expr { (false, e) }	  // acc. to grammar
   | e=expr { (true, e) } // acc. to example bank.as 


(* Declarations *)

dec :
  | LET p=pat EQ e=expr
    { LetD (p,e) @@ at($symbolstartpos,$endpos) }
  | VAR x=id COLON t=typ eo=init?
    { VarD(x, t, eo.it) @@ at($symbolstartpos,$endpos) } 
  | FUNC fd=func_def
    { let (id, tps, p, t, e) = fd.it in
      FuncD(id,tps,p,t,e) @@ at($symbolstartpos,$endpos) }
  | TYPE x=id tps=typ_params_opt EQ t=typ
    { TypD(x, tps, t) @@ at($symbolstartpos,$endpos) }
(* TBR: Syntax.md specifies EQ expr but the examples allow a expr_field* (sans EQ), shall we allow both?
  | a=actor_opt CLASS x=id tps=typ_params_opt p=pat EQ e=expr
    { ClassD(a, x, tps, p, e) @@ at($symbolstartpos,$endpos) }
*)
  | a=actor_opt CLASS x=id tps=typ_params_opt p=params
      LCURLY efs=seplist(expr_field, SEMICOLON) RCURLY
    { ClassD(a, x, tps, p, efs.it) @@ at($symbolstartpos,$endpos) }


(* Programs *)

prog :
  | ds=seplist(dec, SEMICOLON) EOF { ds }

%%
