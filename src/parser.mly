%{

open Syntax
open Source 

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

let at (startpos, endpos) = positions_to_region startpos endpos

let (@?) it at = {it; at; note = Type.Any}
let (@!) it at = {it; at; note = Type.Const}

(* TBR: once opam is on menhir 201807, replace ($symbolstartpos,$endpos) with $sloc *)

%}

%token EOF

%token LET VAR
%token LPAR RPAR LBRACKET RBRACKET LCURLY RCURLY
%token AWAIT ASYNC BREAK CASE CONTINUE LABEL
%token IF IN IS ELSE SWITCH LOOP WHILE FOR LIKE RETURN 
%token ARROW ASSIGN
%token FUNC TYPE ACTOR CLASS PRIVATE NEW
%token SEMICOLON COMMA COLON SUB DOT QUEST
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
%token<string> FLOAT
%token<Value.unicode> CHAR
%token<bool> BOOL
%token<string> ID
%token<string> TEXT

%token<Type.prim> PRIM

%token UNDERSCORE

%nonassoc IF_NO_ELSE LOOP_NO_WHILE
%nonassoc ELSE WHILE

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
    
%type<Syntax.exp> exp exp_nullary
%start<Syntax.prog> prog

%%

(* Helpers *)

seplist(X, SEP) :
  | (* empty *) { [] }
  | x=X SEP xs=seplist(X, SEP) { x::xs } 
  | x=X { [x] }


(* Basics *)

%inline id :
  | id=ID { id @@ at($symbolstartpos,$endpos)}

%inline var_ref :
  | id=ID { id @! at($symbolstartpos,$endpos) }

%inline var :
  | VAR { Type.Mut @@ at($symbolstartpos,$endpos) }

%inline var_opt :
  | (* empty *) { Type.Const @@ at($symbolstartpos,$endpos) }
  | VAR { Type.Mut @@ at($symbolstartpos,$endpos) }

%inline sort :
  | NEW { Type.Object @@ at($symbolstartpos,$endpos) }
  | ACTOR { Type.Actor @@ at($symbolstartpos,$endpos) }

%inline sort_opt :
  | (* empty *) { Type.Object @@ at($symbolstartpos,$endpos) }
  | s=sort { s }


(* Types *)

typ_obj :
  | LCURLY tfs=seplist(typ_field, SEMICOLON) RCURLY
    { tfs }

typ_nullary :
  | p=PRIM
    { PrimT(p) @@ at($symbolstartpos,$endpos) }
  | LPAR ts=seplist(typ_item, COMMA) RPAR
    { match ts with [t] -> t | _ -> TupT(ts) @@ at($symbolstartpos,$endpos) }
  | x=id tso=typ_args?
    {	VarT(x, Lib.Option.get tso []) @@ at($symbolstartpos,$endpos) }
  | s=sort_opt tfs=typ_obj
    { ObjT(s, tfs) @@ at($symbolstartpos,$endpos) }

typ_post :
  | t=typ_nullary
    { t }
  | t=typ_post LBRACKET RBRACKET
    { ArrayT(Type.Const @@ no_region, t) @@ at($symbolstartpos,$endpos) }
  | t=typ_post QUEST
    { OptT(t) @@ at($symbolstartpos,$endpos) }

typ_pre :
  | t=typ_post
    { t }
  | ASYNC t=typ_pre
    { AsyncT(t) @@ at($symbolstartpos,$endpos) }
  | LIKE t=typ_pre
    { LikeT(t) @@ at($symbolstartpos,$endpos) }
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
  | LT ts=seplist(typ, COMMA) GT { ts }

%inline typ_params_opt :
  | (* empty *) { [] }
  | LT ts=seplist(typ_bind, COMMA) GT { ts }

typ_field :
  | mut=var_opt x=id COLON t=typ
    { {var = x; typ = t; mut} @@ at($symbolstartpos,$endpos) }
  | x=id tps=typ_params_opt t1=typ_nullary t2=return_typ 
    { let t = FuncT(tps, t1, t2) @@ span x.at t2.at in
      {var = x; typ = t; mut = Type.Const @@ no_region} @@ at($symbolstartpos,$endpos) }

typ_bind :
  | x=id SUB t=typ
    { {var = x; bound = t} @@ at($symbolstartpos,$endpos) }
  | x=id
    { {var = x; bound = AnyT @@ at($symbolstartpos,$endpos)} @@ at($symbolstartpos,$endpos) }



(* Expressions *)

lit :
  | NULL { NullLit }
  | b=BOOL { BoolLit b }
  | s=NAT { PreLit (s, Type.Nat) }
  | s=INT { PreLit (s, Type.Int) }
  | s=FLOAT { PreLit (s, Type.Float) }
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


exp_block :
  | LCURLY es=seplist(exp, SEMICOLON) RCURLY
    { BlockE(es) @? at($symbolstartpos,$endpos) }

exp_obj :
  | LCURLY efs=seplist(exp_field, SEMICOLON) RCURLY
    { efs }

exp_nullary :
  | e=exp_block
    { e }
  | x=var_ref
    { VarE(x) @? at($symbolstartpos,$endpos) }
  | l=lit
    { LitE(ref l) @? at($symbolstartpos,$endpos) }
  | LPAR es = seplist(exp, COMMA) RPAR
    { match es with [e] -> e | _ -> TupE(es) @? at($symbolstartpos,$endpos) }
  | s=sort xo=id? efs=exp_obj
    { ObjE(s, xo, efs) @? at($symbolstartpos,$endpos) }

exp_post :
  | e=exp_nullary
    { e }
  | LBRACKET es=seplist(exp, COMMA) RBRACKET
    { ArrayE(es) @? at($symbolstartpos,$endpos) }
  | e1=exp_post LBRACKET e2=exp RBRACKET
    { IdxE(e1, e2) @? at($symbolstartpos,$endpos) }
  | e=exp_post DOT s=NAT
    { ProjE (e, int_of_string s) @? at($symbolstartpos,$endpos) }
  | e=exp_post DOT x=var_ref
    { DotE(e, x) @? at($symbolstartpos,$endpos) }
  | e1=exp_post tso=typ_args? e2=exp_nullary
    { CallE(e1, Lib.Option.get tso [], e2) @? at($symbolstartpos,$endpos) }

exp_un :
  | e=exp_post
    { e } 
  | op=unop e=exp_un
    { UnE(op ,e) @? at($symbolstartpos,$endpos) }
  | op=unassign e=exp_un
    (* TBR: this is incorrect, since it duplicates e *)
    { AssignE(e, UnE(op, e) @? at($symbolstartpos,$endpos)) @? at($symbolstartpos,$endpos) }
  | NOT e=exp_un
    { NotE e @? at($symbolstartpos,$endpos) }

exp_bin :
  | e=exp_un
    { e } 
  | e1=exp_bin op=binop e2=exp_bin
    { BinE(e1, op, e2) @? at($symbolstartpos,$endpos) }
  | e1=exp_bin op=relop e2=exp_bin
    { RelE(e1, op, e2) @? at($symbolstartpos,$endpos) }
  | e1=exp_bin ASSIGN e2=exp_bin
    { AssignE(e1, e2) @? at($symbolstartpos,$endpos)}
  | e1=exp_bin op=binassign e2=exp_bin
    (* TBR: this is incorrect, since it duplicates e1 *)
    { AssignE(e1, BinE(e1, op, e2) @? at($symbolstartpos,$endpos)) @? at($symbolstartpos,$endpos) }
  | e1=exp_bin AND e2=exp_bin
    { AndE(e1, e2) @? at($symbolstartpos,$endpos) }
  | e1=exp_bin OR e2=exp_bin
    { OrE(e1, e2) @? at($symbolstartpos,$endpos) }
  | e=exp_bin IS t=typ
    { IsE(e, t) @? at($symbolstartpos,$endpos) }
  | e=exp_bin COLON t=typ
    { AnnotE(e, t) @? at($symbolstartpos,$endpos) }

exp_pre :
  | e=exp_bin
    { e } 
  | RETURN eo=exp_pre?
    { let e = Lib.Option.get eo (TupE([]) @? at($symbolstartpos,$endpos)) in
      RetE(e) @? at($symbolstartpos,$endpos) }
  | ASYNC e=exp_pre
    { AsyncE(e) @? at($symbolstartpos,$endpos) }
  | AWAIT e=exp_pre
    { AwaitE(e) @? at($symbolstartpos,$endpos) }
  | ASSERT e=exp_pre
    { AssertE(e) @? at($symbolstartpos,$endpos) }

exp :
  | e=exp_pre
    { e } 
  | LABEL x=id e=exp
    { let x' = ("continue " ^ x.it) @@ x.at in
      let e' =
        match e.it with
        | WhileE (e1, e2) -> WhileE (e1, LabelE (x', e2) @? e2.at) @? e.at
        | LoopE (e1, eo) -> LoopE (LabelE (x', e1) @? e1.at, eo) @? e.at
        | ForE (p, e1, e2) -> ForE (p, e1, LabelE (x', e2) @? e2.at) @? e.at
        | _ -> e
      in LabelE(x, e') @? at($symbolstartpos,$endpos) }
  | BREAK x=id eo=exp_nullary?
    { let e = Lib.Option.get eo (TupE([]) @? at($symbolstartpos,$endpos)) in
      BreakE(x, e) @? at($symbolstartpos,$endpos) }
  | CONTINUE x=id
    { let x' = ("continue " ^ x.it) @@ x.at in
      BreakE(x', TupE([]) @? no_region) @? at($symbolstartpos,$endpos) }
  | IF b=exp_nullary e1=exp %prec IF_NO_ELSE
    { IfE(b, e1, TupE([]) @? no_region) @? at($symbolstartpos,$endpos) }
  | IF b=exp_nullary e1=exp ELSE e2=exp
    { IfE(b, e1, e2) @? at($symbolstartpos,$endpos) }
  | SWITCH e=exp_nullary LCURLY cs=case* RCURLY
    { SwitchE(e, cs) @? at($symbolstartpos,$endpos) }
  | WHILE e1=exp_nullary e2=exp
    { WhileE(e1, e2) @? at($symbolstartpos,$endpos) }
  | LOOP e=exp %prec LOOP_NO_WHILE
    { LoopE(e, None) @? at($symbolstartpos,$endpos) }
  | LOOP e1=exp WHILE e2=exp
    { LoopE(e1, Some e2) @? at($symbolstartpos,$endpos) }
  | FOR p=pat IN e1=exp_nullary e2=exp
    { ForE(p, e1, e2) @? at($symbolstartpos,$endpos) }
  | d=dec
    { DecE(d) @? at($symbolstartpos,$endpos) }
      
    
case : 
  | CASE p=pat_nullary e=exp
    { {pat = p; exp = e} @@ at($symbolstartpos,$endpos) }

%inline private_opt :
  | (* empty *) { Public @@ at($symbolstartpos,$endpos) }
  | PRIVATE { Private @@ at($symbolstartpos,$endpos) }

exp_field :
  | p=private_opt m=var_opt x=id EQ e=exp
    { {var = x; mut = m; priv = p; exp = e} @@ at($symbolstartpos,$endpos) }
  | p=private_opt m=var_opt x=id COLON t=typ EQ e=exp
    { let e = AnnotE(e, t) @? span t.at e.at in
      {var = x; mut = m; priv = p; exp = e} @@ at($symbolstartpos,$endpos) }
  | priv=private_opt x=id fd=func_dec
    { let d = fd x in
      let e = DecE(d) @? d.at in
      {var = x; mut = Type.Const @@ no_region; priv; exp = e} @@ at($symbolstartpos,$endpos) }

(* TBR: allow patterns *)
param :
  | x=id COLON t=typ
    { AnnotP(VarP(x) @@ x.at, t) @@ at($symbolstartpos,$endpos) }

params :
  | LPAR ps=seplist(param, COMMA) RPAR
    { match ps with [p] -> p | _ -> TupP(ps) @@ at($symbolstartpos,$endpos) }


(* Patterns *)

pat_nullary :
  | l=lit
    { LitP(ref l) @@ at($symbolstartpos,$endpos) }
  | UNDERSCORE
    { WildP @@ at($symbolstartpos,$endpos) }
  | x=id
    { VarP(x) @@ at($symbolstartpos,$endpos) }
  | LPAR ps=seplist(pat, COMMA) RPAR
    { match ps with [p] -> p | _ -> TupP(ps) @@ at($symbolstartpos,$endpos) }

pat :
  | p=pat_nullary
    { p }
  | p=pat COLON t=typ
    { AnnotP(p, t) @@ at($symbolstartpos,$endpos) }

return_typ :
  | COLON t=typ { t }


(* Declarations *)

dec :
  | LET p=pat EQ e=exp
    { let p', e' =
        match p.it with
        | AnnotP (p', t) -> p', AnnotE (e, t) @? p.at
        | _ -> p, e
      in LetD (p', e') @@ at($symbolstartpos,$endpos) }
  | VAR x=id COLON t=typ EQ e=exp
    { VarD(x, t, e) @@ at($symbolstartpos,$endpos) } 
  | FUNC xo=id? fd=func_dec
    { let x = Lib.Option.get xo ("" @@ at($symbolstartpos,$endpos)) in
      (fd x).it @@ at($symbolstartpos,$endpos) }
  | TYPE x=id tps=typ_params_opt EQ t=typ
    { TypD(x, tps, t) @@ at($symbolstartpos,$endpos) }
  | s=sort_opt CLASS xo=id? tps=typ_params_opt p=params efs=class_body
    { let x = Lib.Option.get xo ("" @@ at($symbolstartpos,$endpos)) in
      ClassD(s, x, tps, p, efs) @@ at($symbolstartpos,$endpos) }

func_dec :
  | tps=typ_params_opt ps=params rt=return_typ? fb=func_body
    { let t = Lib.Option.get rt (TupT([]) @@ no_region) in
      (* This is a hack to support async method declarations. *)
      let e = match fb with
        | (false, e) -> e (* body declared as EQ e *)
        | (true, e) -> (* body declared as immediate block *)
          match t.it with
          | AsyncT _ -> AsyncE(e) @? e.at
          | _ -> e
      in fun x -> FuncD(x, tps, ps, t, e) @@ at($symbolstartpos,$endpos) }

func_body :
  | EQ e=exp { (false, e) }
  | e=exp_block { (true, e) }

class_body :
  | EQ efs=exp_obj { efs }
  | efs=exp_obj { efs }


(* Programs *)

prog :
  | es=seplist(exp, SEMICOLON) EOF { es @@ at($symbolstartpos,$endpos) }

%%
