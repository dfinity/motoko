%{

open Syntax
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

let (@?) it at = {it; at; note = (Type.Triv,Type.Any)}
let (@!) it at = {it; at; note = Type.Const}

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
%start<Syntax.prog> parse_prog

%%

(* Helpers *)

seplist(X, SEP) :
  | (* empty *) { [] }
  | x=X { [x] }
  | x=X SEP xs=seplist(X, SEP) { x::xs }


(* Basics *)

%inline id :
  | id=ID { id @@ at $sloc}

%inline var_ref :
  | id=ID { id @! at $sloc }

%inline var :
  | VAR { Type.Mut @@ at $sloc }

%inline var_opt :
  | (* empty *) { Type.Const @@  no_region }
  | VAR { Type.Mut @@ at $sloc }

%inline sort :
  | NEW { Type.Object @@ at $sloc }
  | ACTOR { Type.Actor @@ at $sloc }

%inline sort_opt :
  | (* empty *) { Type.Object @@ no_region }
  | s=sort { s }


(* Types *)

typ_obj :
  | LCURLY tfs=seplist(typ_field, SEMICOLON) RCURLY
    { tfs }

typ_nullary :
  | p=PRIM
    { PrimT(p) @@ at $sloc }
  | LPAR ts=seplist(typ_item, COMMA) RPAR
    { match ts with [t] -> t | _ -> TupT(ts) @@ at $sloc }
  | x=id tso=typ_args?
    {	VarT(x, Lib.Option.get tso []) @@ at $sloc }
  | s=sort_opt tfs=typ_obj
    { ObjT(s, tfs) @@ at $sloc }

typ_post :
  | t=typ_nullary
    { t }
  | t=typ_post LBRACKET RBRACKET
    { ArrayT(Type.Const @@ no_region, t) @@ at $sloc }
  | t=typ_post QUEST
    { OptT(t) @@ at $sloc }

typ_pre :
  | t=typ_post
    { t }
  | ASYNC t=typ_pre
    { AsyncT(t) @@ at $sloc }
  | LIKE t=typ_pre
    { LikeT(t) @@ at $sloc }
  | mut=var t=typ_nullary LBRACKET RBRACKET
    { ArrayT(mut, t) @@ at $sloc }

typ :
  | t=typ_pre
    { t }
  | tps=typ_params_opt t1=typ_pre ARROW t2=typ 
    { FuncT(tps, t1, t2) @@ at $sloc }

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
    { {var = x; typ = t; mut} @@ at $sloc }
  | x=id tps=typ_params_opt t1=typ_nullary t2=return_typ 
    { let t = FuncT(tps, t1, t2) @@ span x.at t2.at in
      {var = x; typ = t; mut = Type.Const @@ no_region} @@ at $sloc }

typ_bind :
  | x=id SUB t=typ
    { {var = x; bound = t} @@ at $sloc }
  | x=id
    { {var = x; bound = AnyT @@ at $sloc} @@ at $sloc }



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
    { BlockE(es) @? at $sloc }

exp_obj :
  | LCURLY efs=seplist(exp_field, SEMICOLON) RCURLY
    { efs }

exp_nullary :
  | e=exp_block
    { e }
  | x=var_ref
    { VarE(x) @? at $sloc }
  | l=lit
    { LitE(ref l) @? at $sloc }
  | LPAR es = seplist(exp, COMMA) RPAR
    { match es with [e] -> e | _ -> TupE(es) @? at $sloc }
  | s=sort xo=id? efs=exp_obj
    { ObjE(s, xo, efs) @? at $sloc }

exp_post :
  | e=exp_nullary
    { e }
  | LBRACKET es=seplist(exp, COMMA) RBRACKET
    { ArrayE(es) @? at $sloc }
  | e1=exp_post LBRACKET e2=exp RBRACKET
    { IdxE(e1, e2) @? at $sloc }
  | e=exp_post DOT s=NAT
    { ProjE (e, int_of_string s) @? at $sloc }
  | e=exp_post DOT x=var_ref
    { DotE(e, x) @? at $sloc }
  | e1=exp_post tso=typ_args? e2=exp_nullary
    { CallE(e1, Lib.Option.get tso [], e2) @? at $sloc }

exp_un :
  | e=exp_post
    { e } 
  | op=unop e=exp_un
    { UnE(op ,e) @? at $sloc }
  | op=unassign e=exp_un
    (* TBR: this is incorrect, since it duplicates e *)
    { AssignE(e, UnE(op, e) @? at $sloc) @? at $sloc }
  | NOT e=exp_un
    { NotE e @? at $sloc }

exp_bin :
  | e=exp_un
    { e } 
  | e1=exp_bin op=binop e2=exp_bin
    { BinE(e1, op, e2) @? at $sloc }
  | e1=exp_bin op=relop e2=exp_bin
    { RelE(e1, op, e2) @? at $sloc }
  | e1=exp_bin ASSIGN e2=exp_bin
    { AssignE(e1, e2) @? at $sloc}
  | e1=exp_bin op=binassign e2=exp_bin
    (* TBR: this is incorrect, since it duplicates e1 *)
    { AssignE(e1, BinE(e1, op, e2) @? at $sloc) @? at $sloc }
  | e1=exp_bin AND e2=exp_bin
    { AndE(e1, e2) @? at $sloc }
  | e1=exp_bin OR e2=exp_bin
    { OrE(e1, e2) @? at $sloc }
  | e=exp_bin IS t=typ
    { IsE(e, t) @? at $sloc }
  | e=exp_bin COLON t=typ
    { AnnotE(e, t) @? at $sloc }

exp_pre :
  | e=exp_bin
    { e } 
  | RETURN eo=exp_pre?
    { let e = Lib.Option.get eo (TupE([]) @? at $sloc) in
      RetE(e) @? at $sloc }
  | ASYNC e=exp_pre
    { AsyncE(e) @? at $sloc }
  | AWAIT e=exp_pre
    { AwaitE(e) @? at $sloc }
  | ASSERT e=exp_pre
    { AssertE(e) @? at $sloc }

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
      in LabelE(x, e') @? at $sloc }
  | BREAK x=id eo=exp_nullary?
    { let e = Lib.Option.get eo (TupE([]) @? at $sloc) in
      BreakE(x, e) @? at $sloc }
  | CONTINUE x=id
    { let x' = ("continue " ^ x.it) @@ x.at in
      BreakE(x', TupE([]) @? no_region) @? at $sloc }
  | IF b=exp_nullary e1=exp %prec IF_NO_ELSE
    { IfE(b, e1, TupE([]) @? no_region) @? at $sloc }
  | IF b=exp_nullary e1=exp ELSE e2=exp
    { IfE(b, e1, e2) @? at $sloc }
  | SWITCH e=exp_nullary LCURLY cs=case* RCURLY
    { SwitchE(e, cs) @? at $sloc }
  | WHILE e1=exp_nullary e2=exp
    { WhileE(e1, e2) @? at $sloc }
  | LOOP e=exp %prec LOOP_NO_WHILE
    { LoopE(e, None) @? at $sloc }
  | LOOP e1=exp WHILE e2=exp
    { LoopE(e1, Some e2) @? at $sloc }
  | FOR p=pat IN e1=exp_nullary e2=exp
    { ForE(p, e1, e2) @? at $sloc }
  | d=dec
    { DecE(d) @? at $sloc }
      
    
case : 
  | CASE p=pat_nullary e=exp
    { {pat = p; exp = e} @@ at $sloc }

%inline private_opt :
  | (* empty *) { Public @@ no_region }
  | PRIVATE { Private @@ at $sloc }

exp_field :
  | p=private_opt m=var_opt x=id EQ e=exp
    { {var = x; mut = m; priv = p; exp = e} @@ at $sloc }
  | p=private_opt m=var_opt x=id COLON t=typ EQ e=exp
    { let e = AnnotE(e, t) @? span t.at e.at in
      {var = x; mut = m; priv = p; exp = e} @@ at $sloc }
  | priv=private_opt x=id fd=func_dec
    { let d = fd x in
      let e = DecE(d) @? d.at in
      {var = x; mut = Type.Const @@ no_region; priv; exp = e} @@ at $sloc }

(* TBR: allow patterns *)
param :
  | x=id COLON t=typ
    { AnnotP(VarP(x) @@ x.at, t) @@ at $sloc }

params :
  | LPAR ps=seplist(param, COMMA) RPAR
    { match ps with [p] -> p | _ -> TupP(ps) @@ at $sloc }


(* Patterns *)

pat_nullary :
  | l=lit
    { LitP(ref l) @@ at $sloc }
  | UNDERSCORE
    { WildP @@ at $sloc }
  | x=id
    { VarP(x) @@ at $sloc }
  | LPAR ps=seplist(pat, COMMA) RPAR
    { match ps with [p] -> p | _ -> TupP(ps) @@ at $sloc }

pat :
  | p=pat_nullary
    { p }
  | p=pat COLON t=typ
    { AnnotP(p, t) @@ at $sloc }

return_typ :
  | COLON t=typ { t }


(* Declarations *)

dec :
  | LET p=pat EQ e=exp
    { let p', e' =
        match p.it with
        | AnnotP (p', t) -> p', AnnotE (e, t) @? p.at
        | _ -> p, e
      in LetD (p', e') @@ at $sloc }
  | VAR x=id t=return_typ? EQ e=exp
    { let e' =
        match t with
        | None -> e
        | Some t -> AnnotE (e, t) @? span t.at e.at
      in VarD(x, e') @@ at $sloc }
  | FUNC xo=id? fd=func_dec
    { let x = Lib.Option.get xo ("" @@ at $sloc) in
      (fd x).it @@ at $sloc }
  | TYPE x=id tps=typ_params_opt EQ t=typ
    { TypD(x, tps, t) @@ at $sloc }
  | s=sort_opt CLASS xo=id? tps=typ_params_opt p=params efs=class_body
    { let x = Lib.Option.get xo ("" @@ at $sloc) in
      ClassD(s, x, tps, p, efs) @@ at $sloc }

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
      in fun x -> FuncD(x, tps, ps, t, e) @@ at $sloc }

func_body :
  | EQ e=exp { (false, e) }
  | e=exp_block { (true, e) }

class_body :
  | EQ efs=exp_obj { efs }
  | efs=exp_obj { efs }


(* Programs *)

parse_prog :
  | es=seplist(exp, SEMICOLON) EOF { es @@ at $sloc }

%%
