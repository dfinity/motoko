%{
open As_def
open As_types
open As_values

open Syntax
open Source
open Operator


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

let dup_var x = VarE (x.it @@ x.at) @? x.at

let anon sort at = "anon-" ^ sort ^ "-" ^ string_of_pos at.left

let name_exp e =
  match e.it with
  | VarE x -> [], e, dup_var x
  | _ ->
    let x = anon "val" e.at @@ e.at in
    [LetD (VarP x @! x.at, e) @? e.at], dup_var x, dup_var x

let assign_op lhs rhs_f at =
  let ds, lhs', rhs' =
    match lhs.it with
    | VarE x -> [], lhs, dup_var x
    | DotE (e1, x) ->
      let ds, ex11, ex12 = name_exp e1 in
      ds, DotE (ex11, x) @? lhs.at, DotE (ex12, x.it @@ x.at) @? lhs.at
    | IdxE (e1, e2) ->
      let ds1, ex11, ex12 = name_exp e1 in
      let ds2, ex21, ex22 = name_exp e2 in
      ds1 @ ds2, IdxE (ex11, ex21) @? lhs.at, IdxE (ex12, ex22) @? lhs.at
    | _ ->
      name_exp lhs
  in
  let e = AssignE (lhs', rhs_f rhs') @? at in
  match ds with
  | [] -> e
  | ds -> BlockE (ds @ [ExpD e @? e.at]) @? at

let let_or_exp named x e' at =
  if named
  then LetD(VarP(x) @! at, e' @? at) @? at
  else ExpD(e' @? at) @? at

let share_typ t =
  match t.it with
  | FuncT ({it = Type.Local; _} as s, tbs, t1, t2) ->
    { t with it = FuncT ({s with it = Type.Shared}, tbs, t1, t2)}
  | _ -> t

let share_typfield (tf : typ_field) =
  {tf with it = {tf.it with typ = share_typ tf.it.typ}}

let share_exp e =
  match e.it with
  | FuncE (x, ({it = Type.Local; _} as s), tbs, p, t, e) ->
    FuncE (x, {s with it = Type.Shared}, tbs, p, t, e) @? e.at
  | _ -> e

let share_dec d =
  match d.it with
  | LetD (p, e) -> LetD (p, share_exp e) @? d.at
  | _ -> d

let share_expfield (ef : exp_field) =
  if ef.it.vis.it = Private
  then ef
  else {ef with it = {ef.it with dec = share_dec ef.it.dec}}

%}

%token EOF

%token LET VAR
%token LPAR RPAR LBRACKET RBRACKET LCURLY RCURLY
%token AWAIT ASYNC BREAK CASE CATCH CONTINUE LABEL
%token IF IN ELSE SWITCH LOOP WHILE FOR RETURN TRY THROW
%token ARROW ASSIGN
%token FUNC TYPE OBJECT ACTOR CLASS PUBLIC PRIVATE SHARED
%token SEMICOLON SEMICOLON_EOL COMMA COLON SUB DOT QUEST
%token AND OR NOT
%token IMPORT MODULE
%token DEBUG_SHOW
%token ASSERT
%token ADDOP SUBOP MULOP DIVOP MODOP POWOP
%token ANDOP OROP XOROP SHLOP USHROP SSHROP ROTLOP ROTROP
%token EQOP NEQOP LEOP LTOP GTOP GEOP
%token HASH
%token EQ LT GT
%token PLUSASSIGN MINUSASSIGN MULASSIGN DIVASSIGN MODASSIGN POWASSIGN CATASSIGN
%token ANDASSIGN ORASSIGN XORASSIGN SHLASSIGN USHRASSIGN SSHRASSIGN ROTLASSIGN ROTRASSIGN
%token NULL
%token<string> DOT_NUM
%token<string> NAT
%token<string> FLOAT
%token<As_values.Value.unicode> CHAR
%token<bool> BOOL
%token<string> ID
%token<string> TEXT
%token PRIM
%token UNDERSCORE

%nonassoc RETURN_NO_ARG IF_NO_ELSE LOOP_NO_WHILE
%nonassoc ELSE WHILE

%right ASSIGN PLUSASSIGN MINUSASSIGN MULASSIGN DIVASSIGN MODASSIGN POWASSIGN CATASSIGN ANDASSIGN ORASSIGN XORASSIGN SHLASSIGN USHRASSIGN SSHRASSIGN ROTLASSIGN ROTRASSIGN
%left COLON
%left OR
%left AND
%nonassoc EQOP NEQOP LEOP LTOP GTOP GEOP
%left ADDOP SUBOP HASH
%left MULOP DIVOP MODOP
%left OROP
%left ANDOP
%left XOROP
%nonassoc SHLOP USHROP SSHROP ROTLOP ROTROP
%left POWOP

%type<Syntax.exp> exp(ob) exp_nullary(ob)
%start<string -> As_def.Syntax.prog> parse_prog
%start<string -> As_def.Syntax.prog> parse_prog_interactive

%%

(* Helpers *)

seplist(X, SEP) :
  | (* empty *) { [] }
  | x=X { [x] }
  | x=X SEP xs=seplist(X, SEP) { x::xs }

seplist1(X, SEP) :
  | x=X { [x] }
  | x=X SEP xs=seplist(X, SEP) { x::xs }


(* Basics *)

%inline semicolon :
  | SEMICOLON
  | SEMICOLON_EOL { () }

%inline id :
  | id=ID { id @@ at $sloc }

%inline tag :
  | HASH id=ID { id @@ at $sloc }

%inline typ_id :
  | id=ID { id @= at $sloc }

%inline id_opt :
  | id=id { fun _ _ -> true, id }
  | (* empty *) { fun sort sloc -> false, anon sort (at sloc) @@ at sloc }

%inline typ_id_opt :
  | id=typ_id { fun _ _ -> id }
  | (* empty *) { fun sort sloc -> anon sort (at sloc) @= at sloc }

%inline var_opt :
  | (* empty *) { Const @@ no_region }
  | VAR { Var @@ at $sloc }

%inline obj_sort :
  | OBJECT { Type.Object @@ at $sloc }
  | ACTOR { Type.Actor @@ at $sloc }
  | MODULE { Type.Module @@ at $sloc }

%inline obj_sort_opt :
  | (* empty *) { Type.Object @@ no_region }
  | s=obj_sort { s }

%inline func_sort_opt :
  | (* empty *) { Type.Local @@ no_region }
  | SHARED { Type.Shared @@ at $sloc }


(* Paths *)

path :
  | x=id
    { IdH x @! at $sloc }
  | p=path DOT x=id
    { DotH (p, x) @! at $sloc }


(* Types *)

typ_obj :
  | LCURLY tfs=seplist(typ_field, semicolon) RCURLY
    { tfs }

typ_variant :
  | LCURLY HASH RCURLY
    { [] }
  | LCURLY tfs=seplist1(typ_tag, semicolon) RCURLY
    { tfs }

typ_nullary :
  | LPAR ts=seplist(typ_item, COMMA) RPAR
    { (match ts with [t] -> ParT(t) | _ -> TupT(ts)) @! at $sloc }
  | p=path tso=typ_args?
    { PathT(p, Lib.Option.get tso []) @! at $sloc }
  | LBRACKET m=var_opt t=typ RBRACKET
    { ArrayT(m, t) @! at $sloc }
  | tfs=typ_obj
    { ObjT(Type.Object @@ at $sloc, tfs) @! at $sloc }
  | tfs=typ_variant
    { VariantT tfs @! at $sloc }

typ_un :
  | t=typ_nullary
    { t }
  | QUEST t=typ_un
    { OptT(t) @! at $sloc }

typ_pre :
  | t=typ_un
    { t }
  | PRIM s=TEXT
    { PrimT(s) @! at $sloc }
  | ASYNC t=typ_pre
    { AsyncT(t) @! at $sloc }
  | s=obj_sort tfs=typ_obj
    { let tfs' =
        if s.it = Type.Actor then List.map share_typfield tfs else tfs
      in ObjT(s, tfs') @! at $sloc }

typ :
  | t=typ_pre
    { t }
  | s=func_sort_opt tps=typ_params_opt t1=typ_un ARROW t2=typ
    { FuncT(s, tps, t1, t2) @! at $sloc }

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
    { {id = x; typ = t; mut} @@ at $sloc }
  | x=id tps=typ_params_opt t1=typ_nullary t2=return_typ
    { let t = FuncT(Type.Local @@ no_region, tps, t1, t2)
              @! span x.at t2.at in
      {id = x; typ = t; mut = Const @@ no_region} @@ at $sloc }

typ_tag :
  | x=tag t=return_typ_nullary?
    { {tag = x; typ = Lib.Option.get t (TupT [] @! at $sloc)} @@ at $sloc }

typ_bind :
  | x=id SUB t=typ
    { {var = x; bound = t} @= at $sloc }
  | x=id
    { {var = x; bound = PrimT "Any" @! at $sloc} @= at $sloc }



(* Expressions *)

lit :
  | NULL { NullLit }
  | b=BOOL { BoolLit b }
  | s=NAT { PreLit (s, Type.Nat) }
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
  | POWOP { PowOp }
  | ANDOP { AndOp }
  | OROP  { OrOp }
  | XOROP { XorOp }
  | SHLOP { ShLOp }
  | USHROP { UShROp }
  | SSHROP { SShROp }
  | ROTLOP { RotLOp }
  | ROTROP { RotROp }
  | HASH { CatOp }

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
  | POWASSIGN { PowOp }
  | ANDASSIGN { AndOp }
  | ORASSIGN { OrOp }
  | XORASSIGN { XorOp }
  | SHLASSIGN { ShLOp }
  | USHRASSIGN { UShROp }
  | SSHRASSIGN { SShROp }
  | ROTLASSIGN { RotLOp }
  | ROTRASSIGN { RotROp }
  | CATASSIGN { CatOp }


(* Default {} to block or object, respectively *)
bl : { fun ds -> BlockE(ds) }
ob : { fun ds -> ObjE(Type.Object @@ no_region,
         List.map (fun d -> {dec = d; vis = Public @@ d.at} @@ d.at) ds) }

exp_block :
  | LCURLY ds=seplist(dec, semicolon) RCURLY
    { BlockE(ds) @? at $sloc }

exp_nullary(B) :
  | LCURLY ds=seplist(dec_var, semicolon) RCURLY e=B
    { e ds @? at $sloc }
  | LCURLY efs=exp_field_list_unamb RCURLY
    { ObjE(Type.Object @@ at $sloc, efs) @? at $sloc }
  | LCURLY ds=dec_list_unamb RCURLY
    { BlockE(ds) @? at $sloc }
  | x=id
    { VarE(x) @? at $sloc }
  | l=lit
    { LitE(ref l) @? at $sloc }
  | LPAR es=seplist(exp(ob), COMMA) RPAR
    { match es with [e] -> e | _ -> TupE(es) @? at $sloc }
  | PRIM s=TEXT
    { PrimE(s) @? at $sloc }

exp_post(B) :
  | e=exp_nullary(B)
    { e }
  | LBRACKET m=var_opt es=seplist(exp_nonvar(ob), COMMA) RBRACKET
    { ArrayE(m, es) @? at $sloc }
  | e1=exp_post(ob) LBRACKET e2=exp(ob) RBRACKET
    { IdxE(e1, e2) @? at $sloc }
  | e=exp_post(ob) s=DOT_NUM
    { ProjE (e, int_of_string s) @? at $sloc }
  | e=exp_post(ob) DOT x=id
    { DotE(e, x) @? at $sloc }
  | e1=exp_post(ob) tso=typ_args? e2=exp_nullary(ob)
    { let typ_args = Lib.Option.get tso [] in
      CallE(e1, typ_args, e2) @? at $sloc }

exp_un(B) :
  | e=exp_post(B)
    { e }
  | x=tag
    { TagE (x, TupE([]) @? at $sloc) @? at $sloc }
  | x=tag e=exp_nullary(ob)
    { TagE (x, e) @? at $sloc }
  | QUEST e=exp_un(ob)
    { OptE(e) @? at $sloc }
  | op=unop e=exp_un(ob)
    { UnE(ref Type.Pre, op, e) @? at $sloc }
  | op=unassign e=exp_un(ob)
    { assign_op e (fun e' -> UnE(ref Type.Pre, op, e') @? at $sloc) (at $sloc) }
  | NOT e=exp_un(ob)
    { NotE e @? at $sloc }
  | DEBUG_SHOW e=exp_un(ob)
    { ShowE (ref Type.Pre, e) @? at $sloc }

exp_bin(B) :
  | e=exp_un(B)
    { e }
  | e1=exp_bin(ob) op=binop e2=exp_bin(ob)
    { BinE(ref Type.Pre, e1, op, e2) @? at $sloc }
  | e1=exp_bin(ob) op=relop e2=exp_bin(ob)
    { RelE(ref Type.Pre, e1, op, e2) @? at $sloc }
  | e1=exp_bin(ob) ASSIGN e2=exp_bin(ob)
    { AssignE(e1, e2) @? at $sloc}
  | e1=exp_bin(ob) op=binassign e2=exp_bin(ob)
    { assign_op e1 (fun e1' -> BinE(ref Type.Pre, e1', op, e2) @? at $sloc) (at $sloc) }
  | e1=exp_bin(ob) AND e2=exp_bin(ob)
    { AndE(e1, e2) @? at $sloc }
  | e1=exp_bin(ob) OR e2=exp_bin(ob)
    { OrE(e1, e2) @? at $sloc }
  | e=exp_bin(ob) COLON t=typ
    { AnnotE(e, t) @? at $sloc }

exp_nondec(B) :
  | e=exp_bin(B)
    { e }
  | RETURN %prec RETURN_NO_ARG
    { RetE(TupE([]) @? at $sloc) @? at $sloc }
  | RETURN e=exp(ob)
    { RetE(e) @? at $sloc }
  | ASYNC e=exp(bl)
    { AsyncE(e) @? at $sloc }
  | AWAIT e=exp(bl)
    { AwaitE(e) @? at $sloc }
  | ASSERT e=exp(bl)
    { AssertE(e) @? at $sloc }
  | LABEL x=id rt=return_typ_nullary? e=exp(bl)
    { let x' = ("continue " ^ x.it) @@ x.at in
      let t = Lib.Option.get rt (TupT [] @! at $sloc) in
      let e' =
        match e.it with
        | WhileE (e1, e2) -> WhileE (e1, LabelE (x', t, e2) @? e2.at) @? e.at
        | LoopE (e1, eo) -> LoopE (LabelE (x', t, e1) @? e1.at, eo) @? e.at
        | ForE (p, e1, e2) -> ForE (p, e1, LabelE (x', t, e2) @? e2.at) @? e.at
        | _ -> e
      in LabelE(x, t, e') @? at $sloc }
  | BREAK x=id eo=exp_nullary(ob)?
    { let e = Lib.Option.get eo (TupE([]) @? at $sloc) in
      BreakE(x, e) @? at $sloc }
  | CONTINUE x=id
    { let x' = ("continue " ^ x.it) @@ x.at in
      BreakE(x', TupE([]) @? no_region) @? at $sloc }
  | IF b=exp_nullary(ob) e1=exp(bl) %prec IF_NO_ELSE
    { IfE(b, e1, TupE([]) @? no_region) @? at $sloc }
  | IF b=exp_nullary(ob) e1=exp(bl) ELSE e2=exp(bl)
    { IfE(b, e1, e2) @? at $sloc }
  | TRY e1=exp_nullary(ob) CATCH c=catch
    { TryE(e1, [c]) @? at $sloc }
(* TODO: enable multi-branch TRY (already supported by compiler)
  | TRY e=exp_nullary(ob) LCURLY cs=seplist(case, semicolon) RCURLY
    { TryE(e, cs) @? at $sloc }
*)
  | THROW e=exp(bl)
    { ThrowE(e) @? at $sloc }
  | SWITCH e=exp_nullary(ob) LCURLY cs=seplist(case, semicolon) RCURLY
    { SwitchE(e, cs) @? at $sloc }
  | WHILE e1=exp_nullary(ob) e2=exp(bl)
    { WhileE(e1, e2) @? at $sloc }
  | LOOP e=exp(bl) %prec LOOP_NO_WHILE
    { LoopE(e, None) @? at $sloc }
  | LOOP e1=exp(bl) WHILE e2=exp(bl)
    { LoopE(e1, Some e2) @? at $sloc }
  | FOR LPAR p=pat IN e1=exp(ob) RPAR e2=exp(bl)
    { ForE(p, e1, e2) @? at $sloc }

exp_nonvar(B) :
  | e=exp_nondec(B)
    { e }
  | d=dec_nonvar
    { match d.it with ExpD e -> e | _ -> BlockE([d]) @? at $sloc }

exp(B) :
  | e=exp_nonvar(B)
    { e }
  | d=dec_var
    { match d.it with ExpD e -> e | _ -> BlockE([d]) @? at $sloc }

case :
  | CASE p=pat_nullary e=exp(bl)
    { {pat = p; exp = e} @@ at $sloc }

catch :
  | p=pat_nullary e=exp(bl)
    { {pat = p; exp = e} @@ at $sloc }

exp_field_nonvar :
  | x=id EQ e=exp(ob)
    { let d = LetD(VarP(x) @! x.at, e) @? at $sloc in
      {dec = d; vis = Public @@ x.at} @@ at $sloc }

exp_field :
  | ef=exp_field_nonvar { ef }
  | d=dec_var { {dec = d; vis = Public @@ d.at} @@ at $sloc }

exp_field_list_unamb :  (* does not overlap with dec_list_unamb *)
  | ef=exp_field_nonvar
    { [ef] }
  | ef=exp_field_nonvar semicolon efs=seplist(exp_field, semicolon)
    { ef::efs }
  | d=dec_var semicolon efs=exp_field_list_unamb
    { ({dec = d; vis = Public @@ d.at} @@ at $sloc) :: efs }

dec_field :
  | v=vis d=dec
    { {dec = d; vis = v} @@ at $sloc }

vis :
  | (* empty *) { Private @@ no_region }
  | PRIVATE { Private @@ at $sloc }
  | PUBLIC { Public @@ at $sloc }


(* Patterns *)

pat_param :
  | UNDERSCORE
    { WildP @! at $sloc }
  | x=id
    { VarP(x) @! at $sloc }
  | l=lit
    { LitP(ref l) @! at $sloc }
  | LPAR ps=seplist(pat_bin, COMMA) RPAR
    { (match ps with [p] -> ParP(p) | _ -> TupP(ps)) @! at $sloc }

pat_nullary :
  | p=pat_param
    { p }
  | LCURLY fps=seplist(pat_field, semicolon) RCURLY
    { ObjP(fps) @! at $sloc }

pat_un :
  | p=pat_nullary
    { p }
  | x=tag
    { TagP(x, TupP [] @! at $sloc) @! at $sloc }
  | x=tag p=pat_nullary
    { TagP(x, p) @! at $sloc }
  | QUEST p=pat_un
    { OptP(p) @! at $sloc }
  | op=unop l=lit
    { SignP(op, ref l) @! at $sloc }

pat_bin :
  | p=pat_un
    { p }
  | p1=pat_bin OR p2=pat_bin
    { AltP(p1, p2) @! at $sloc }
  | p=pat_bin COLON t=typ
    { AnnotP(p, t) @! at $sloc }

pat :
  | p=pat_bin
    { p }

return_typ :
  | COLON t=typ { t }

return_typ_nullary :
  | COLON t=typ_nullary { t }

pat_field :
  | x=id
    { {id = x; pat = VarP x @! x.at} @@ at $sloc }
  | x=id EQ p=pat
    { {id = x; pat = p} @@ at $sloc }


(* Declarations *)

dec_var :
  | VAR x=id t=return_typ? EQ e=exp(ob)
    { let e' =
        match t with
        | None -> e
        | Some t -> AnnotE (e, t) @? span t.at e.at
      in VarD(x, e') @? at $sloc }

dec_nonvar :
  | LET p=pat EQ e=exp(ob)
    { let p', e' =
        match p.it with
        | AnnotP (p', t) -> p', AnnotE (e, t) @? p.at
        | _ -> p, e
      in LetD (p', e') @? at $sloc }
  | TYPE x=typ_id tps=typ_params_opt EQ t=typ
    { TypD(x, tps, t) @? at $sloc }
  | s=obj_sort xf=id_opt EQ? efs=obj_body
    { let named, x = xf "object" $sloc in
      let efs' =
        if s.it = Type.Actor then List.map share_expfield efs else efs
      in let_or_exp named x (ObjE(s, efs')) (at $sloc) }
  | s=func_sort_opt FUNC xf=id_opt
      tps=typ_params_opt p=pat_param t=return_typ? fb=func_body
    { (* This is a hack to support local func declarations that return a computed async.
         These should be defined using RHS syntax EQ e to avoid the implicit AsyncE introduction
         around bodies declared as blocks *)
      let e = match fb with
        | (false, e) -> e (* body declared as EQ e *)
        | (true, e) -> (* body declared as immediate block *)
          match t with
          | Some {it = AsyncT _; _} -> AsyncE(e) @? e.at
          | _ -> e
      in
      let named, x = xf "func" $sloc in
      let_or_exp named x (FuncE(x.it, s, tps, p, t, e)) (at $sloc) }
  | s=obj_sort_opt CLASS xf=typ_id_opt
      tps=typ_params_opt p=pat_param t=return_typ? cb=class_body
    { let x, efs = cb in
      let efs' =
        if s.it = Type.Actor then List.map share_expfield efs else efs
      in ClassD(xf "class" $sloc, tps, p, t, s, x, efs') @? at $sloc }
  | IMPORT xf=id_opt EQ? f=TEXT
    { let named, x = xf "import" $sloc in
      let_or_exp named x (ImportE (f, ref "")) (at $sloc) }

dec :
  | d=dec_var
    { d }
  | d=dec_nonvar
    { d }
  | e=exp_nondec(ob)
    { ExpD e @? at $sloc }

dec_list_unamb :  (* does not overlap with exp_field_list_unamb *)
  | e=exp_nondec(ob)
    { [ExpD e @? e.at] }
  | d=dec_nonvar
    { [d] }
  | e=exp_nondec(ob) semicolon ds=seplist(dec, semicolon)
    { (ExpD e @? e.at) :: ds }
  | d=dec_nonvar semicolon ds=seplist(dec, semicolon)
    { d::ds }
  | d=dec_var semicolon ds=dec_list_unamb
    { d::ds }

func_body :
  | EQ e=exp(bl) { (false, e) }
  | e=exp_block { (true, e) }

obj_body :
  | LCURLY efs=seplist(dec_field, semicolon) RCURLY { efs }

class_body :
  | EQ xf=id_opt efs=obj_body { snd (xf "object" $sloc), efs }
  | efs=obj_body { anon "object" (at $sloc) @@ at $sloc, efs }


(* Programs *)

parse_prog :
  | ds=seplist(dec, semicolon) EOF
    { fun filename -> { it = ds; at = at $sloc ; note = filename} }

parse_prog_interactive :
  | ds=seplist(dec, SEMICOLON) SEMICOLON_EOL
    { fun filename -> { it = ds; at = at $sloc ; note = filename} }

%%
