open Source

open Syntax

module T = Mo_types.Type
module M = Mo_def.Syntax

let fail sexp =
  failwith (Wasm.Sexpr.to_string 80 sexp)

type sort = Field | Local | Method

module Env = T.Env

type ctxt =
  { self : string option;
    ids : sort T.Env.t
  }

let self ctxt at =
  match ctxt.self with
  | Some id -> { it = LocalVar ({ it = id; at; note = NoInfo},
                                { it = RefT; at; note = NoInfo });
                   at;
                   note = NoInfo }
  | _ -> failwith "no self"

let rec unit (u : Mo_def.Syntax.comp_unit) : prog =
  let { M.imports; M.body } = u.it in
  match body.it with
  | M.ActorU(id_opt, decs) ->
     let ctxt = { self = None; ids = Env.empty } in
     let (ctxt', mk_is) = dec_fields ctxt decs in
     let is = List.map (fun mk_i -> mk_i ctxt') mk_is in
     { it = is;
       at = body.at;
       note = NoInfo
     }
  | _ -> assert false

and dec_fields (ctxt : ctxt) (ds : M.dec_field list) =
  match ds with
  | [] ->
    (ctxt, [])
  | d :: ds ->
    let ctxt, mk_i = dec_field ctxt d in
    let ctxt, mk_is = dec_fields ctxt ds in
    (ctxt, mk_i::mk_is)

and dec_field ctxt d =
  let (ctxt, mk_i) = dec_field' ctxt d.it in
  (ctxt,
   fun ctxt' ->
     let (i, info) = mk_i ctxt' in
     { it = i;
       at = d.at;
       note = info })

and dec_field' ctxt d =
  match d.M.dec.it with
  (*  | ExpD e -> "ExpD" $$ [exp e ] *)

  | M.VarD (x, e) ->
     { ctxt with ids = Env.add x.it Field ctxt.ids },
     (* TODO: translate e? *)
     fun ctxt' ->
       (FieldI(id x, tr_typ e.note.M.note_typ),
       NoInfo)
  | M.(LetD ({it=VarP f;_},
             {it=FuncE(x, sp, tp, p, t_opt, sugar,
                       {it = AsyncE (_, e); _} );_})) -> (* ignore async *)
     { ctxt with ids = Env.add f.it Method ctxt.ids },
     fun ctxt' ->
       let self_id = {it = "$Self"; at = Source.no_region; note = NoInfo } in
       let ctxt'' = { ctxt' with self = Some self_id.it }
       in (* TODO: add args (and rets?) *)
       (MethodI(id f, (self_id, {it = RefT; at = Source.no_region; note = NoInfo})::args p, rets t_opt, [], [], Some (stmt ctxt'' e)),
        NoInfo)
  | _ -> fail (Mo_def.Arrange.dec d.M.dec)

(*
  | TypD (x, tp, t) ->
    "TypD" $$ [id x] @ List.map typ_bind tp @ [typ t]
  | ClassD (sp, x, tp, p, rt, s, i', dfs) ->
    "ClassD" $$ shared_pat sp :: id x :: List.map typ_bind tp @ [
      pat p;
      (match rt with None -> Atom "_" | Some t -> typ t);
      obj_sort s; id i'
    ] @ List.map dec_field dfs
*)

and args p = match p.it with
  | M.TupP ps ->
    List.map (fun {it = M.VarP x; note; _} -> (id x, tr_typ note)) ps

and block ctxt at ds =
  let ctxt, mk_ss = decs ctxt ds in
  { it = mk_ss ctxt;
    at;
    note = NoInfo }

and decs ctxt ds =
  match ds with
  | [] -> (ctxt, fun ctxt' -> ([],[]))
  | d::ds' ->
    let (ctxt1, mk_s) = dec ctxt d in
    let (ctxt2, mk_ss) = decs ctxt1 ds' in
    (ctxt2,
     fun ctxt' ->
       let (l, s) = mk_s ctxt' in
       let (ls, ss) = mk_ss ctxt' in
       (l @ ls, s @ ss))

and dec ctxt d =
  match d.it with
  | M.VarD (x, e) ->
     (* TODO: translate e? *)
    { ctxt with ids = Env.add x.it Local ctxt.ids },
    fun ctxt' ->
      ([{ it = (id x, tr_typ e.note.M.note_typ);
        at = d.at;
        note = NoInfo }],
       [{ it = VarAssignS (id x, exp ctxt' e);
          at = d.at;
          note = NoInfo } ])
  | M.(LetD ({it=VarP x;_}, e))->
     { ctxt with ids = Env.add x.it Local ctxt.ids },
     fun ctxt' ->
       ([{ it = (id x, tr_typ e.note.M.note_typ);
           at = d.at;
           note = NoInfo }],
        [{ it = VarAssignS (id x, exp ctxt' e);
           at = d.at;
           note = NoInfo }])
  | M.(ExpD e) ->
     (ctxt,
      fun ctxt' ->
        let s = stmt ctxt' e in
        s.it)
  | _ -> fail (Mo_def.Arrange.dec d)

and stmt ctxt (s : M.exp) : seqn =
  match s.it with
  | M.TupE [] ->
     block ctxt s.at []
  | M.BlockE ds ->
     block ctxt s.at ds
  | M.IfE(e, s1, s2) ->
     { it =
         ([],
          [ { it = IfS(exp ctxt e, stmt ctxt s1, stmt ctxt s2);
              at = s.at;
              note = NoInfo } ]);
       at = s.at;
       note = NoInfo }
  | M.(AwaitE({ it = AsyncE (_, e); _ })) -> (* gross hack *)
     { it =
         ([],
          (* TODO: add havoc etc *)
          [ { it = SeqnS (stmt ctxt e);
              at = s.at;
              note = NoInfo } ]);
       at = s.at;
       note = NoInfo }
  | M.WhileE(e, s1) ->
     { it =
         ([],
          [ { it = WhileS(exp ctxt e, [], stmt ctxt s1); (* TODO: invariant *)
              at = s.at;
              note = NoInfo } ]);
       at = s.at;
       note = NoInfo }
  | M.(AssignE({it = VarE x; _}, e2)) ->
     match Env.find x.it ctxt.ids with
     | Local ->
       let loc = { it = x.it; at = x.at; note = NoInfo } in
       { it =
           ([],
            [ { it = VarAssignS(loc, exp ctxt e2);
                at = s.at;
                note = NoInfo } ]);
         at = s.at;
         note = NoInfo }
     | Field ->
       let fld = (self ctxt x.at, id x) in
       { it =
           ([],
            [ { it = FieldAssignS(fld, exp ctxt e2);
                at = s.at;
                note = NoInfo } ]);
         at = s.at;
         note = NoInfo }
        
  | _ -> fail (Mo_def.Arrange.exp s)
          
(*    
  | M.AssignE({it = VarE id;_}, e2) when isField e1->
     { it =
         ([],
          [ { it = FieldAssignS((), exp e2);
              at = s.at;
              note = NoInfo } ]);
       at = s.at;
       note = NoInfo }
*)

and isLocal id = true (* fix me *)

and exp ctxt e =
  let (e', info) = exp' ctxt e in
  { it = e';
    at = e.at;
    note = info }
and exp' ctxt (e : M.exp) =
  match e.it with
  | M.VarE x (* when Env.find x.it ctxt = Local *) ->
    begin
     match Env.find x.it ctxt.ids with
     | Local ->
        (LocalVar (id x, tr_typ e.note.note_typ),
        NoInfo)
     | Field ->
        (FldAcc (self ctxt x.at, id x),
         NoInfo)
     | _ -> fail (Mo_def.Arrange.exp e)
    end
(*
  | M.VarE x when Env.find x.it ctxt = Field ->
     (*TODO: need environment to distinguish fields from locals *)
     (LocalVar (id x, tr_typ e.note.note_typ),
      NoInfo)
*)
  | M.LitE r ->
    begin match !r with
    | M.BoolLit b ->
       (BoolLitE b, NoInfo)
    | M.IntLit i ->
       (IntLitE i, NoInfo)
    | _ -> fail (Mo_def.Arrange.exp e)
    end
  | M.NotE e ->
     NotE (exp ctxt e), NoInfo
  | M.RelE (ot, e1, EqOp, e2) ->
     EqCmpE (exp ctxt e1, exp ctxt e2), NoInfo
  | M.RelE (ot, e1, GtOp, e2) ->
     GtCmpE (exp ctxt e1, exp ctxt e2), NoInfo
  | M.OrE (e1, e2) ->
     OrE (exp ctxt e1, exp ctxt e2), NoInfo
  | M.AndE (e1, e2) ->
     AndE (exp ctxt e1, exp ctxt e2), NoInfo
  | _ -> fail (Mo_def.Arrange.exp e)
(*           
  | VarE x              -> 
  | LitE l              -> "LitE"      $$ [lit !l]
  | ActorUrlE e         -> "ActorUrlE" $$ [exp e]
  | UnE (ot, uo, e)     -> "UnE"       $$ [operator_type !ot; Arrange_ops.unop uo; exp e]
  | BinE (ot, e1, bo, e2) -> "BinE"    $$ [operator_type !ot; exp e1; Arrange_ops.binop bo; exp e2]
  | RelE (ot, e1, ro, e2) -> "RelE"    $$ [operator_type !ot; exp e1; Arrange_ops.relop ro; exp e2]
  | ShowE (ot, e)       -> "ShowE"     $$ [operator_type !ot; exp e]
  | ToCandidE es        -> "ToCandidE"   $$ exps es
  | FromCandidE e       -> "FromCandidE" $$ [exp e]
  | TupE es             -> "TupE"      $$ exps es
  | ProjE (e, i)        -> "ProjE"     $$ [exp e; Atom (string_of_int i)]
  | ObjBlockE (s, dfs)  -> "ObjBlockE" $$ [obj_sort s] @ List.map dec_field dfs
  | ObjE ([], efs)      -> "ObjE"      $$ List.map exp_field efs
  | ObjE (bases, efs)   -> "ObjE"      $$ exps bases @ [Atom "with"] @ List.map exp_field efs
  | DotE (e, x)         -> "DotE"      $$ [exp e; id x]
  | AssignE (e1, e2)    -> "AssignE"   $$ [exp e1; exp e2]
  | ArrayE (m, es)      -> "ArrayE"    $$ [mut m] @ exps es
  | IdxE (e1, e2)       -> "IdxE"      $$ [exp e1; exp e2]
  | FuncE (x, sp, tp, p, t, sugar, e') ->
    "FuncE" $$ [
      Atom (Type.string_of_typ e.note.note_typ);
      shared_pat sp;
      Atom x] @
      List.map typ_bind tp @ [
      pat p;
      (match t with None -> Atom "_" | Some t -> typ t);
      Atom (if sugar then "" else "=");
      exp e'
    ]
  | CallE (e1, ts, e2)  -> "CallE"   $$ [exp e1] @ inst ts @ [exp e2]
  | BlockE ds           -> "BlockE"  $$ List.map dec ds
  | NotE e              -> "NotE"    $$ [exp e]
  | AndE (e1, e2)       -> "AndE"    $$ [exp e1; exp e2]
  | OrE (e1, e2)        -> "OrE"     $$ [exp e1; exp e2]
  | IfE (e1, e2, e3)    -> "IfE"     $$ [exp e1; exp e2; exp e3]
  | SwitchE (e, cs)     -> "SwitchE" $$ [exp e] @ List.map case cs
  | WhileE (e1, e2)     -> "WhileE"  $$ [exp e1; exp e2]
  | LoopE (e1, None)    -> "LoopE"   $$ [exp e1]
  | LoopE (e1, Some e2) -> "LoopE"   $$ [exp e1; exp e2]
  | ForE (p, e1, e2)    -> "ForE"    $$ [pat p; exp e1; exp e2]
  | LabelE (i, t, e)    -> "LabelE"  $$ [id i; typ t; exp e]
  | DebugE e            -> "DebugE"  $$ [exp e]
  | BreakE (i, e)       -> "BreakE"  $$ [id i; exp e]
  | RetE e              -> "RetE"    $$ [exp e]
  | AsyncE (tb, e)      -> "AsyncE"  $$ [typ_bind tb; exp e]
  | AwaitE e            -> "AwaitE"  $$ [exp e]
  | AssertE e           -> "AssertE" $$ [exp e]
  | AnnotE (e, t)       -> "AnnotE"  $$ [exp e; typ t]
  | OptE e              -> "OptE"    $$ [exp e]
  | DoOptE e            -> "DoOptE"  $$ [exp e]
  | BangE e             -> "BangE"   $$ [exp e]
  | TagE (i, e)         -> "TagE"    $$ [id i; exp e]
  | PrimE p             -> "PrimE"   $$ [Atom p]
  | ImportE (f, _fp)    -> "ImportE" $$ [Atom f]
  | ThrowE e            -> "ThrowE"  $$ [exp e]
  | TryE (e, cs)        -> "TryE"    $$ [exp e] @ List.map catch cs
  | IgnoreE e           -> "IgnoreE" $$ [exp e]
*)


and rets t_opt =
  match t_opt with
  | None -> []
  | Some t ->
    (match T.normalize t.note with
     | T.Tup [] -> []
     | T.Async (_, _) -> [])

and id id = { it = id.it; at = id.at; note = NoInfo }

and tr_typ typ =
  { it = tr_typ' typ;
    at = Source.no_region;
    note = NoInfo }
and tr_typ' typ =
  match T.normalize typ with
  | T.Prim T.Int -> IntT
  | T.Prim T.Bool -> BoolT



(*       
let rec exp e = match e.it with
  | VarE x              -> "VarE"      $$ [id x]
  | LitE l              -> "LitE"      $$ [lit !l]
  | ActorUrlE e         -> "ActorUrlE" $$ [exp e]
  | UnE (ot, uo, e)     -> "UnE"       $$ [operator_type !ot; Arrange_ops.unop uo; exp e]
  | BinE (ot, e1, bo, e2) -> "BinE"    $$ [operator_type !ot; exp e1; Arrange_ops.binop bo; exp e2]
  | RelE (ot, e1, ro, e2) -> "RelE"    $$ [operator_type !ot; exp e1; Arrange_ops.relop ro; exp e2]
  | ShowE (ot, e)       -> "ShowE"     $$ [operator_type !ot; exp e]
  | ToCandidE es        -> "ToCandidE"   $$ exps es
  | FromCandidE e       -> "FromCandidE" $$ [exp e]
  | TupE es             -> "TupE"      $$ exps es
  | ProjE (e, i)        -> "ProjE"     $$ [exp e; Atom (string_of_int i)]
  | ObjBlockE (s, dfs)  -> "ObjBlockE" $$ [obj_sort s] @ List.map dec_field dfs
  | ObjE ([], efs)      -> "ObjE"      $$ List.map exp_field efs
  | ObjE (bases, efs)   -> "ObjE"      $$ exps bases @ [Atom "with"] @ List.map exp_field efs
  | DotE (e, x)         -> "DotE"      $$ [exp e; id x]
  | AssignE (e1, e2)    -> "AssignE"   $$ [exp e1; exp e2]
  | ArrayE (m, es)      -> "ArrayE"    $$ [mut m] @ exps es
  | IdxE (e1, e2)       -> "IdxE"      $$ [exp e1; exp e2]
  | FuncE (x, sp, tp, p, t, sugar, e') ->
    "FuncE" $$ [
      Atom (Type.string_of_typ e.note.note_typ);
      shared_pat sp;
      Atom x] @
      List.map typ_bind tp @ [
      pat p;
      (match t with None -> Atom "_" | Some t -> typ t);
      Atom (if sugar then "" else "=");
      exp e'
    ]
  | CallE (e1, ts, e2)  -> "CallE"   $$ [exp e1] @ inst ts @ [exp e2]
  | BlockE ds           -> "BlockE"  $$ List.map dec ds
  | NotE e              -> "NotE"    $$ [exp e]
  | AndE (e1, e2)       -> "AndE"    $$ [exp e1; exp e2]
  | OrE (e1, e2)        -> "OrE"     $$ [exp e1; exp e2]
  | IfE (e1, e2, e3)    -> "IfE"     $$ [exp e1; exp e2; exp e3]
  | SwitchE (e, cs)     -> "SwitchE" $$ [exp e] @ List.map case cs
  | WhileE (e1, e2)     -> "WhileE"  $$ [exp e1; exp e2]
  | LoopE (e1, None)    -> "LoopE"   $$ [exp e1]
  | LoopE (e1, Some e2) -> "LoopE"   $$ [exp e1; exp e2]
  | ForE (p, e1, e2)    -> "ForE"    $$ [pat p; exp e1; exp e2]
  | LabelE (i, t, e)    -> "LabelE"  $$ [id i; typ t; exp e]
  | DebugE e            -> "DebugE"  $$ [exp e]
  | BreakE (i, e)       -> "BreakE"  $$ [id i; exp e]
  | RetE e              -> "RetE"    $$ [exp e]
  | AsyncE (tb, e)      -> "AsyncE"  $$ [typ_bind tb; exp e]
  | AwaitE e            -> "AwaitE"  $$ [exp e]
  | AssertE e           -> "AssertE" $$ [exp e]
  | AnnotE (e, t)       -> "AnnotE"  $$ [exp e; typ t]
  | OptE e              -> "OptE"    $$ [exp e]
  | DoOptE e            -> "DoOptE"  $$ [exp e]
  | BangE e             -> "BangE"   $$ [exp e]
  | TagE (i, e)         -> "TagE"    $$ [id i; exp e]
  | PrimE p             -> "PrimE"   $$ [Atom p]
  | ImportE (f, _fp)    -> "ImportE" $$ [Atom f]
  | ThrowE e            -> "ThrowE"  $$ [exp e]
  | TryE (e, cs)        -> "TryE"    $$ [exp e] @ List.map catch cs
  | IgnoreE e           -> "IgnoreE" $$ [exp e]

and exps es = List.map exp es

and inst inst = match inst.it with
  | None -> []
  | Some ts -> List.map typ ts

and pat p = match p.it with
  | WildP           -> Atom "WildP"
  | VarP x          -> "VarP"       $$ [id x]
  | TupP ps         -> "TupP"       $$ List.map pat ps
  | ObjP ps         -> "ObjP"       $$ List.map pat_field ps
  | AnnotP (p, t)   -> "AnnotP"     $$ [pat p; typ t]
  | LitP l          -> "LitP"       $$ [lit !l]
  | SignP (uo, l)   -> "SignP"      $$ [Arrange_ops.unop uo ; lit !l]
  | OptP p          -> "OptP"       $$ [pat p]
  | TagP (i, p)     -> "TagP"       $$ [tag i; pat p]
  | AltP (p1,p2)    -> "AltP"       $$ [pat p1; pat p2]
  | ParP p          -> "ParP"       $$ [pat p]

and lit = function
  | NullLit       -> Atom "NullLit"
  | BoolLit true  -> "BoolLit"   $$ [ Atom "true" ]
  | BoolLit false -> "BoolLit"   $$ [ Atom "false" ]
  | NatLit n      -> "NatLit"    $$ [ Atom (Numerics.Nat.to_pretty_string n) ]
  | Nat8Lit n     -> "Nat8Lit"   $$ [ Atom (Numerics.Nat8.to_pretty_string n) ]
  | Nat16Lit n    -> "Nat16Lit"  $$ [ Atom (Numerics.Nat16.to_pretty_string n) ]
  | Nat32Lit n    -> "Nat32Lit"  $$ [ Atom (Numerics.Nat32.to_pretty_string n) ]
  | Nat64Lit n    -> "Nat64Lit"  $$ [ Atom (Numerics.Nat64.to_pretty_string n) ]
  | IntLit i      -> "IntLit"    $$ [ Atom (Numerics.Int.to_pretty_string i) ]
  | Int8Lit i     -> "Int8Lit"   $$ [ Atom (Numerics.Int_8.to_pretty_string i) ]
  | Int16Lit i    -> "Int16Lit"  $$ [ Atom (Numerics.Int_16.to_pretty_string i) ]
  | Int32Lit i    -> "Int32Lit"  $$ [ Atom (Numerics.Int_32.to_pretty_string i) ]
  | Int64Lit i    -> "Int64Lit"  $$ [ Atom (Numerics.Int_64.to_pretty_string i) ]
  | FloatLit f    -> "FloatLit"  $$ [ Atom (Numerics.Float.to_pretty_string f) ]
  | CharLit c     -> "CharLit"   $$ [ Atom (string_of_int c) ]
  | TextLit t     -> "TextLit"   $$ [ Atom t ]
  | BlobLit b     -> "BlobLit"   $$ [ Atom b ]
  | PreLit (s,p)  -> "PreLit"    $$ [ Atom s; Arrange_type.prim p ]

and case c = "case" $$ [pat c.it.pat; exp c.it.exp]

and catch c = "catch" $$ [pat c.it.pat; exp c.it.exp]

and pat_field pf = pf.it.id.it $$ [pat pf.it.pat]

and obj_sort s = match s.it with
  | Type.Object -> Atom "Object"
  | Type.Actor -> Atom "Actor"
  | Type.Module -> Atom "Module"
  | Type.Memory -> Atom "Memory"

and shared_pat sp = match sp.it with
  | Type.Local -> Atom "Local"
  | Type.Shared (Type.Write, p) -> "Shared" $$ [pat p]
  | Type.Shared (Type.Query, p) -> "Query" $$ [pat p]

and func_sort s = match s.it with
  | Type.Local -> Atom "Local"
  | Type.Shared Type.Write -> Atom "Shared"
  | Type.Shared Type.Query -> Atom "Query"

and mut m = match m.it with
  | Const -> Atom "Const"
  | Var   -> Atom "Var"

and vis v = match v.it with
  | Public None -> Atom "Public"
  | Public (Some m) -> "Public" $$ [Atom m]
  | Private -> Atom "Private"
  | System -> Atom "System"

and stab s_opt = match s_opt with
  | None -> Atom "(Flexible)"
  | Some s ->
    (match s.it with
    | Flexible -> Atom "Flexible"
    | Stable -> Atom "Stable")

and typ_field (tf : typ_field) = match tf.it with
  | ValF (id, t, m) -> id.it $$ [typ t; mut m]
  | TypF (id', tbs, t) ->
      "TypF" $$ [id id'] @ List.map typ_bind tbs @ [typ t]
and typ_item ((id, ty) : typ_item) =
  match id with
  | None -> [typ ty]
  | Some { it;_ } -> [Atom it; typ ty]

and typ_tag (tt : typ_tag)
  = tt.it.tag.it $$ [typ tt.it.typ]

and typ_bind (tb : typ_bind)
  = tb.it.var.it $$ [typ tb.it.bound]

and dec_field (df : dec_field)
  = "DecField" $$ [dec df.it.dec; vis df.it.vis; stab df.it.stab]

and exp_field (ef : exp_field)
  = "ExpField" $$ [mut ef.it.mut; id ef.it.id; exp ef.it.exp]

and operator_type t = Atom (Type.string_of_typ t)

and path p = match p.it with
  | IdH i -> "IdH" $$ [id i]
  | DotH (p,i) -> "DotH" $$ [path p; id i]

and typ t = match t.it with
  | PathT (p, ts) -> "PathT" $$ [path p] @ List.map typ ts
  | PrimT p -> "PrimT" $$ [Atom p]
  | ObjT (s, ts) -> "ObjT" $$ [obj_sort s] @ List.map typ_field ts
  | ArrayT (m, t) -> "ArrayT" $$ [mut m; typ t]
  | OptT t -> "OptT" $$ [typ t]
  | VariantT cts -> "VariantT" $$ List.map typ_tag cts
  | TupT ts -> "TupT" $$ List.concat_map typ_item ts
  | FuncT (s, tbs, at, rt) -> "FuncT" $$ [func_sort s] @ List.map typ_bind tbs @ [ typ at; typ rt]
  | AsyncT (t1, t2) -> "AsyncT" $$ [typ t1; typ t2]
  | AndT (t1, t2) -> "AndT" $$ [typ t1; typ t2]
  | OrT (t1, t2) -> "OrT" $$ [typ t1; typ t2]
  | ParT t -> "ParT" $$ [typ t]
  | NamedT (id, t) -> "NamedT" $$ [Atom id.it; typ t]

and dec d = match d.it with
  | ExpD e -> "ExpD" $$ [exp e ]
  | LetD (p, e) -> "LetD" $$ [pat p; exp e]
  | VarD (x, e) -> "VarD" $$ [id x; exp e]
  | TypD (x, tp, t) ->
    "TypD" $$ [id x] @ List.map typ_bind tp @ [typ t]
  | ClassD (sp, x, tp, p, rt, s, i', dfs) ->
    "ClassD" $$ shared_pat sp :: id x :: List.map typ_bind tp @ [
      pat p;
      (match rt with None -> Atom "_" | Some t -> typ t);
      obj_sort s; id i'
    ] @ List.map dec_field dfs

 *)

                       
