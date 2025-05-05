open Mo_types
open Mo_values

open Source
open Syntax
open Wasm.Sexpr

module type Config = sig
  val include_sources : bool
  val include_types : bool
  val include_type_rep : bool
  val include_docs : Trivia.trivia_info Trivia.PosHashtbl.t option
  val include_parenthetical : bool
  val main_file : string option
end

module Default = struct
  let include_sources = false
  let include_type_rep = false
  let include_types = false
  let include_parenthetical = true (* false for vscode *)
  let include_docs = None
  let main_file = None
end

module Type_pretty = Mo_types.Type.MakePretty (Mo_types.Type.ElideStamps)

module Make (Cfg : Config) = struct
  let ($$) head inner = Node (head, inner)

  let pos p =
    let file = match Cfg.main_file with
    | Some f when f <> p.file -> p.file
    | _ -> ""
    in "Pos" $$ [Atom file; Atom (string_of_int p.line); Atom (string_of_int p.column)]
  let source at it = if Cfg.include_sources && at <> Source.no_region then "@" $$ [pos at.left; pos at.right; it] else it

  let typ t = Atom (Type_pretty.string_of_typ t)

  let trivia at it =
    match Cfg.include_docs with
    | Some table ->
      let rec lookup_trivia (line, column) =
        Trivia.PosHashtbl.find_opt table Trivia.{ line; column }
      and find_trivia (parser_pos : Source.region) : Trivia.trivia_info =
        lookup_trivia Source.(parser_pos.left.line, parser_pos.left.column) |> Option.get
      in
      (match Trivia.doc_comment_of_trivia_info (find_trivia at) with
      | Some s -> "*" $$ [Atom s; it]
      | None -> it)
    | None -> it

  let eff (eff : Mo_types.Type.eff) = match eff with
  | Mo_types.Type.Triv -> Atom "Triv"
  | Mo_types.Type.Await -> Atom "Await"

  let annot_arrange_typ t it =
    if Cfg.include_types
    then
      if Cfg.include_type_rep
      then ":" $$ [it; typ t; Arrange_type.typ t]
      else ":" $$ [it; typ t]
    else it

  let annot_typ t it =
    if Cfg.include_types
    then ":" $$ [it; typ t]
    else it

  let annot ?arrange_typ note =
    if Option.value ~default:false arrange_typ
    then annot_arrange_typ note.note_typ
    else annot_typ note.note_typ

  let id i = source i.at ("ID" $$ [Atom i.it])
  let tag i = Atom ("#" ^ i.it)

  (* TODO: For the language server, we occasionally need not only the resulting
     [typ] of a node but also its [Arrange_type.typ] (which motivated these
     changes). Arranging the type for every node would be expensive; there
     would be considerable duplication as there is no sharing mechanism
     currently. As a compromise, we currently annotate only the nodes that
     currently matter for the language server, i.e. [DotE] nodes and the left
     expression of a [DotE] node (the latter is handled in [exp]). *)
  let should_arrange_exp_typ ?arrange_typ e =
    match e.it with
    | DotE _ -> true
    | _ -> Option.value ~default:false arrange_typ

  let rec exp ?arrange_typ e = source e.at (annot ~arrange_typ:(should_arrange_exp_typ ?arrange_typ e) e.note (match e.it with
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
    | ObjBlockE (eo, s, nt, dfs) -> "ObjBlockE" $$
                                               parenthetical eo
                                               [obj_sort s;
                                                match nt with
                                                | None, None -> Atom "_"
                                                | None, Some t -> typ t
                                                | Some id, Some t -> id.it $$ [Atom ":"; typ t]
                                                | Some id, None -> Atom id.it
                                                ] @ List.map dec_field dfs
    | ObjE ([], efs)      -> "ObjE"      $$ List.map exp_field efs
    | ObjE (bases, efs)   -> "ObjE"      $$ exps bases @ [Atom "with"] @ List.map exp_field efs
    | DotE (e, x)         -> "DotE"      $$ [exp ~arrange_typ:true e; id x]
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
    | CallE (par_opt, e1, ts, e2) -> "CallE" $$ parenthetical par_opt ([exp e1] @ inst ts @ [exp e2])
    | BlockE ds           -> "BlockE"  $$ List.map dec ds
    | NotE e              -> "NotE"    $$ [exp e]
    | AndE (e1, e2)       -> "AndE"    $$ [exp e1; exp e2]
    | OrE (e1, e2)        -> "OrE"     $$ [exp e1; exp e2]
    | ImpliesE (e1, e2)   -> "ImpliesE"$$ [exp e1; exp e2]
    | OldE e              -> "OldE"    $$ [exp e]
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
    | AsyncE (par_opt, Type.Fut, tb, e) -> "AsyncE" $$ parenthetical par_opt [typ_bind tb; exp e]
    | AsyncE (None, Type.Cmp, tb, e) -> "AsyncE*" $$ [typ_bind tb; exp e]
    | AsyncE (Some _ , Type.Cmp, tb, e) -> assert false;
    | AwaitE (Type.Fut, e)     -> "AwaitE"  $$ [exp e]
    | AwaitE (Type.Cmp, e)     -> "AwaitE*" $$ [exp e]
    | AssertE (Runtime, e)       -> "AssertE" $$ [exp e]
    | AssertE (Static, e)        -> "Static_AssertE" $$ [exp e]
    | AssertE (Invariant, e)     -> "Invariant" $$ [exp e]
    | AssertE (Precondition, e)  -> "Precondition" $$ [exp e]
    | AssertE (Postcondition, e) -> "Postcondition" $$ [exp e]
    | AssertE (Loop_entry, e)    -> "Loop_entry" $$ [exp e]
    | AssertE (Loop_continue, e) -> "Loop_continue" $$ [exp e]
    | AssertE (Loop_exit, e)     -> "Loop_exit" $$ [exp e]
    | AssertE (Loop_invariant, e)-> "Loop_invariant" $$ [exp e]
    | AssertE (Concurrency s, e) -> "Concurrency"^s $$ [exp e]
    | AnnotE (e, t)       -> "AnnotE"  $$ [exp e; typ t]
    | OptE e              -> "OptE"    $$ [exp e]
    | DoOptE e            -> "DoOptE"  $$ [exp e]
    | BangE e             -> "BangE"   $$ [exp e]
    | TagE (i, e)         -> "TagE"    $$ [id i; exp e]
    | PrimE p             -> "PrimE"   $$ [Atom p]
    | ImportE (f, _fp)    -> "ImportE" $$ [Atom f]
    | ThrowE e            -> "ThrowE"  $$ [exp e]
    | TryE (e, cs, None)  -> "TryE"    $$ [exp e] @ List.map catch cs
    | TryE (e, cs, Some f)-> "TryE"    $$ [exp e] @ List.map catch cs @ Atom ";" :: [exp f]
    | IgnoreE e           -> "IgnoreE" $$ [exp e]))

  and exps es = List.map (exp ?arrange_typ:None) es

  and inst inst = match inst.it with
    | None -> []
    | Some (false, ts) -> List.map typ ts
    | Some (true, ts) -> Atom "system" :: List.map typ ts

  and pat p = source p.at (annot_typ p.note (match p.it with
    | WildP           -> Atom "WildP"
    | VarP x          -> "VarP"       $$ [id x]
    | TupP ps         -> "TupP"       $$ List.map pat ps
    | ObjP ps         -> "ObjP"       $$ List.map pat_field ps
    | AnnotP (p, t)   -> "AnnotP"     $$ [pat p; typ t]
    | LitP l          -> "LitP"       $$ [lit !l]
    | SignP (uo, l)   -> "SignP"      $$ [Arrange_ops.unop uo ; lit !l]
    | OptP p          -> "OptP"       $$ [pat p]
    | TagP (i, p)     -> "TagP"       $$ [tag i; pat p]
    | AltP (p1, p2)   -> "AltP"       $$ [pat p1; pat p2]
    | ParP p          -> "ParP"       $$ [pat p]))

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
    | PreLit (s, p)  -> "PreLit"    $$ [ Atom s; Arrange_type.prim p ]

  and case c = source c.at ("case" $$ [pat c.it.pat; exp c.it.exp])

  and catch c = "catch" $$ [pat c.it.pat; exp c.it.exp]

  and pat_field pf = source pf.at (pf.it.id.it $$ [pat pf.it.pat])

  (* conditionally include parenthetical to avoid breaking lsp *)
  and parenthetical eo sexps =
    if Cfg.include_parenthetical then
      (match eo with None -> Atom "_" | Some e -> exp e) ::
      sexps
    else sexps

  and obj_sort s = match s.it with
    | Type.Object -> Atom "Object"
    | Type.Actor -> Atom "Actor"
    | Type.Module -> Atom "Module"
    | Type.Memory -> Atom "Memory"

  and shared_pat sp = match sp.it with
    | Type.Local -> Atom "Local"
    | Type.Shared (Type.Write, p) -> "Shared" $$ [pat p]
    | Type.Shared (Type.Query, p) -> "Query" $$ [pat p]
    | Type.Shared (Type.Composite, p) -> "Composite" $$ [pat p]

  and func_sort s = match s.it with
    | Type.Local -> Atom "Local"
    | Type.Shared Type.Write -> Atom "Shared"
    | Type.Shared Type.Query -> Atom "Query"
    | Type.Shared Type.Composite -> Atom "Composite"

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

  and typ_field (tf : typ_field) = source tf.at (match tf.it with
    | ValF (lab, t, m) -> "ValF" $$ [id lab; typ t; mut m]
    | TypF (lab, tbs, t) -> "TypF" $$ id lab :: List.map typ_bind tbs @ [typ t])

  and typ_item ((id, ty) : typ_item) =
    match id with
    | None -> [typ ty]
    | Some { it;_ } -> [Atom it; typ ty]

  and typ_tag (tt : typ_tag)
    = source tt.at (tt.it.tag.it $$ [typ tt.it.typ])

  and typ_bind (tb : typ_bind)
    = source tb.at (tb.it.var.it $$ [typ tb.it.bound])

  and dec_field (df : dec_field)
    = trivia df.at (source df.at ("DecField" $$ [dec df.it.dec; vis df.it.vis; stab df.it.stab]))

  and exp_field (ef : exp_field)
    = source ef.at ("ExpField" $$ [mut ef.it.mut; id ef.it.id; exp ef.it.exp])

  and operator_type t = Atom (Type.string_of_typ t)

  and path p = match p.it with
    | IdH i -> "IdH" $$ [id i]
    | DotH (p, i) -> "DotH" $$ [path p; id i]

  and typ t = source t.at (annot_typ t.note (match t.it with
  | PathT (p, ts) -> "PathT" $$ [path p] @ List.map typ ts
  | PrimT p -> "PrimT" $$ [Atom p]
  | ObjT (s, ts) -> "ObjT" $$ [obj_sort s] @ List.map typ_field ts
  | ArrayT (m, t) -> "ArrayT" $$ [mut m; typ t]
  | OptT t -> "OptT" $$ [typ t]
  | VariantT cts -> "VariantT" $$ List.map typ_tag cts
  | TupT ts -> "TupT" $$ List.concat_map typ_item ts
  | FuncT (s, tbs, at, rt) -> "FuncT" $$ [func_sort s] @ List.map typ_bind tbs @ [ typ at; typ rt]
  | AsyncT (Type.Fut, t1, t2) -> "AsyncT" $$ [typ t1; typ t2]
  | AsyncT (Type.Cmp, t1, t2) -> "AsyncT*" $$ [typ t1; typ t2]
  | AndT (t1, t2) -> "AndT" $$ [typ t1; typ t2]
  | OrT (t1, t2) -> "OrT" $$ [typ t1; typ t2]
  | ParT t -> "ParT" $$ [typ t]
  | NamedT (id, t) -> "NamedT" $$ [Atom id.it; typ t]))

  and dec d = trivia d.at (source d.at (match d.it with
    | ExpD e -> "ExpD" $$ [exp e]
    | LetD (p, e, Some f) -> "LetD" $$ [pat p; exp e; exp f]
    | LetD (p, e, None) -> "LetD" $$ [pat p; exp e]
    | VarD (x, e) -> "VarD" $$ [id x; exp e]
    | TypD (x, tp, t) ->
      "TypD" $$ [id x] @ List.map typ_bind tp @ [typ t]
    | ClassD (eo, sp, s, x, tp, p, rt, i, dfs) ->
      "ClassD" $$
        parenthetical eo
        (shared_pat sp :: id x :: List.map typ_bind tp @ [
        pat p;
        (match rt with None -> Atom "_" | Some t -> typ t);
        obj_sort s;
        id i
      ] @ List.map dec_field dfs)))

  and prog p = "Prog" $$ List.map dec p.it
end

module type S = module type of Make (Default)

(* Defaults *)
include Make (Default)
