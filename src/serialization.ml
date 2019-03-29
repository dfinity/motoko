open Source
open Ir
module T = Type
open Construct

(*
This transforms
 * the types of shared functions to pass ElemBufs (according to their arity)
 * shared function definitions to call deserialize on the arguments
 * calls to shared functions to call serialize on the arguments
*)

module Transform() = struct

  module ConRenaming = Env.Make(struct type t = T.con let compare = Con.compare end)

  (* the state *)

  (* maps constructors to new constructors (new name, new stamp, new kind)
     it is initialized with the type constructors defined outside here, which are
     not rewritten.

     If we run this translation on two program fragments (e.g. prelude and program)
     we would have to pass down the `con_renaming`. But this is simply the right thing
     to do for a pass that changes the context.
  *)

  let con_renaming = ref ConRenaming.empty

  (* The primitive serialization functions *)
  let deserialize_prim =
    let open Type in
    let var : var = "A" in
    primE "@deserialize"
      (Func (Local, Returns, [{var; bound = Shared}], [Serialized (Var (var, 0))], [(Var (var, 0))]))
  let serialize_prim =
    let open Type in
    let var : var = "A" in
    primE "@serialize"
      (Func (Local, Returns, [{var; bound = Shared}], [Var (var, 0)], [Serialized (Var (var, 0))]))

  let deserialize e =
    let t = T.as_serialized e.note.note_typ in
    callE deserialize_prim [t] e

  let serialize e =
    let t = e.note.note_typ in
    callE serialize_prim [t] e

  let serialized_arg a =
    { it = a.it ^ "/raw"; note = T.Serialized a.note; at = a.at }

  let rec map_tuple n f e = match n, e.it with
    | 0, _ -> e
    | _, TupE es ->
      assert (List.length es = n);
      tupE (List.map f es)
    | _, BlockE (ds, e) ->
      blockE ds (map_tuple n f e)
    | _, _ ->
      let ts = T.as_tup e.note.note_typ in
      assert (List.length ts = n);
      let vs = fresh_vars "tup" ts in
      blockE [letP (seqP (List.map varP vs)) e]
        (tupE (List.map f vs))

  let rec t_typ (t:T.typ) =
    match t with
    | T.Prim _
    | T.Shared
    | T.Any
    | T.Non
    | T.Pre
    | T.Var _ -> t

    | T.Con (c, ts) ->
      T.Con (t_con c, List.map t_typ ts)
    | T.Array t -> T.Array (t_typ t)
    | T.Tup ts -> T.Tup (List.map t_typ ts)
    | T.Func (T.Sharable, c, tbs, t1, t2) ->
      assert (c = T.Returns);
      assert (t2 = []); (* A returning sharable function has no return values *)
      T.Func (T.Sharable, T.Returns, tbs, List.map (fun t -> T.Serialized (t_typ t)) t1, [])
    | T.Func (T.Local, c, tbs, t1, t2) ->
      T.Func (T.Local, c, List.map t_bind tbs, List.map t_typ t1, List.map t_typ t2)
    | T.Opt t -> T.Opt (t_typ t)
    | T.Obj (s, fs) -> T.Obj (s, List.map t_field fs)
    | T.Vrn (s, cs) -> T.Vrn (s, List.map t_field cs)
    | T.Mut t -> T.Mut (t_typ t)

    | T.Serialized t -> assert false (* This transformation should only run once *)
    | T.Async t -> assert false (* Should happen after async-translation *)

  and t_bind {T.var; T.bound} =
    {T.var; T.bound = t_typ bound}

  and t_binds typbinds = List.map t_bind typbinds

  and t_kind k =
    match k with
    | T.Abs(typ_binds,typ) ->
      T.Abs(t_binds typ_binds, t_typ typ)
    | T.Def(typ_binds,typ) ->
      T.Def(t_binds typ_binds, t_typ typ)

  and t_con c =
    match  ConRenaming.find_opt c (!con_renaming) with
    | Some c' -> c'
    | None ->
      let clone = Con.clone c (T.Abs ([], T.Pre)) in
      con_renaming := ConRenaming.add c clone (!con_renaming);
      (* Need to extend con_renaming before traversing the kind *)
      Type.set_kind clone (t_kind (Con.kind c));
      clone

  and t_field {T.lab; T.typ} =
    { T.lab; T.typ = t_typ typ }

  let rec t_exp (exp: exp) =
    { it = t_exp' exp;
      note = { note_typ = t_typ exp.note.note_typ;
               note_eff = exp.note.note_eff};
      at = exp.at;
    }
  and t_exp' (exp:exp) =
    let exp' = exp.it in
    match exp' with
    | CallE (cc, exp1, typs, exp2)  ->
      begin match cc.Value.sort with
      | T.Local ->
        CallE(cc, t_exp exp1, List.map t_typ typs, t_exp exp2)
      | T.Sharable ->
        assert (T.is_unit exp.note.note_typ);
        if cc.Value.n_args = 1
        then
          let exp2' = serialize (t_exp exp2) in
          CallE (cc, t_exp exp1, [], exp2')
        else
          let exp2' = map_tuple cc.Value.n_args serialize (t_exp exp2) in
          CallE (cc, t_exp exp1, [], exp2')
      end
    | FuncE (x, cc, typbinds, args, typT, exp) ->
      begin match cc.Value.sort with
      | T.Local ->
        FuncE (x, cc, t_typ_binds typbinds, t_args args, t_typ typT, t_exp exp)
      | T.Sharable ->
        assert (T.is_unit typT);
        let args' = t_args args in
        let raw_args = List.map serialized_arg args' in
        let body' =
          blockE [letP (tupP (List.map varP (List.map exp_of_arg args')))
                       (tupE (List.map deserialize (List.map exp_of_arg raw_args))) ]
            (t_exp exp) in
        FuncE (x, cc, [], raw_args, T.unit, body')
      end
    | PrimE _
      | LitE _ -> exp'
    | VarE id -> exp'
    | UnE (ot, op, exp1) ->
      UnE (t_typ ot, op, t_exp exp1)
    | BinE (ot, exp1, op, exp2) ->
      BinE (t_typ ot, t_exp exp1, op, t_exp exp2)
    | RelE (ot, exp1, op, exp2) ->
      RelE (t_typ ot, t_exp exp1, op, t_exp exp2)
    | TupE exps ->
      TupE (List.map t_exp exps)
    | OptE exp1 ->
      OptE (t_exp exp1)
    | VrnE (i, e) ->
      assert false
    | ProjE (exp1, n) ->
      ProjE (t_exp exp1, n)
    | DotE (exp1, id) ->
      DotE (t_exp exp1, id)
    | ActorDotE (exp1, id) ->
      ActorDotE (t_exp exp1, id)
    | AssignE (exp1, exp2) ->
      AssignE (t_exp exp1, t_exp exp2)
    | ArrayE (mut, t, exps) ->
      ArrayE (mut, t_typ t, List.map t_exp exps)
    | IdxE (exp1, exp2) ->
      IdxE (t_exp exp1, t_exp exp2)
    | BlockE b ->
      BlockE (t_block b)
    | IfE (exp1, exp2, exp3) ->
      IfE (t_exp exp1, t_exp exp2, t_exp exp3)
    | SwitchE (exp1, cases) ->
      let cases' = List.map
                     (fun {it = {pat;exp}; at; note} ->
                       {it = {pat = t_pat pat ;exp = t_exp exp}; at; note})
                     cases
      in
      SwitchE (t_exp exp1, cases')
    | LoopE exp1 ->
      LoopE (t_exp exp1)
    | LabelE (id, typ, exp1) ->
      LabelE (id, t_typ typ, t_exp exp1)
    | BreakE (id, exp1) ->
      BreakE (id, t_exp exp1)
    | RetE exp1 ->
      RetE (t_exp exp1)
    | AsyncE _ -> assert false
    | AwaitE _ -> assert false
    | AssertE exp1 ->
      AssertE (t_exp exp1)
    | DeclareE (id, typ, exp1) ->
      DeclareE (id, t_typ typ, t_exp exp1)
    | DefineE (id, mut ,exp1) ->
      DefineE (id, mut, t_exp exp1)
    | ActorE (id, ds, fs, typ) ->
      ActorE (id, t_decs ds, t_fields fs, t_typ typ)
    | NewObjE (sort, ids, t) ->
      NewObjE (sort, t_fields ids, t_typ t)

  and t_dec dec = { dec with it = t_dec' dec.it }

  and t_dec' dec' =
    match dec' with
    | TypD con_id -> TypD (t_con con_id)
    | LetD (pat,exp) -> LetD (t_pat pat,t_exp exp)
    | VarD (id,exp) -> VarD (id,t_exp exp)

  and t_decs decs = List.map t_dec decs

  and t_block (ds, exp) = (t_decs ds, t_exp exp)

  and t_fields fs =
    List.map (fun f -> { f with note = t_typ f.note }) fs

  and t_args as_ =
    List.map (fun a -> { a with note = t_typ a.note }) as_

  and t_pat pat =
    { pat with
      it = t_pat' pat.it;
      note = t_typ pat.note }

  and t_pat' pat =
    match pat with
    | WildP
      | LitP _
      | VarP _ ->
      pat
    | TupP pats ->
      TupP (List.map t_pat pats)
    | OptP pat1 ->
      OptP (t_pat pat1)
    | VrnP (id, pat1) ->
      VrnP (id, t_pat pat1)
    | AltP (pat1, pat2) ->
      AltP (t_pat pat1, t_pat pat2)

  and t_typ_bind' {con; bound} =
    {con = t_con con; bound = t_typ bound}

  and t_typ_bind typ_bind =
    { typ_bind with it = t_typ_bind' typ_bind.it }

  and t_typ_binds typbinds = List.map t_typ_bind typbinds

  and t_prog (prog, flavor) = (t_block prog, { flavor with serialized = true })

end

let transform env prog =
  let module T = Transform() in
  (*
  Initialized the con_renaming with those type constructors already in scope.
  Eventually, pipeline will allow us to pass the con_renaming to downstream program
  fragments, then we would simply start with an empty con_renaming and the prelude.
  *)
  Type.ConSet.iter (fun c -> T.con_renaming := T.ConRenaming.add c c (!T.con_renaming)) env.Typing.con_env;
  T.t_prog prog
