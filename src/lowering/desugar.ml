open Ir_def
open Mo_def
open Mo_types
open Mo_values
open Syntax

open Source
open Operator
module S = Syntax
module I = Ir
module T = Type
open Construct

(*
As a first scaffolding, we translate imported files into let-bound
variables with a special, non-colliding name, which we sometimes
want to recognize for better user experience.
*)

let id_of_full_path (fp : string) : string =
  "file$" ^ fp

(* Combinators used in the desugaring *)

let apply_sign op l = Syntax.(match op, l with
  | PosOp, l -> l
  | NegOp, (NatLit n | IntLit n) -> IntLit (Numerics.Int.sub Numerics.Int.zero n)
  | NegOp, Int8Lit n -> Int8Lit (Numerics.Int_8.sub Numerics.Int_8.zero n)
  | NegOp, Int16Lit n -> Int16Lit (Numerics.Int_16.sub Numerics.Int_16.zero n)
  | NegOp, Int32Lit n -> Int32Lit (Numerics.Int_32.sub Numerics.Int_32.zero n)
  | NegOp, Int64Lit n -> Int64Lit (Numerics.Int_64.sub Numerics.Int_64.zero n)
  | _, _ -> raise (Invalid_argument "Invalid signed pattern")
  )

let phrase f x = { x with it = f x.it }

let typ_note : S.typ_note -> Note.t =
  fun {S.note_typ;S.note_eff} -> Note.{def with typ = note_typ; eff = note_eff}

let phrase' f x =
  { x with it = f x.at x.note x.it }

let typed_phrase' f x =
  let n' = typ_note x.note in
  { x with it = f x.at n' x.it; note = n' }


let rec exps es = List.map exp es

and exp e =
    (* We short-cut AnnotE here, so that we get the position of the inner expression *)
    match e.it with
    | S.AnnotE (e', t) -> exp e'
    | _ -> typed_phrase' exp' e

and exp' at note = function
  | S.VarE i -> I.VarE i.it
  | S.ActorUrlE e ->
    I.(PrimE (ActorOfIdBlob note.Note.typ, [url e at]))
  | S.LitE l -> I.LitE (lit !l)
  | S.UnE (ot, o, e) ->
    I.PrimE (I.UnPrim (!ot, o), [exp e])
  | S.BinE (ot, e1, o, e2) ->
    I.PrimE (I.BinPrim (!ot, o), [exp e1; exp e2])
  | S.RelE (ot, e1, Operator.NeqOp, e2) ->
    (notE (primE (I.RelPrim (!ot, Operator.EqOp)) [exp e1; exp e2])).it
  | S.RelE (ot, e1, o, e2) ->
    I.PrimE (I.RelPrim (!ot, o), [exp e1; exp e2])
  | S.ShowE (ot, e) ->
    I.PrimE (I.ShowPrim !ot, [exp e])
  | S.TupE es -> (tupE (exps es)).it
  | S.ProjE (e, i) -> (projE (exp e) i).it
  | S.OptE e -> (optE (exp e)).it
  | S.DoOptE e ->
    I.LabelE ("!", note.Note.typ, optE (exp e))
  | S.BangE e ->
    let ty = note.Note.typ in
    let v = fresh_var "v" ty in
    (switch_optE (exp e)
      (* case null : *)
      (breakE "!" (nullE()))
      (* case ? v : *)
      (varP v) (varE v) ty).it
  | S.ObjBlockE (s, dfs) ->
    obj_block at s None dfs note.Note.typ
  | S.ObjE efs ->
    obj note.Note.typ efs
  | S.TagE (c, e) -> (tagE c.it (exp e)).it
  | S.DotE (e, x) when T.is_array e.note.S.note_typ ->
    (array_dotE e.note.S.note_typ x.it (exp e)).it
  | S.DotE (e, x) when T.is_prim T.Blob e.note.S.note_typ ->
    (blob_dotE  x.it (exp e)).it
  | S.DotE (e, x) when T.is_prim T.Text e.note.S.note_typ ->
    (text_dotE  x.it (exp e)).it
  | S.DotE (e, x) ->
    begin match T.as_obj_sub [x.it] e.note.S.note_typ with
    | T.Actor, _ -> I.PrimE (I.ActorDotPrim x.it, [exp e])
    | _ -> I.PrimE (I.DotPrim x.it, [exp e])
    end
  | S.AssignE (e1, e2) -> I.AssignE (lexp e1, exp e2)
  | S.ArrayE (m, es) ->
    let t = T.as_array note.Note.typ in
    I.PrimE (I.ArrayPrim (mut m, T.as_immut t), exps es)
  | S.IdxE (e1, e2) -> I.PrimE (I.IdxPrim, [exp e1; exp e2])
  | S.FuncE (name, sp, tbs, p, _t_opt, _, e) ->
    let s, po = match sp.it with
      | T.Local -> (T.Local, None)
      | T.Shared (ss, {it = S.WildP; _} ) -> (* don't bother with ctxt pat *)
        (T.Shared ss, None)
      | T.Shared (ss, sp) -> (T.Shared ss, Some sp) in
    let args, wrap, control, res_tys = to_args note.Note.typ po p in
    let tbs' = typ_binds tbs in
    let vars = List.map (fun (tb : I.typ_bind) -> T.Con (tb.it.I.con, [])) tbs' in
    let tys = List.map (T.open_ vars) res_tys in
    I.FuncE (name, s, control, tbs', args, tys, wrap (exp e))
  (* Primitive functions in the prelude have particular shapes *)
  | S.CallE ({it=S.AnnotE ({it=S.PrimE p;_}, _);note;_}, _, e)
    when Lib.String.chop_prefix "num_conv" p <> None ->
    begin match String.split_on_char '_' p with
    | ["num"; "conv"; s1; s2] ->
      let p1 = Type.prim s1 in
      let p2 = Type.prim s2 in
      I.PrimE (I.NumConvTrapPrim (p1, p2), [exp e])
    | _ -> assert false
    end
  | S.CallE ({it=S.AnnotE ({it=S.PrimE p;_}, _);note;_}, _, e)
    when Lib.String.chop_prefix "num_wrap" p <> None ->
    begin match String.split_on_char '_' p with
    | ["num"; "wrap"; s1; s2] ->
      let p1 = Type.prim s1 in
      let p2 = Type.prim s2 in
      I.PrimE (I.NumConvWrapPrim (p1, p2), [exp e])
    | _ -> assert false
    end
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "decodeUtf8";_},_);_}, _, e) ->
    I.PrimE (I.DecodeUtf8, [exp e])
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "encodeUtf8";_},_);_}, _, e) ->
    I.PrimE (I.EncodeUtf8, [exp e])
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "cast";_}, _);note;_}, _, e) ->
    begin match note.S.note_typ with
    | T.Func (T.Local, T.Returns, [], ts1, ts2) ->
      I.PrimE (I.CastPrim (T.seq ts1, T.seq ts2), [exp e])
    | _ -> assert false
    end
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "serialize";_}, _);note;_}, _, e) ->
    begin match note.S.note_typ with
    | T.Func (T.Local, T.Returns, [], ts1, ts2) ->
      I.PrimE (I.SerializePrim ts1, [exp e])
    | _ -> assert false
    end
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "deserialize";_}, _);note;_}, _, e) ->
    begin match note.S.note_typ with
    | T.Func (T.Local, T.Returns, [], ts1, ts2) ->
      I.PrimE (I.DeserializePrim ts2, [exp e])
    | _ -> assert false
    end
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "caller";_},_);_}, _, {it=S.TupE es;_}) ->
    assert (es = []);
    I.PrimE (I.ICCallerPrim, [])
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "time";_},_);_}, _, {it=S.TupE es;_}) ->
    assert (es = []);
    I.PrimE (I.SystemTimePrim, [])
  (* Cycles *)
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "cyclesBalance";_},_);_}, _, {it=S.TupE es;_}) ->
    assert (es = []);
    I.PrimE (I.SystemCyclesBalancePrim, [])
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "cyclesAvailable";_},_);_}, _, {it=S.TupE es;_}) ->
    assert (es = []);
    I.PrimE (I.SystemCyclesAvailablePrim, [])
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "cyclesRefunded";_},_);_}, _, {it=S.TupE es;_}) ->
    assert (es = []);
    I.PrimE (I.SystemCyclesRefundedPrim, [])
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "cyclesAccept";_},_);_}, _, e) ->
    I.PrimE (I.SystemCyclesAcceptPrim, [exp e])
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "cyclesAdd";_},_);_}, _, e) ->
    I.PrimE (I.SystemCyclesAddPrim, [exp e])
  (* Certified data *)
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "setCertifiedData";_},_);_}, _, e) ->
    I.PrimE (I.SetCertifiedData, [exp e])
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "getCertificate";_},_);_}, _, {it=S.TupE es;_}) ->
    I.PrimE (I.GetCertificate, [])
  (* Other *)
  | S.CallE ({it=S.AnnotE ({it=S.PrimE p;_},_);_}, _, {it=S.TupE es;_}) ->
    I.PrimE (I.OtherPrim p, exps es)
  | S.CallE ({it=S.AnnotE ({it=S.PrimE p;_},_);_}, _, e) ->
    I.PrimE (I.OtherPrim p, [exp e])
  | S.CallE (e1, inst, e2) ->
    I.PrimE (I.CallPrim inst.note, [exp e1; exp e2])
  | S.BlockE [] -> (unitE ()).it
  | S.BlockE [{it = S.ExpD e; _}] -> (exp e).it
  | S.BlockE ds -> I.BlockE (block (T.is_unit note.Note.typ) ds)
  | S.NotE e -> (notE (exp e)).it
  | S.AndE (e1, e2) -> (andE (exp e1) (exp e2)).it
  | S.OrE (e1, e2) -> (orE (exp e1) (exp e2)).it
  | S.IfE (e1, e2, e3) -> I.IfE (exp e1, exp e2, exp e3)
  | S.SwitchE (e1, cs) -> I.SwitchE (exp e1, cases cs)
  | S.TryE (e1, cs) -> I.TryE (exp e1, cases cs)
  | S.WhileE (e1, e2) -> (whileE (exp e1) (exp e2)).it
  | S.LoopE (e1, None) -> I.LoopE (exp e1)
  | S.LoopE (e1, Some e2) -> (loopWhileE (exp e1) (exp e2)).it
  | S.ForE (p, ({it=S.CallE (({it=S.DotE (arr, proj); _} as c0), c1, c2); _} as e1), e2) when T.is_array arr.note.S.note_typ && proj.it = "vals"-> (sequentialForE  (pat p) arr proj c0 c1 c2 e1 (exp e2)).it
  | S.ForE (p, e1, e2) -> (forE (pat p) (exp e1) (exp e2)).it
  | S.DebugE e -> if !Mo_config.Flags.release_mode then (unitE ()).it else (exp e).it
  | S.LabelE (l, t, e) -> I.LabelE (l.it, t.Source.note, exp e)
  | S.BreakE (l, e) -> (breakE l.it (exp e)).it
  | S.RetE e -> (retE (exp e)).it
  | S.ThrowE e -> I.PrimE (I.ThrowPrim, [exp e])
  | S.AsyncE (tb, e) ->
    I.AsyncE (typ_bind tb, exp e,
              match note.Note.typ with
              | T.Async (t, _) -> t
              | _ -> assert false)
  | S.AwaitE e -> I.PrimE (I.AwaitPrim, [exp e])
  | S.AssertE e -> I.PrimE (I.AssertPrim, [exp e])
  | S.AnnotE (e, _) -> assert false
  | S.ImportE (f, ir) -> raise (Invalid_argument (Printf.sprintf "Import expression found in unit body: %s" f))
  | S.PrimE s -> raise (Invalid_argument ("Unapplied prim " ^ s))
  | S.IgnoreE e ->
    I.BlockE ([
      { it = I.LetD ({it = I.WildP; at = e.at; note = T.Any}, exp e);
        at = e.at; note = ()}], (unitE ()))

and url e at =
    (* Set position explicitly *)
    match e.it with
    | S.AnnotE (e,_) -> url e at
    | _ ->
      let e' = exp e in
      { it = I.(PrimE (BlobOfIcUrl, [e'])); at; note = Note.{def with typ = T.blob; eff = e'.note.eff } }

and lexp e =
    (* We short-cut AnnotE here, so that we get the position of the inner expression *)
    match e.it with
    | S.AnnotE (e,_) -> lexp e
    | _ -> { e with it = lexp' e.it; note = e.note.S.note_typ }

and lexp' = function
  | S.VarE i -> I.VarLE i.it
  | S.DotE (e, x) -> I.DotLE (exp e, x.it)
  | S.IdxE (e1, e2) -> I.IdxLE (exp e1, exp e2)
  | _ -> raise (Invalid_argument ("Unexpected expression as lvalue"))

and sequentialForE p arr proj c0 c1 c2 e1 e2 =
  let arrt = arr.note.S.note_typ in
  let arrv = fresh_var "arr" arrt in
  letE arrv (exp arr)
    (countingE p (varE arrv) (exp { e1 with note = {e1.note with note_typ = T.nat}; it = S.CallE ({c0 with note = T.{c0.note with note_typ = Func (Local, Returns, [], [], [nat])}; it = S.DotE ({arr with it=S.VarE {it=id_of_var arrv;note=();at=arr.at}}, { proj with it = "size" })}, c1, c2) })
     e2)

and mut m = match m.it with
  | S.Const -> Ir.Const
  | S.Var -> Ir.Var

and obj_block at s self_id dfs obj_typ =
  match s.it with
  | T.Object | T.Module ->
    build_obj at s.it self_id dfs obj_typ
  | T.Actor -> build_actor at self_id dfs obj_typ
  | T.Memory -> assert false

and build_field {T.lab; T.typ;_} =
  { it = { I.name = lab
         ; I.var = lab
         }
  ; at = no_region
  ; note = typ
  }

and build_fields obj_typ =
    match obj_typ with
    | T.Obj (_, fields) ->
      (* TBR: do we need to sort val_fields?*)
      let val_fields = List.filter (fun {T.lab; T.typ; _} -> not (T.is_typ typ)) fields in
      List.map build_field val_fields
    | _ -> assert false

and with_self i typ decs =
  let_no_shadow (var i typ) (selfRefE typ) decs

and call_system_func_opt name es =
  List.find_map (fun es ->
    match es.it with
    | { S.vis = { it = S.System; _ };
        S.dec = { it = S.LetD( { it = S.VarP id; _ } as p, _); _ };
        _
      } when id.it = name
      -> Some (callE (varE (var id.it p.note)) [] (tupE []))
    | _ -> None) es

and export_interface txt =
  (* This is probably a temporary hack. *)
  let open T in
  let name = "__get_candid_interface_tmp_hack" in
  let v = "$__get_candid_interface_tmp_hack"  in
  let binds = [T.scope_bind] in
  let typ = Func (Shared Query, Promises, binds, [], [text]) in

  let scope_con = Con.fresh "T" (Abs ([], T.scope_bound)) in
  let scope_con2 = Con.fresh "T2" (Abs ([], Any)) in
  let bind  = typ_arg scope_con T.Scope T.scope_bound in
  let bind2 = typ_arg scope_con2 T.Scope T.scope_bound in
  ([ letD (var v typ) (
    funcE v (Shared Query) Promises [bind] [] [text] (
      asyncE bind2 (textE txt) (T.Con (scope_con, []))
    )
  )],
  [{ it = { I.name = name; var = v }; at = no_region; note = typ }])

and build_actor at self_id es obj_typ =
  let fs = build_fields obj_typ in
  let es = List.filter (fun ef -> is_not_typD ef.it.S.dec) es in
  let ds = decs (List.map (fun ef -> ef.it.S.dec) es) in
  let stabs = List.map (fun ef -> ef.it.S.stab) es in
  let pairs = List.map2 stabilize stabs ds in
  let idss = List.map fst pairs in
  let ids = List.concat idss in
  let fields = List.map (fun (i,t) -> T.{lab = i; typ = T.Opt t; depr = None}) ids in
  let mk_ds = List.map snd pairs in
  let ty = T.Obj (T.Memory, List.sort T.compare_field fields) in
  let state = fresh_var "state" (T.Mut (T.Opt ty)) in
  let get_state = fresh_var "getState" (T.Func(T.Local, T.Returns, [], [], [ty])) in
  let ds = List.map (fun mk_d -> mk_d get_state) mk_ds in
  let ds =
    varD state (optE (primE (I.ICStableRead ty) []))
    ::
    nary_funcD get_state []
      (let v = fresh_var "v" ty in
       switch_optE (immuteE (varE state))
         (unreachableE ())
         (varP v) (varE v)
         ty)
    ::
    ds
    @
    [expD (assignE state (nullE()))]
  in
  let ds' = match self_id with
    | Some n -> with_self n.it obj_typ ds
    | None -> ds in
  let candid_interface =
    Idllib.Arrange_idl.string_of_prog (Mo_idl.Mo_to_idl.of_actor_type obj_typ) in
  let (interface_d, interface_f) = export_interface candid_interface in
  I.ActorE (interface_d @ ds', interface_f @ fs,
    { I.pre =
       (let vs = fresh_vars "v" (List.map (fun f -> f.T.typ) fields) in
        blockE
          ((match call_system_func_opt "preupgrade" es with
            | Some call -> [expD call]
            | None -> []) @
           [letP (seqP (List.map varP vs)) (* dereference any mutable vars, option 'em all *)
              (seqE (List.map (fun (i,t) -> optE (varE (var i t))) ids))])
          (primE (I.ICStableWrite ty)
             [ newObjE T.Memory
                 (List.map2 (fun f v ->
                      { it = {I.name = f.T.lab; I.var = id_of_var v};
                        at = no_region;
                        note = f.T.typ }
                    ) fields vs)
                 ty]));
        I.post = match call_system_func_opt "postupgrade" es with
                 | Some call -> call
                 | None -> tupE []},
    obj_typ)


and stabilize stab_opt d =
  let s = match stab_opt with None -> S.Flexible | Some s -> s.it  in
  match s, d.it with
  | (S.Flexible, _) ->
    ([], fun _ -> d)
  | (S.Stable, I.VarD(i, t, e)) ->
    ([(i, t)],
     fun get_state ->
     let v = fresh_var i t in
     varD (var i (T.Mut t))
       (switch_optE (dotE (callE (varE get_state) [] (unitE ())) i (T.Opt t))
         e
         (varP v) (varE v)
         t))
  | (S.Stable, I.LetD({it = I.VarP i; _} as p, e)) ->
    let t = p.note in
    ([(i, t)],
     fun get_state ->
     let v = fresh_var i t in
     letP p
       (switch_optE (dotE (callE (varE get_state) [] (unitE ())) i (T.Opt t))
         e
         (varP v) (varE v)
         t))
  | (S.Stable, I.LetD _) ->
    assert false

and build_obj at s self_id dfs obj_typ =
  let fs = build_fields obj_typ in
  let obj_e = newObjE s fs obj_typ in
  let ret_ds, ret_o =
    match self_id with
    | None -> [], obj_e
    | Some id -> let self = var id.it obj_typ in [ letD self obj_e ], varE self
  in I.BlockE (decs (List.map (fun df -> df.it.S.dec) dfs) @ ret_ds, ret_o)

and exp_field ef =
  let S.{mut; id; exp = e} = ef.it in
  let typ = e.note.S.note_typ in
  match mut.it with
  | S.Var ->
    let id' = fresh_var id.it (T.Mut typ) in
    let d = varD id' (exp e) in
    let f = { it = { I.name = id.it; I.var = id_of_var id'}; at = no_region; note = T.Mut typ } in
    (d, f)
  | S.Const ->
    let id' = fresh_var id.it typ in
    let d = letD id' (exp e) in
    let f = { it = { I.name = id.it; I.var = id_of_var id'}; at = no_region; note = typ } in
    (d, f)

and obj obj_typ efs =
  let (ds, fs) = List.map exp_field efs |> List.split in
  let obj_e = newObjE T.Object fs obj_typ in
  I.BlockE(ds, obj_e)

and typ_binds tbs = List.map typ_bind tbs

and typ_bind tb =
  let c = match tb.note with
    | Some c -> c
    | _ -> assert false
  in
  { it = { Ir.con = c; Ir.sort = tb.it.S.sort.it; Ir.bound = tb.it.S.bound.note}
  ; at = tb.at
  ; note = ()
  }

and array_dotE array_ty proj e =
  let fun_ty bs t1 t2 = T.Func (T.Local, T.Returns, bs, t1, t2) in
  let varA = T.Var ("A", 0) in
  let element_ty = T.as_immut (T.as_array array_ty) in
  let call name t1 t2 =
    let poly_array_ty =
      if T.is_mut (T.as_array array_ty)
      then T.Array (T.Mut varA)
      else T.Array varA in
    let ty_param = {T.var = "A"; sort = T.Type; T.bound = T.Any} in
    let f = var name (fun_ty [ty_param] [poly_array_ty] [fun_ty [] t1 t2]) in
    callE (varE f) [element_ty] e in
  match T.is_mut (T.as_array array_ty), proj with
    | true,  "size" -> call "@mut_array_size"   [] [T.nat]
    | false, "size" -> call "@immut_array_size" [] [T.nat]
    | true,  "get"  -> call "@mut_array_get"    [T.nat] [varA]
    | false, "get"  -> call "@immut_array_get"  [T.nat] [varA]
    | true,  "put"  -> call "@mut_array_put"    [T.nat; varA] []
    | true,  "keys" -> call "@mut_array_keys"   [] [T.iter_obj T.nat]
    | false, "keys" -> call "@immut_array_keys" [] [T.iter_obj T.nat]
    | true,  "vals" -> call "@mut_array_vals"   [] [T.iter_obj varA]
    | false, "vals" -> call "@immut_array_vals" [] [T.iter_obj varA]
    | _, _ -> assert false

and blob_dotE proj e =
  let fun_ty t1 t2 = T.Func (T.Local, T.Returns, [], t1, t2) in
  let call name t1 t2 =
    let f = var name (fun_ty [T.blob] [fun_ty t1 t2]) in
    callE (varE f) [] e in
  match proj with
    | "size"   -> call "@blob_size"   [] [T.nat]
    | "vals" -> call "@blob_vals" [] [T.iter_obj T.(Prim Nat8)]
    |  _ -> assert false

and text_dotE proj e =
  let fun_ty t1 t2 = T.Func (T.Local, T.Returns, [], t1, t2) in
  let call name t1 t2 =
    let f = var name (fun_ty [T.text] [fun_ty t1 t2]) in
    callE (varE f) [] e in
  match proj with
    | "size"   -> call "@text_size"   [] [T.nat]
    | "chars" -> call "@text_chars" [] [T.iter_obj T.char]
    |  _ -> assert false

and block force_unit ds =
  match ds with
  | [] -> ([], tupE [])
  | [{it = S.ExpD ({it = S.BlockE ds; _}); _}] -> block force_unit ds
  | _ ->
  let prefix, last = Lib.List.split_last ds in
  match force_unit, last.it with
  | _, S.ExpD e ->
    (decs prefix, exp e)
  | false, S.LetD ({it = S.VarP x; _}, e) ->
    (decs ds, varE (var x.it e.note.S.note_typ))
  | false, S.LetD (p', e') ->
    let x = fresh_var "x" (e'.note.S.note_typ) in
    (decs prefix @ [letD x (exp e'); letP (pat p') (varE x)], varE x)
  | _, _ ->
    (decs ds, tupE [])

and is_not_typD d = match d.it with | S.TypD _ -> false | _ -> true

and decs ds =
  List.map dec (List.filter is_not_typD ds)

and dec d = { (phrase' dec' d) with note = () }

and dec' at n d = match d with
  | S.ExpD e -> (expD (exp e)).it
  | S.LetD (p, e) ->
    let p' = pat p in
    let e' = exp e in
    (* HACK: remove this once backend supports recursive actors *)
    begin match p'.it, e'.it with
    | I.VarP i, I.ActorE (ds, fs, u, t) ->
      I.LetD (p', {e' with it = I.ActorE (with_self i t ds, fs, u, t)})
    | _ -> I.LetD (p', e')
    end
  | S.VarD (i, e) -> I.VarD (i.it, e.note.S.note_typ, exp e)
  | S.TypD _ -> assert false
  | S.ClassD (sp, id, tbs, p, _t_opt, s, self_id, dfs) ->
    let id' = {id with note = ()} in
    let sort, _, _, _, _ = Type.as_func n.S.note_typ in
    let op = match sp.it with
      | T.Local -> None
      | T.Shared (_, p) -> Some p in
    let inst = List.map
                 (fun tb ->
                   match tb.note with
                   | None -> assert false
                   | Some c -> T.Con (c, []))
                 tbs in
    let fun_typ = n.S.note_typ in
    let rng_typ =
      match fun_typ with
      | T.Func(_, _, bds, dom, [rng]) ->
        assert(List.length inst = List.length bds);
        T.promote (T.open_ inst rng)
      | _ -> assert false
    in
    let varPat = {it = I.VarP id'.it; at = at; note = fun_typ } in
    let args, wrap, control, _n_res = to_args n.S.note_typ op p in
    let body = if s.it = T.Actor
      then
        let (_, obj_typ) = T.as_async rng_typ in
        let c = Con.fresh T.default_scope_var (T.Abs ([], T.scope_bound)) in
        asyncE (typ_arg c T.Scope T.scope_bound)
          (wrap { it = obj_block at s (Some self_id) dfs (T.promote obj_typ);
            at = at;
            note = Note.{def with typ = obj_typ } })
          (List.hd inst)
      else
       wrap
        { it = obj_block at s (Some self_id) dfs rng_typ;
          at = at;
          note = Note.{ def with typ = rng_typ } }
    in
    let fn = {
      it = I.FuncE (id.it, sort, control, typ_binds tbs, args, [rng_typ], body);
      at = at;
      note = Note.{ def with typ = fun_typ }
    } in
    I.LetD (varPat, fn)

and cases cs = List.map case cs

and case c = phrase case' c

and case' c = S.{ I.pat = pat c.pat; I.exp = exp c.exp }

and pats ps = List.map pat ps

and pat p = phrase pat' p

and pat' = function
  | S.VarP v -> I.VarP v.it
  | S.WildP -> I.WildP
  | S.LitP l -> I.LitP (lit !l)
  | S.SignP (o, l) -> I.LitP (lit (apply_sign o (!l)))
  | S.TupP ps -> I.TupP (pats ps)
  | S.ObjP pfs ->
    I.ObjP (pat_fields pfs)
  | S.OptP p -> I.OptP (pat p)
  | S.TagP (i, p) -> I.TagP (i.it, pat p)
  | S.AltP (p1, p2) -> I.AltP (pat p1, pat p2)
  | S.AnnotP (p, _)
  | S.ParP p -> pat' p.it

and lit l = match l with
  | S.NullLit -> I.NullLit
  | S.BoolLit x -> I.BoolLit x
  | S.NatLit x -> I.NatLit x
  | S.Nat8Lit x -> I.Nat8Lit x
  | S.Nat16Lit x -> I.Nat16Lit x
  | S.Nat32Lit x -> I.Nat32Lit x
  | S.Nat64Lit x -> I.Nat64Lit x
  | S.IntLit x -> I.IntLit x
  | S.Int8Lit x -> I.Int8Lit x
  | S.Int16Lit x -> I.Int16Lit x
  | S.Int32Lit x -> I.Int32Lit x
  | S.Int64Lit x -> I.Int64Lit x
  | S.FloatLit x -> I.FloatLit x
  | S.CharLit x -> I.CharLit x
  | S.TextLit x -> I.TextLit x
  | S.BlobLit x -> I.BlobLit x
  | S.PreLit _ -> assert false

and pat_fields pfs = List.map pat_field pfs

and pat_field pf = phrase (fun S.{id; pat=p} -> I.{name=id.it; pat=pat p}) pf

and to_args typ po p : Ir.arg list * (Ir.exp -> Ir.exp) * T.control * T.typ list =

  let mergeE ds e =
    match e.it with
    | Ir.ActorE _ ->
      (match Rename.exp' Rename.Renaming.empty e.it with
       |  Ir.ActorE (ds', fs, up, ot) ->
         { e with it = Ir.ActorE (ds @ ds', fs, up, ot) }
       | _ -> assert false)
    | _ -> blockE ds e
  in

  let sort, control, n_args, res_tys =
    match typ with
    | Type.Func (sort, control, tbds, dom, res) ->
      sort, control, List.length dom, res
    | Type.Non ->
      Type.Local, Type.Returns, 1, []
    | _ -> raise (Invalid_argument ("to_args " ^ Type.string_of_typ typ))
  in

  let tys = if n_args = 1 then [p.note] else T.seq_of_tup p.note in

  let rec pat_unannot p = match p.it with
    | S.AnnotP (p, _) -> pat_unannot p
    | S.ParP p -> pat_unannot p
    | _ -> p
  in

  (* In source, the context pattern is outside the argument pattern,
  but in the IR, parameters are bound first. So if there is a context pattern,
  we _must_ create fresh names for the parameters and bind the actual parameters
  inside the wrapper. *)
  let must_wrap = po <> None in

  let to_arg p : (Ir.arg * (Ir.exp -> Ir.exp)) =
    match (pat_unannot p).it with
    | S.AnnotP _ | S.ParP _ -> assert false
    | S.VarP i when not must_wrap ->
      { i with note = p.note },
      (fun e -> e)
    | S.WildP ->
      let v = fresh_var "param" p.note in
      arg_of_var v,
      (fun e -> e)
    |  _ ->
      let v = fresh_var "param" p.note in
      arg_of_var v,
      (fun e -> mergeE [letP (pat p) (varE v)] e)
  in

  let args, wrap =
    match n_args, (pat_unannot p).it with
    | _, (S.AnnotP _ | S.ParP _) -> assert false
    | _, S.WildP ->
      let vs = fresh_vars "ignored" tys in
      List.map arg_of_var vs,
      (fun e -> e)
    | 1, _ ->
      let a, wrap = to_arg p in
      [a], wrap
    | 0, S.TupP [] ->
      [] , (fun e -> e)
    | _, S.TupP ps ->
      assert (List.length ps = n_args);
      List.fold_right (fun p (args, wrap) ->
        let (a, wrap1) = to_arg p in
        (a::args, fun e -> wrap1 (wrap e))
      ) ps ([], fun e -> e)
    | _, _ ->
      let vs = fresh_vars "param" tys in
      List.map arg_of_var vs,
      (fun e -> mergeE [letP (pat p) (tupE (List.map varE vs))] e)
  in

  let wrap_po e =
    match po with
    | None -> wrap e
    | Some p ->
      let v = fresh_var "caller" T.caller in
      mergeE
        [letD v (primE I.ICCallerPrim []);
         letP (pat p)
           (newObjE T.Object
              [{ it = {Ir.name = "caller"; var = id_of_var v};
                 at = no_region;
                 note = T.caller }]
              T.ctxt)]
        (wrap e)
  in

  let wrap_under_async e =
    if T.is_shared_sort sort
    then match control, e.it with
      | (T.Promises, Ir.AsyncE (tb, e', t)) ->
        { e with it = Ir.AsyncE (tb, wrap_po e', t) }
      | T.Returns, Ir.BlockE (
          [{ it = Ir.LetD ({ it = Ir.WildP; _} as pat, ({ it = Ir.AsyncE (tb,e',t); _} as exp)); _ }],
          ({ it = Ir.PrimE (Ir.TupPrim, []); _} as unit)) ->
        blockE [letP pat {exp with it = Ir.AsyncE (tb,wrap_po e',t)} ] unit
      | _, Ir.ActorE _ -> wrap_po e
      | _ -> assert false
    else wrap_po e in

  args, wrap_under_async, control, res_tys

type import_declaration = Ir.dec list

let actor_class_mod_exp id class_typ func =
  let fun_typ = func.note.Note.typ in
  let class_con = Con.fresh id (T.Def([], class_typ)) in
  let v = fresh_var id fun_typ in
  blockE
    [letD v func]
    (newObjE T.Module
       [{ it = {I.name = id; I.var = id_of_var v};
          at = no_region;
          note = fun_typ }]
       (T.Obj(T.Module, List.sort T.compare_field [
          { T.lab = id; T.typ = T.Typ class_con; depr = None };
          { T.lab = id; T.typ = fun_typ; depr = None }])))

let import_compiled_class (lib : S.comp_unit)  wasm : import_declaration =
  let f = lib.note.filename in
  let { body; _ } = lib.it in
  let id = match body.it with
    | S.ActorClassU (_, id, _, _, _, _, _) -> id.it
    | _ -> assert false
  in
  let fun_typ = T.normalize body.note.S.note_typ in
  let s, cntrl, tbs, ts1, ts2 = T.as_func fun_typ in
  let cs = T.open_binds tbs in
  let c, _ = T.as_con (List.hd cs) in
  let ts1' = List.map (T.open_ cs) ts1 in
  let ts2' = List.map (T.open_ cs) ts2 in
  let class_typ = match List.map T.normalize ts2' with
    | [T.Async (_ , class_typ)] -> class_typ
    | _ -> assert false
  in
  let vs = fresh_vars "param" ts1' in
  let arg_blob = fresh_var "arg_blob" T.blob in
  let principal = fresh_var "principal" T.principal in
  let t_async = T.codom cntrl (fun () -> assert false) ts2' in
  let _, t_actor = T.as_async (T.normalize t_async) in
  let wasm_blob = blobE wasm in
  let create_actor_helper = var "@create_actor_helper"
    (T.Func (T.Local, T.Returns, [T.scope_bind],
      [T.blob; T.blob],
      [T.Async(T.Var (T.default_scope_var, 0), T.principal)]))
  in
  let cs' = T.open_binds tbs in
  let c', _ = T.as_con (List.hd cs') in
  let available = fresh_var "available" T.nat64 in
  let accepted = fresh_var "accepted" T.nat64 in
  let cycles = var "@cycles" (T.Mut (T.nat64)) in
  let body =
    asyncE
      (typ_arg c' T.Scope T.scope_bound)
      (blockE [
         letD arg_blob (primE (Ir.SerializePrim ts1') [seqE (List.map varE vs)]);
         letD available (primE Ir.SystemCyclesAvailablePrim []);
         letD accepted (primE Ir.SystemCyclesAcceptPrim [varE available]);
         expD (assignE cycles (varE accepted));
         letD principal
           (awaitE (callE (varE create_actor_helper) cs'
             (tupE [wasm_blob;  varE arg_blob])))
         ]
         (primE (Ir.CastPrim (T.principal, t_actor)) [varE principal]))
      (List.hd cs)
  in
  let func = funcE id T.Local T.Returns
    [typ_arg c T.Scope T.scope_bound]
    (List.map arg_of_var vs)
    ts2'
    body
  in
  let mod_exp = actor_class_mod_exp id class_typ func in
  let mod_typ = mod_exp.note.Note.typ in
  [ letD (var (id_of_full_path f) mod_typ) mod_exp ]

let import_prelude prelude : import_declaration =
  decs prelude.it

let inject_decs extra_ds u =
  let open Ir in
  match u with
  | LibU (ds, exp) -> LibU (extra_ds @ ds, exp)
  | ProgU ds -> ProgU (extra_ds @ ds)
  | ActorU (None, ds, fs, up, t) ->
    Ir.ActorU (None, extra_ds @ ds, fs, up, t)
  | ActorU (Some _, _, _, _, _) ->
    let u'= Rename.comp_unit Rename.Renaming.empty u in
    match u' with
    | ActorU (as_opt, ds, fs, up, t) ->
      Ir.ActorU (as_opt, extra_ds @ ds, fs, up, t)
    | _ -> assert false

let link_declarations imports (cu, flavor) =
  inject_decs imports cu, flavor


let transform_import (i : S.import) : import_declaration =
  let (id, f, ir) = i.it in
  let t = i.note in
  assert (t <> T.Pre);
  let rhs = match !ir with
    | S.Unresolved -> raise (Invalid_argument ("Unresolved import " ^ f))
    | S.LibPath fp ->
      varE (var (id_of_full_path fp) t)
    | S.PrimPath ->
      varE (var (id_of_full_path "@prim") t)
    | S.IDLPath (fp, canister_id) ->
      primE (I.ActorOfIdBlob t) [blobE canister_id]
  in [ letD (var id.it t) rhs ]

let transform_unit_body (u : S.comp_unit_body) : Ir.comp_unit =
  match u.it with
  | S.ProgU ds -> I.ProgU (decs ds)
  | S.ModuleU (self_id, fields) -> (* compiling a module as a library *)
    I.LibU ([], {
      it = build_obj u.at T.Module self_id fields u.note.S.note_typ;
      at = u.at; note = typ_note u.note})
  | S.ActorClassU (sp, typ_id, _tbs, p, _, self_id, fields) ->
    let fun_typ = u.note.S.note_typ in
    let op = match sp.it with
      | T.Local -> None
      | T.Shared (_, p) -> Some p in
    let args, wrap, control, _n_res = to_args fun_typ op p in
    let obj_typ =
      match fun_typ with
      | T.Func(_s, _c, bds, _dom, [async_rng]) ->
        assert(1 = List.length bds);
        let cs  = T.open_binds bds in
        let (_, rng) = T.as_async (T.normalize (T.open_ cs async_rng)) in
        T.promote rng
      | _ -> assert false
    in
    let e = wrap {
       it = build_actor u.at (Some self_id) fields obj_typ;
       at = no_region;
       note = Note.{ def with typ = obj_typ } }
    in
    begin match e.it with
    | I.ActorE(ds, fs, u, t) -> I.ActorU (Some args, ds, fs, u, t)
    | _ -> assert false
    end
  | S.ActorU (self_id, fields) ->
    begin match build_actor u.at self_id fields u.note.S.note_typ with
    | I.ActorE (ds, fs, u, t) -> I.ActorU (None, ds, fs, u, t)
    | _ -> assert false
    end

let transform_unit (u : S.comp_unit) : Ir.prog  =
  let { imports; body; _ } = u.it in
  let imports' = List.concat_map transform_import imports in
  let body' = transform_unit_body body in
  inject_decs imports' body', Ir.full_flavor()


(* Import a unit by composing IR.

   When used for IR interpretation only, this includes
   any class that would have been separately handled by
   import_compiled_class (above) for compilation.
*)
let import_unit (u : S.comp_unit) : import_declaration =
  let { imports; body; _ } = u.it in
  let f = u.note.filename in
  let t = body.note.S.note_typ in
  assert (t <> T.Pre);
  let imports' = List.concat_map transform_import imports in
  let body' = transform_unit_body body in
  let prog = inject_decs imports' body' in
  let exp = match prog with
    | I.LibU (ds, e) -> blockE ds e
    | I.ActorU (None, ds, fs, up, t) ->
      raise (Invalid_argument "Desugar: Cannot import actor")
    | I.ActorU (Some as_, ds, fs, up, actor_t) ->
      let id = match body.it with
        | S.ActorClassU (_, id, _, _, _, _, _) -> id.it
        | _ -> assert false
      in
      let s, cntrl, tbs, ts1, ts2 = T.as_func t in
      let cs = T.open_binds [T.scope_bind] in
      let c, _ = T.as_con (List.hd cs) in
      let cs' = T.open_binds [T.scope_bind] in
      let c', _ = T.as_con (List.hd cs') in
      let body =
        asyncE
          (typ_arg c' T.Scope T.scope_bound)
          { it = I.ActorE (ds, fs, up, actor_t); at = u.at; note = Note.{ def with typ = actor_t } }
          (List.hd cs)
      in
      let class_typ = match List.map T.normalize ts2 with
        | [ T.Async(_, t2) ] -> t2
        | _ -> assert false in
      let func = funcE id T.Local T.Returns
        [typ_arg c T.Scope T.scope_bound]
        as_
        [T.Async (List.hd cs, actor_t)]
        body
      in
      actor_class_mod_exp id class_typ func
    | I.ProgU ds ->
      raise (Invalid_argument "Desugar: Cannot import program")
  in
  [ letD (var (id_of_full_path f) exp.note.Note.typ) exp ]
