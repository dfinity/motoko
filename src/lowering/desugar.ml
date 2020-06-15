open Ir_def
open Mo_def
open Mo_types
open Mo_values

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

let trueE : Ir.exp = boolE true
let falseE : Ir.exp = boolE false

let apply_sign op l = Syntax.(match op, l with
  | PosOp, l -> l
  | NegOp, (NatLit n | IntLit n) -> IntLit (Value.Int.sub Value.Int.zero n)
  | NegOp, Int8Lit n -> Int8Lit (Value.Int_8.sub Value.Int_8.zero n)
  | NegOp, Int16Lit n -> Int16Lit (Value.Int_16.sub Value.Int_16.zero n)
  | NegOp, Int32Lit n -> Int32Lit (Value.Int_32.sub Value.Int_32.zero n)
  | NegOp, Int64Lit n -> Int64Lit (Value.Int_64.sub Value.Int_64.zero n)
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
    I.(PrimE (ActorOfIdBlob note.Note.typ, [url e]))
  | S.LitE l -> I.LitE (lit !l)
  | S.UnE (ot, o, e) ->
    I.PrimE (I.UnPrim (!ot, o), [exp e])
  | S.BinE (ot, e1, o, e2) ->
    I.PrimE (I.BinPrim (!ot, o), [exp e1; exp e2])
  | S.RelE (ot, e1, o, e2) ->
    I.PrimE (I.RelPrim (!ot, o), [exp e1; exp e2])
  | S.ShowE (ot, e) ->
    I.PrimE (I.ShowPrim !ot, [exp e])
  | S.TupE es -> (tupE (exps es)).it
  | S.ProjE (e, i) -> (projE (exp e) i).it
  | S.OptE e -> (optE (exp e)).it
  | S.ObjE (s, es) ->
    obj at s.it None es note.Note.typ
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
    | ["num";"conv";s1;s2] ->
      let p1 = Type.prim s1 in
      let p2 = Type.prim s2 in
      I.PrimE (I.NumConvPrim (p1, p2), [exp e])
    | _ -> assert false
    end
  | S.CallE ({it=S.AnnotE ({it=S.PrimE "cast";_}, _);note;_}, _, e) ->
    begin match note.S.note_typ with
    | T.Func (T.Local, T.Returns, [], ts1, ts2) ->
      I.PrimE (I.CastPrim (T.seq ts1, T.seq ts2), [exp e])
    | _ -> assert false
    end
  | S.CallE ({it=S.AnnotE ({it=S.PrimE p;_},_);_}, _, {it=S.TupE es;_}) ->
    I.PrimE (I.OtherPrim p, exps es)
  | S.CallE ({it=S.AnnotE ({it=S.PrimE p;_},_);_}, _, e) ->
    I.PrimE (I.OtherPrim p, [exp e])
  | S.CallE (e1, inst, e2) ->
    I.PrimE (I.CallPrim inst.note, [exp e1; exp e2])
  | S.BlockE [] -> unitE.it
  | S.BlockE [{it = S.ExpD e; _}] -> (exp e).it
  | S.BlockE ds -> I.BlockE (block (T.is_unit note.Note.typ) ds)
  | S.NotE e -> I.IfE (exp e, falseE, trueE)
  | S.AndE (e1, e2) -> I.IfE (exp e1, exp e2, falseE)
  | S.OrE (e1, e2) -> I.IfE (exp e1, trueE, exp e2)
  | S.IfE (e1, e2, e3) -> I.IfE (exp e1, exp e2, exp e3)
  | S.SwitchE (e1, cs) -> I.SwitchE (exp e1, cases cs)
  | S.TryE (e1, cs) -> I.TryE (exp e1, cases cs)
  | S.WhileE (e1, e2) -> (whileE (exp e1) (exp e2)).it
  | S.LoopE (e1, None) -> I.LoopE (exp e1)
  | S.LoopE (e1, Some e2) -> (loopWhileE (exp e1) (exp e2)).it
  | S.ForE (p, e1, e2) -> (forE (pat p) (exp e1) (exp e2)).it
  | S.DebugE e -> if !Mo_config.Flags.release_mode then unitE.it else (exp e).it
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

and url e =
    (* We short-cut AnnotE here, so that we get the position of the inner expression *)
    match e.it with
    | S.AnnotE (e,_) -> url e
    | _ ->
      let transformed = typed_phrase' (url' e) e in
      { transformed with note = Note.{ transformed.note with typ = T.blob } }

and url' e at _ _ = I.(PrimE (BlobOfIcUrl, [exp e]))

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

and mut m = match m.it with
  | S.Const -> Ir.Const
  | S.Var -> Ir.Var

and obj at s self_id es obj_typ =
  match s with
  | T.Object | T.Module | T.Memory -> build_obj at s self_id es obj_typ
  | T.Actor -> build_actor at self_id es obj_typ

and build_field {T.lab; T.typ} =
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
      let val_fields = List.filter (fun {T.lab;T.typ} -> not (T.is_typ typ)) fields in
      List.map build_field val_fields
    | _ -> assert false

and with_self i typ decs =
  let_no_shadow (var i typ) (selfRefE typ) decs

and call_system_func_opt name es =
  Lib.List.first_opt (fun es ->
    match es.it with
    | { S.vis = { it = S.System; _ };
        S.dec = { it = S.LetD( { it = S.VarP id; _ } as p, _); _ };
        _
      } when id.it = name
      -> Some (callE (varE (var id.it p.note)) [] (tupE []))
    | _ -> None) es

and build_actor at self_id es obj_typ =
  let fs = build_fields obj_typ in
  let es = List.filter (fun ef -> is_not_typD ef.it.S.dec) es in
  let ds = decs (List.map (fun ef -> ef.it.S.dec) es) in
  let stabs = List.map (fun ef -> ef.it.S.stab) es in
  let pairs = List.map2 stabilize stabs ds in
  let idss = List.map fst pairs in
  let ids = List.concat idss in
  let fields = List.map (fun (i,t) -> T.{lab = i; typ = T.Opt t}) ids in
  let mk_ds = List.map snd pairs in
  let ty = T.Obj (T.Memory, List.sort T.compare_field fields) in
  let state = fresh_var "state" (T.Mut (T.Opt ty)) in
  let get_state = fresh_var "getState" (T.Func(T.Local, T.Returns, [], [], [ty])) in
  let ds = List.map (fun mk_d -> mk_d get_state) mk_ds in
  let ds =
    varD (id_of_var state) (T.Opt ty) (optE (primE (I.ICStableRead ty) []))
    ::
    nary_funcD get_state []
      (let v = fresh_var "v" ty in
       switch_optE (immuteE (varE state))
         (unreachableE)
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
  I.ActorE (ds', fs,
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
     varD i t
       (switch_optE (dotE (callE (varE get_state) [] unitE) i (T.Opt t))
         e
         (varP v) (varE v)
         t))
  | (S.Stable, I.LetD({it = I.VarP i; _} as p, e)) ->
    let t = p.note in
    ([(i, t)],
     fun get_state ->
     let v = fresh_var i t in
     letP p
       (switch_optE (dotE (callE (varE get_state) [] unitE) i (T.Opt t))
         e
         (varP v) (varE v)
         t))
  | (S.Stable, I.LetD _) ->
    assert false

and build_obj at s self_id es obj_typ =
  let fs = build_fields obj_typ in
  let obj_e = newObjE s fs obj_typ in
  let ret_ds, ret_o =
    match self_id with
    | None -> [], obj_e
    | Some id -> let self = var id.it obj_typ in [ letD self obj_e ], varE self
  in I.BlockE (decs (List.map (fun ef -> ef.it.S.dec) es) @ ret_ds, ret_o)

and typ_binds tbs = List.map typ_bind tbs

and typ_bind tb =
  let c = match tb.note with
    | Some c -> c
    | _ -> assert false
  in
  { it = { Ir.con = c; Ir.sort = T.Type; Ir.bound = tb.it.S.bound.note}
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
    | true,  "size"  -> call "@mut_array_size"    [] [T.nat]
    | false, "size"  -> call "@immut_array_size"  [] [T.nat]
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
    | "bytes" -> call "@blob_bytes" [] [T.iter_obj T.(Prim Word8)]
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
  | false, S.ClassD (id, tbs, p, _t_opt, s, self_id, es)
    when Lib.String.chop_prefix "anon" id.it != None ->
    let e = match (dec last).it with I.LetD (_, e) -> e | _ -> assert false in
    (decs prefix, e)
  | false, S.LetD ({it = S.VarP x; _}, e) ->
    (decs ds, varE (var x.it e.note.S.note_typ))
  | false, S.LetD (p', e') ->
    let x = fresh_var "x" (e'.note.S.note_typ) in
    (decs prefix @ [letD x (exp e'); letP (pat p') (varE x)], varE x)
  | _ , S.IgnoreD _ (* redundant, but explicit *)
  | _, _ ->
    (decs ds, tupE [])

and is_not_typD d = match d.it with | S.TypD _ -> false | _ -> true

and decs ds =
  List.map dec (List.filter is_not_typD ds)

and dec d = { (phrase' dec' d) with note = () }

and dec' at n d = match d with
  | S.ExpD e -> (expD (exp e)).it
  | S.IgnoreD e -> I.LetD ({ it = I.WildP; at = e.at; note = T.Any}, exp e)
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
  | S.ClassD (id, tbs, p, _t_opt, s, self_id, es) ->
    let id' = {id with note = ()} in
    let sort, _, _, _, _ = Type.as_func n.S.note_typ in
    let inst = List.map
                 (fun tb ->
                   match tb.note with
                   | None -> assert false
                   | Some c -> T.Con (c, []))
                 tbs in
    let fun_typ = n.S.note_typ in
    let obj_typ =
      match fun_typ with
      | T.Func(s,c,bds,dom,[rng]) ->
        assert(List.length inst = List.length bds);
        T.promote (T.open_ inst rng)
      | _ -> assert false
    in
    let varPat = {it = I.VarP id'.it; at = at; note = fun_typ } in
    let args, wrap, control, _n_res = to_args n.S.note_typ None p in
    let fn = {
      it = I.FuncE (id.it, sort, control, typ_binds tbs, args, [obj_typ], wrap
         { it = obj at s.it (Some self_id) es obj_typ;
           at = at;
           note = Note.{ def with typ = obj_typ } });
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
  | S.Word8Lit x -> I.Word8Lit x
  | S.Word16Lit x -> I.Word16Lit x
  | S.Word32Lit x -> I.Word32Lit x
  | S.Word64Lit x -> I.Word64Lit x
  | S.FloatLit x -> I.FloatLit x
  | S.CharLit x -> I.CharLit x
  | S.TextLit x -> I.TextLit x
  | S.PreLit _ -> assert false

and pat_fields pfs = List.map pat_field pfs

and pat_field pf = phrase (fun S.{id; pat=p} -> I.{name=id.it; pat=pat p}) pf

and to_args typ po p : Ir.arg list * (Ir.exp -> Ir.exp) * T.control * T.typ list =
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
  but in the IR, paramteres are bound first. So if there is a context pattern,
  we _must_ create fresh names for the parameters and bind the actual paramters
  inside the wrapper. *)
  let must_wrap = po != None in

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
      (fun e -> blockE [letP (pat p) (varE v)] e)
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
      (fun e -> blockE [letP (pat p) (tupE (List.map varE vs))] e)
  in

  let wrap_po e =
    match po with
    | None -> wrap e
    | Some p ->
      let v = fresh_var "caller" T.caller in
      blockE
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
      | T.Promises, Ir.AsyncE (tb, e', t) -> { e with it = Ir.AsyncE (tb, wrap_po e', t) }
      | T.Returns, Ir.BlockE (
          [{ it = Ir.LetD ({ it = Ir.WildP; _} as pat, ({ it = Ir.AsyncE (tb,e',t); _} as exp)); _ }],
          ({ it = Ir.PrimE (Ir.TupPrim, []); _} as unit)) ->
        blockE [letP pat {exp with it = Ir.AsyncE (tb,wrap_po e',t)} ] unit
      | _ -> assert false
    else wrap_po e in

  args, wrap_under_async, control, res_tys

type import_declaration = Ir.dec list
let import_class f wasm : import_declaration =
  let t = T.blob in
  [ letD (var (id_of_full_path f) t) (blobE wasm) ]

let import_prelude prelude : import_declaration =
  decs (prelude.it)

let inject_decs extra_ds =
  let open Ir in
  function
  | LibU (ds, exp) -> LibU (extra_ds @ ds, exp)
  | ProgU ds -> ProgU (extra_ds @ ds)
  | ActorU (ds, fs, up, t) -> Ir.ActorU (extra_ds @ ds, fs, up, t)

let link_declarations imports (cu, flavor) =
  inject_decs imports cu, flavor

let initial_flavor : Ir.flavor =
  { I.has_await = true
  ; I.has_async_typ = true
  ; I.has_show = true
  ; I.serialized = false
  }

let transform_import (i : S.import) : import_declaration =
  let (id, f, ir) = i.it in
  let t = i.note in
  let rhs = match !ir with
    | S.Unresolved -> raise (Invalid_argument ("Unresolved import " ^ f))
    | S.LibPath fp ->
      varE (var (id_of_full_path fp) t)
    | S.ClassPath fp ->
      let _, c, _, _, ts = T.as_func t in
      let t = T.codom c (fun () -> assert false) ts in
      [] -->* primE (I.ActorOfIdBlob t) [
        varE (var (id_of_full_path fp) T.blob)
      ]
    | S.PrimPath ->
      varE (var (id_of_full_path "@prim") t)
    | S.IDLPath (fp, blob_id) ->
      primE (I.ActorOfIdBlob t) [blobE blob_id]
  in [ letD (var id.it t) rhs ]

let transform_unit_body (u : S.comp_unit_body) : Ir.comp_unit =
  match u.it with
  | S.ProgU ds -> I.ProgU (decs ds)
  | S.ModuleU fields -> (* compiling a module as program *)
    I.LibU ([], {
      it = build_obj u.at T.Module None fields u.note.S.note_typ;
      at = u.at; note = typ_note u.note})
  | S.ActorClassU (_typ_id, p, _, self_id, fields) ->
    begin match p.it with
    | S.TupP [] -> ()
    | S.ParP ({ it = S.TupP [];_}) -> ()
    | _ ->
      raise (Invalid_argument (Printf.sprintf
        "transform_unit_body: TODO Actor class params: %s"
        (Wasm.Sexpr.to_string 80 (Arrange.pat p))
      ))
    end;

    let fun_typ = u.note.S.note_typ in
    let obj_typ =
      match fun_typ with
      | T.Func(s,c,bds,dom,[rng]) ->
        assert(0 = List.length bds);
        T.promote rng
      | _ -> assert false
    in
    begin match build_actor u.at self_id fields obj_typ with
    | I.ActorE (ds, fs, u, t) -> I.ActorU (ds, fs, u, t)
    | _ -> assert false
    end
  | S.ActorU (self_id, fields) ->
    begin match build_actor u.at self_id fields u.note.S.note_typ with
    | I.ActorE (ds, fs, u, t) -> I.ActorU (ds, fs, u, t)
    | _ -> assert false
    end

let transform_unit (u : S.comp_unit) : Ir.prog  =
  let (imports, body) = u.it in
  let imports' = Lib.List.concat_map transform_import imports in
  let body' = transform_unit_body body in
  inject_decs imports' body', initial_flavor


(* Import a unit by substitution *)
let import_unit (u : S.comp_unit) : import_declaration =
  let (imports, body) = u.it in
  let f = u.note in
  let t = body.note.S.note_typ in
  let imports' = Lib.List.concat_map transform_import imports in
  let body' = transform_unit_body body in
  let prog = inject_decs imports' body' in
  let exp = match prog with
    | I.LibU (ds, e) -> blockE ds e
    | I.ActorU (ds, fs, up, t) ->
      { it = I.ActorE (ds, fs, up, t); at = u.at; note = Note.{ def with typ = t } }
    | I.ProgU ds -> raise (Invalid_argument "Desugar: Cannot import program")
  in
  [ letD (var (id_of_full_path f) t) exp ]

