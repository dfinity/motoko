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

let id_of_full_path (fp : string) : Syntax.id =
  let open Source in
  ("file$" ^ fp) @@ no_region

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

let typ_note : S.typ_note -> I.typ_note =
  fun {S.note_typ;S.note_eff} -> {I.note_typ;I.note_eff}

let phrase' f x =
  { x with it = f x.at x.note x.it }

let typed_phrase' f x =
  let n' = typ_note x.note in
  { x with it = f x.at n' x.it; note = n' }

let rec exps es = List.map exp es

and exp e =
    (* We short-cut AnnotE here, so that we get the position of the inner expression *)
    match e.it with
    | S.AnnotE (e,_) -> exp e
    | _ -> typed_phrase' exp' e

and exp' at note = function
  | S.VarE i -> I.VarE i.it
  | S.ActorUrlE e ->
    I.(PrimE (ActorOfIdBlob note.note_typ, [url e]))
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
    obj at s None es note.I.note_typ
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
    let t = T.as_array note.I.note_typ in
    I.PrimE (I.ArrayPrim (mut m, T.as_immut t), exps es)
  | S.IdxE (e1, e2) -> I.PrimE (I.IdxPrim, [exp e1; exp e2])
  | S.FuncE (name, sp, tbs, p, _t_opt, e) ->
    let s, po = match sp.it with
      | T.Local -> (T.Local, None)
      | T.Shared (ss, {it = S.WildP; _} ) -> (* don't bother with ctxt pat *)
        (T.Shared ss, None)
      | T.Shared (ss, sp) -> (T.Shared ss, Some sp) in
    let args, wrap, control, res_tys = to_args note.I.note_typ po p in
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
    let inst = List.map (fun t -> t.Source.note) inst in
    I.PrimE (I.CallPrim inst, [exp e1; exp e2])
  | S.BlockE [] -> unitE.it
  | S.BlockE [{it = S.ExpD e; _}] -> (exp e).it
  | S.BlockE ds -> I.BlockE (block (T.is_unit note.I.note_typ) ds)
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
  | S.AsyncE e -> I.AsyncE (exp e)
  | S.AwaitE e -> I.PrimE (I.AwaitPrim, [exp e])
  | S.AssertE e -> I.PrimE (I.AssertPrim, [exp e])
  | S.AnnotE (e, _) -> assert false
  | S.ImportE (f, ir) ->
    begin match !ir with
    | S.Unresolved -> raise (Invalid_argument ("Unresolved import " ^ f))
    | S.LibPath fp -> I.VarE (id_of_full_path fp).it
    | S.PrimPath -> I.VarE (id_of_full_path "@prim").it
    | S.IDLPath (fp, blob_id) -> I.(PrimE (ActorOfIdBlob note.note_typ, [blobE blob_id]))
    end
  | S.PrimE s -> raise (Invalid_argument ("Unapplied prim " ^ s))

and url e =
    (* We short-cut AnnotE here, so that we get the position of the inner expression *)
    match e.it with
    | S.AnnotE (e,_) -> url e
    | _ ->
      let transformed = typed_phrase' (url' e) e in
      I.{ transformed with note = { transformed.note with note_typ = T.blob } }

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
  match s.it with
  | T.Object | T.Module -> build_obj at s self_id es obj_typ
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
  let_no_shadow (idE i typ) (selfRefE typ) decs

and build_actor at self_id es obj_typ =
  let fs = build_fields obj_typ in
  let ds = decs (List.map (fun ef -> ef.it.S.dec) es) in
  let ds' = match self_id with
    | Some n -> with_self n.it obj_typ ds
    | None -> ds in
  I.ActorE (ds', fs, obj_typ)

and build_obj at s self_id es obj_typ =
  let fs = build_fields obj_typ in
  let obj_e = newObjE s.it fs obj_typ in
  let ret_ds, ret_o =
    match self_id with
    | None -> [], obj_e
    | Some id -> let self = idE id.it obj_typ in [ letD self obj_e ], self
  in I.BlockE (decs (List.map (fun ef -> ef.it.S.dec) es) @ ret_ds, ret_o)

and typ_binds tbs = List.map typ_bind tbs

and typ_bind tb =
  let c = match tb.note with
    | Some c -> c
    | _ -> assert false
  in
  { it = { Ir.con = c; Ir.bound = tb.it.S.bound.note}
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
    let ty_param = {T.var = "A"; T.bound = T.Any} in
    let f = idE name (fun_ty [ty_param] [poly_array_ty] [fun_ty [] t1 t2]) in
    callE f [element_ty] e in
  match T.is_mut (T.as_array array_ty), proj with
    | true,  "len"  -> call "@mut_array_len"    [] [T.nat]
    | false, "len"  -> call "@immut_array_len"  [] [T.nat]
    | true,  "get"  -> call "@mut_array_get"    [T.nat] [varA]
    | false, "get"  -> call "@immut_array_get"  [T.nat] [varA]
    | true,  "set"  -> call "@mut_array_set"    [T.nat; varA] []
    | true,  "keys" -> call "@mut_array_keys"   [] [T.iter_obj T.nat]
    | false, "keys" -> call "@immut_array_keys" [] [T.iter_obj T.nat]
    | true,  "vals" -> call "@mut_array_vals"   [] [T.iter_obj varA]
    | false, "vals" -> call "@immut_array_vals" [] [T.iter_obj varA]
    | _, _ -> assert false

and blob_dotE proj e =
  let fun_ty t1 t2 = T.Func (T.Local, T.Returns, [], t1, t2) in
  let call name t1 t2 =
    let f = idE name (fun_ty [T.blob] [fun_ty t1 t2]) in
    callE f [] e in
  match proj with
    | "size"   -> call "@blob_size"   [] [T.nat]
    | "bytes" -> call "@blob_bytes" [] [T.iter_obj T.(Prim Word8)]
    |  _ -> assert false

and text_dotE proj e =
  let fun_ty t1 t2 = T.Func (T.Local, T.Returns, [], t1, t2) in
  let call name t1 t2 =
    let f = idE name (fun_ty [T.text] [fun_ty t1 t2]) in
    callE f [] e in
  match proj with
    | "len"   -> call "@text_len"   [] [T.nat]
    | "chars" -> call "@text_chars" [] [T.iter_obj T.char]
    |  _ -> assert false

and block force_unit ds =
  let prefix, last = Lib.List.split_last ds in
  match force_unit, last.it with
  | _, S.ExpD e ->
    (decs prefix, exp e)
  | false, S.LetD ({it = S.VarP x; _}, e) ->
    (decs ds, idE x.it e.note.S.note_typ)
  | false, S.LetD (p', e') ->
    let x = fresh_var "x" (e'.note.S.note_typ) in
    (decs prefix @ [letD x (exp e'); letP (pat p') x], x)
  | _ , S.IgnoreD _ (* redundant, but explicit *)
  | _, _ ->
    (decs ds, tupE [])

and decs ds =
  let is_not_typD d = match d.it with | S.TypD _ -> false | _ -> true in
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
    | I.VarP i, I.ActorE (ds, fs, t) ->
      I.LetD (p', {e' with it = I.ActorE (with_self i t ds, fs, t)})
    | _ -> I.LetD (p', e')
    end
  | S.VarD (i, e) -> I.VarD (i.it, exp e)
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
         { it = obj at s (Some self_id) es obj_typ;
           at = at;
           note = { I.note_typ = obj_typ; I.note_eff = T.Triv } });
      at = at;
      note = { I.note_typ = fun_typ; I.note_eff = T.Triv }
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

and to_arg p : (Ir.arg * (Ir.exp -> Ir.exp)) =
  match p.it with
  | S.AnnotP (p, _) -> to_arg p
  | S.VarP i ->
    { i with note = p.note },
    (fun e -> e)
  | S.WildP ->
    let v = fresh_var "param" p.note in
    arg_of_exp v,
    (fun e -> e)
  |  _ ->
    let v = fresh_var "param" p.note in
    arg_of_exp v,
    (fun e -> blockE [letP (pat p) v] e)

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

  let args, wrap =
    match n_args, p.it with
    | _, S.WildP ->
      let vs = fresh_vars "param" tys in
      List.map arg_of_exp vs,
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
      List.map arg_of_exp vs,
      (fun e -> blockE [letP (pat p) (tupE vs)] e)
  in

  let wrap_po e =
    match po with
    | None -> wrap e
    | Some p ->
      let v = fresh_var "caller" T.caller in
      let c = fresh_var "ctxt" T.ctxt in
      blockE
        [letD v (primE I.ICCallerPrim []);
         letD c
           (newObjE T.Object
              [{ it = {Ir.name = "caller"; var = id_of_exp v};
                 at = no_region;
                 note = T.caller }]
              T.ctxt);
         letP (pat p) c]
        (wrap e)
  in

  let wrap_under_async e =
    if T.is_shared_sort sort && control <> T.Returns
    then match e.it with
      | Ir.AsyncE e' -> { e with it = Ir.AsyncE (wrap_po e') }
      | _ -> assert false
    else wrap_po e in

  args, wrap_under_async, control, res_tys

and prog (p : Syntax.prog) : Ir.prog =
  begin match p.it with
    | [] -> ([], tupE [])
    | _ -> block false p.it
  end
  , { I.has_await = true
    ; I.has_async_typ = true
    ; I.has_show = true
    ; I.serialized = false
    }


let declare_import lib =
  let open Source in
  let f = lib.note in
  let t = lib.it.note.Syntax.note_typ in
  let typ_note =  { Syntax.empty_typ_note with Syntax.note_typ = t } in
  let p = { it = Syntax.VarP (id_of_full_path f); at = lib.at; note = t } in
  { it = Syntax.LetD (p, lib.it); at = lib.at; note = typ_note }

let combine_files libs progs : Syntax.prog =
  (* This is a hack until the backend has explicit support for libraries *)
  let open Source in
  { it = List.map declare_import libs
         @ List.concat (List.map (fun p -> p.it) progs)
  ; at = no_region
  ; note = match progs with
           | [prog] -> prog.Source.note
           | _ -> "all"
  }

let transform p = prog p

let transform_graph libraries progs =
  prog (combine_files libraries progs)

