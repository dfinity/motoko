open Ir_def
open As_def
open As_types
open As_values

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
  | S.LitE l -> I.LitE (lit !l)
  | S.UnE (ot, o, e) ->
    I.PrimE (I.UnPrim (!ot, o), [exp e])
  | S.BinE (ot, e1, o, e2) ->
    I.PrimE (I.BinPrim (!ot, o), [exp e1; exp e2])
  | S.RelE (ot, e1, o, e2) ->
    I.PrimE (I.RelPrim (!ot, o), [exp e1; exp e2])
  | S.ShowE (ot, e) ->
    I.PrimE (I.ShowPrim !ot, [exp e])
  | S.TupE es -> I.TupE (exps es)
  | S.ProjE (e, i) -> I.ProjE (exp e, i)
  | S.OptE e -> I.OptE (exp e)
  | S.ObjE (s, es) ->
    obj at s None es note.I.note_typ
  | S.TagE (c, e) -> I.TagE (c.it, exp e)
  | S.DotE (e, x) when T.is_array e.note.S.note_typ ->
    (array_dotE e.note.S.note_typ x.it (exp e)).it
  | S.DotE (e, x) when T.is_prim T.Text e.note.S.note_typ ->
    (text_dotE  x.it (exp e)).it
  | S.DotE (e, x) ->
    begin match T.as_obj_sub [x.it] e.note.S.note_typ with
    | T.Actor, _ -> I.ActorDotE (exp e, x.it)
    | _ -> I.DotE (exp e, x.it)
    end
  | S.AssignE (e1, e2) -> I.AssignE (exp e1, exp e2)
  | S.ArrayE (m, es) ->
    let t = T.as_array note.I.note_typ in
    I.ArrayE (mut m, T.as_immut t, exps es)
  | S.IdxE (e1, e2) -> I.IdxE (exp e1, exp e2)
  | S.FuncE (name, s, tbs, p, _t_opt, e) ->
    let cc = Call_conv.call_conv_of_typ note.I.note_typ in
    let args, wrap = to_args cc p in
    let _, _, _, ty = T.as_func_sub s.it (List.length tbs) note.I.note_typ in
    let tbs' = typ_binds tbs in
    let vars = List.map (fun (tb : I.typ_bind) -> T.Con (tb.it.I.con, [])) tbs' in
    let ty = T.open_ vars ty in
    let tys = if cc.Call_conv.n_res = 1 then [ty] else T.as_seq ty in
    I.FuncE (name, cc, tbs', args, tys, wrap (exp e))
  (* Primitive functions in the prelude have particular shapes *)
  | S.CallE ({it=S.AnnotE ({it=S.PrimE p;_}, _);note;_}, _, e)
    when Lib.String.chop_prefix "num_conv" p <> None ->
    begin match String.split_on_char '_' p with
    | [_;_;s1;s2] ->
      let p1 = Type.prim s1 in
      let p2 = Type.prim s2 in
      I.PrimE (I.NumConvPrim (p1, p2), [exp e])
    | _ -> assert false
    end
  | S.CallE ({it=S.AnnotE ({it=S.PrimE p;_},_);_}, _, {it=S.TupE es;_}) ->
    I.PrimE (I.OtherPrim p, exps es)
  | S.CallE ({it=S.AnnotE ({it=S.PrimE p;_},_);_}, _, e) ->
    I.PrimE (I.OtherPrim p, [exp e])
  | S.CallE (e1, inst, e2) ->
    let t = e1.Source.note.S.note_typ in
    if T.is_non t
    then unreachableE.it
    else
      let cc = Call_conv.call_conv_of_typ t in
      let inst = List.map (fun t -> t.Source.note) inst in
      I.CallE (cc, exp e1, inst, exp e2)
  | S.BlockE [] -> I.TupE []
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
  | S.LabelE (l, t, e) -> I.LabelE (l.it, t.Source.note, exp e)
  | S.BreakE (l, e) -> I.BreakE (l.it, exp e)
  | S.RetE e -> I.RetE (exp e)
  | S.ThrowE e -> I.ThrowE (exp e)
  | S.AsyncE e -> I.AsyncE (exp e)
  | S.AwaitE e -> I.AwaitE (exp e)
  | S.AssertE e -> I.AssertE (exp e)
  | S.AnnotE (e, _) -> assert false
  | S.ImportE (f, fp) ->
    if !fp = "" then assert false; (* unresolved import *)
    I.VarE (id_of_full_path !fp).it
  | S.PrimE s -> raise (Invalid_argument ("Unapplied prim " ^ s))

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

and build_actor at self_id es obj_typ =
  let fs = build_fields obj_typ in
  let ds = decs (List.map (fun ef -> ef.it.S.dec) es) in
  let name = match self_id with
    | Some n -> n.it
    | None -> "anon-actor-" ^ string_of_pos at.left in
  I.ActorE (name, ds, fs, obj_typ)

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
  let extra = extra_typDs ds in
  let prefix, last = Lib.List.split_last ds in
  match force_unit, last.it with
  | _, S.ExpD e ->
    (extra @ List.map dec prefix, exp e)
  | false, S.LetD ({it = S.VarP x; _}, e) ->
    (extra @ List.map dec ds, idE x.it e.note.S.note_typ)
  | false, S.LetD (p', e') ->
    let x = fresh_var "x" (e'.note.S.note_typ) in
    (extra @ List.map dec prefix @ [letD x (exp e'); letP (pat p') x], x)
  | _, _ ->
    (extra @ List.map dec ds, tupE [])

and extra_typDs ds =
  match ds with
  | [] -> []
  | d::ds ->
    match d.it with
    | S.ClassD (id, _, _, _, _, _, _) ->
      let c = Lib.Option.value id.note in
      let typD = I.TypD c @@ d.at in
      typD :: extra_typDs ds
    | _ -> extra_typDs ds

and decs ds = extra_typDs ds @ List.map dec ds

and dec d = { (phrase' dec' d) with note = () }

and dec' at n d = match d with
  | S.ExpD e -> (expD (exp e)).it
  | S.LetD (p, e) ->
    let p' = pat p in
    let e' = exp e in
    (* HACK: remove this once backend supports recursive actors *)
    begin match p'.it, e'.it with
    | I.VarP i, I.ActorE (_, ds, fs, t) ->
      I.LetD (p', {e' with it = I.ActorE (i, ds, fs, t)})
    | _ -> I.LetD (p', e')
    end
  | S.VarD (i, e) -> I.VarD (i.it, exp e)
  | S.TypD (id, typ_bind, t) ->
    let c = Lib.Option.value id.note in
    I.TypD c
  | S.ClassD (id, tbs, p, _t_opt, s, self_id, es) ->
    let id' = {id with note = ()} in
    let cc = Call_conv.call_conv_of_typ n.S.note_typ in
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
    let args, wrap = to_args cc p in
    let fn = {
      it = I.FuncE (id.it, cc, typ_binds tbs, args, [obj_typ], wrap
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


and to_args cc p : (Ir.arg list * (Ir.exp -> Ir.exp)) =
  let n = cc.Call_conv.n_args in
  let tys = if n = 1 then [p.note] else T.as_seq p.note in

  let args, wrap =
    match n, p.it with
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
      assert (List.length ps = n);
      List.fold_right (fun p (args, wrap) ->
        let (a, wrap1) = to_arg p in
        (a::args, fun e -> wrap1 (wrap e))
      ) ps ([], (fun e -> e))
    | _, _ ->
      let vs = fresh_vars "param" tys in
      List.map arg_of_exp vs,
      (fun e -> blockE [letP (pat p) (tupE vs)] e)
  in

  let wrap_under_async e =
    if cc.Call_conv.sort = T.Shared && cc.Call_conv.control = T.Promises
    then match e.it with
      | Ir.AsyncE e' -> { e with it = Ir.AsyncE (wrap e') }
      | _ -> assert false
    else wrap e in

  args, wrap_under_async

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


let declare_import imp_env (f, (prog:Syntax.prog))  =
  let open Source in
  let t = T.Env.find f imp_env in
  let typ_note =  { Syntax.empty_typ_note with Syntax.note_typ = t } in
  match prog.it with
  |  [{it = Syntax.ExpD e;_}] ->
     { it = Syntax.LetD (
                { it = Syntax.VarP (id_of_full_path f)
                ; at = no_region
                ; note = t
                }
              , e
              )
     ; at = no_region
     ; note = typ_note
     }
  (* HACK: to be removed once we restrict programs to expressions *)
  |  ds ->
     Diag.(
       print_message
         { sev = Warning
         ; at = prog.at
         ; cat = "import"
         ; text = Printf.sprintf
                    "imported declarations `...` from file %s as a module; \
                     please rewrite library %s as `module { ... }` instead." f f
         }
     );
     { it =
         Syntax.LetD
           (
             { it = Syntax.VarP (id_of_full_path f)
             ; at = no_region
             ; note = t
             }
           , { it = Syntax.ObjE
                      (T.Module @@ no_region,
                       List.map
                         (fun d ->
                           { Syntax.dec=d; vis=Syntax.Public @@ no_region }
                              @@ d.at)
                         prog.it)
             ; at = no_region
             ; note = typ_note
             }
           )
     ; at = no_region
     ; note = typ_note
     }

let combine_files imp_env libraries progs : Syntax.prog =
  (* This is a hack until the backend has explicit support for libraries *)
  let open Source in
  { it = List.map (declare_import imp_env) libraries
         @ List.concat (List.map (fun p -> p.it) progs)
  ; at = no_region
  ; note = match progs with
           | [prog] -> prog.Source.note
           | _ -> "all"
  }

let transform p = prog p

let transform_graph imp_env libraries progs =
  prog (combine_files imp_env libraries progs)

