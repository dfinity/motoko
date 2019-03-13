open Source
module S = Syntax
module I = Ir
module T = Type
open Construct

(* Combinators used in the desguaring *)

let trueE : Ir.exp = boolE true
let falseE : Ir.exp = boolE false

let apply_sign op l = Syntax.(match op, l with
  | PosOp, l -> l
  | NegOp, NatLit n -> NatLit (Value.Nat.sub Value.Nat.zero n)
  | NegOp, IntLit n -> IntLit (Value.Int.sub Value.Int.zero n)
  | _, _ -> raise (Invalid_argument "Invalid signed pattern")
  )

let phrase f x =  { x with it = f x.it }

let phrase' f x = { x with it = f x.at x.note x.it }

let rec exps es = List.map exp es

and exp e =
    (* We short-cut AnnotE here, so that we get the position of the inner expression *)
    match e.it with
    | S.AnnotE (e,_) -> exp e
    | _ -> phrase' exp' e

and exp' at note = function
  | S.PrimE p -> I.PrimE p
  | S.VarE i -> I.VarE i
  | S.LitE l -> I.LitE !l
  | S.UnE (ot, o, e) ->
    I.UnE (!ot, o, exp e)
  | S.BinE (ot, e1, o, e2) ->
    I.BinE (!ot, exp e1, o, exp e2)
  | S.RelE (ot, e1, o, e2) ->
    I.RelE (!ot, exp e1, o, exp e2)
  | S.TupE es -> I.TupE (exps es)
  | S.ProjE (e, i) -> I.ProjE (exp e, i)
  | S.OptE e -> I.OptE (exp e)
  | S.ObjE (s, es) ->
    obj at s None es note.S.note_typ
  | S.DotE (e, x) ->
    let n = {x with it = I.Name x.it} in
    begin match T.as_obj_sub x.it e.note.S.note_typ with
    | Type.Actor, _ -> I.ActorDotE (exp e, n)
    | _ -> I.DotE (exp e, n)
    end
  | S.AssignE (e1, e2) -> I.AssignE (exp e1, exp e2)
  | S.ArrayE (m, es) ->
    let t = Type.as_array note.S.note_typ in
    I.ArrayE (m, Type.as_immut t, exps es)
  | S.IdxE (e1, e2) -> I.IdxE (exp e1, exp e2)
  | S.FuncE (name, s, tbs, p, ty, e) ->
    let cc = Value.call_conv_of_typ note.S.note_typ in
    let args, wrap = to_args cc p in
    I.FuncE (name, cc, typ_binds tbs, args, ty.note, wrap (exp e))
  | S.CallE (e1, inst, e2) ->
    let cc = Value.call_conv_of_typ e1.Source.note.S.note_typ in
    let inst = List.map (fun t -> t.Source.note) inst in
    I.CallE (cc, exp e1, inst, exp e2)
  | S.BlockE [] -> I.TupE []
  | S.BlockE [{it = S.ExpD e; _}] -> (exp e).it
  | S.BlockE ds -> I.BlockE (block (Type.is_unit note.S.note_typ) ds)
  | S.NotE e -> I.IfE (exp e, falseE, trueE)
  | S.AndE (e1, e2) -> I.IfE (exp e1, exp e2, falseE)
  | S.OrE (e1, e2) -> I.IfE (exp e1, trueE, exp e2)
  | S.IfE (e1, e2, e3) -> I.IfE (exp e1, exp e2, exp e3)
  | S.SwitchE (e1, cs) -> I.SwitchE (exp e1, cases cs)
  | S.WhileE (e1, e2) -> (whileE (exp e1) (exp e2)).it
  | S.LoopE (e1, None) -> I.LoopE (exp e1)
  | S.LoopE (e1, Some e2) -> (loopWhileE (exp e1) (exp e2)).it
  | S.ForE (p, e1, e2) -> (forE (pat p) (exp e1) (exp e2)).it
  | S.LabelE (l, t, e) -> I.LabelE (l, t.Source.note, exp e)
  | S.BreakE (l, e) -> I.BreakE (l, exp e)
  | S.RetE e -> I.RetE (exp e)
  | S.AsyncE e -> I.AsyncE (exp e)
  | S.AwaitE e -> I.AwaitE (exp e)
  | S.AssertE e -> I.AssertE (exp e)
  | S.AnnotE (e, _) -> assert false

and obj at s self_id es obj_typ =
  match s.it with
  | Type.Object _ -> build_obj at s self_id es obj_typ
  | Type.Actor -> build_actor at self_id es obj_typ

and build_fields obj_typ =
    match obj_typ with
    | Type.Obj (_, fields) ->
      List.map (fun {Type.lab; Type.typ} ->
        { it = { I.name = I.Name lab @@ no_region
               ; I.var = lab @@ no_region
               }
        ; at = no_region
        ; note = typ
        }) fields
    | _ -> assert false

and build_actor at self_id es obj_typ =
  let fs = build_fields obj_typ in
  let ds = decs (List.map (fun ef -> ef.it.S.dec) es) in
  let name = match self_id with
    | Some n -> n
    | None -> ("anon-actor-" ^ string_of_pos at.left) @@ at in
  I.ActorE (name, ds, fs, obj_typ)

and build_obj at s self_id es obj_typ =
  let fs = build_fields obj_typ in
  let obj_e = newObjE s.it fs obj_typ in
  let ret_ds, ret_o =
    match self_id with
    | None -> [], obj_e
    | Some id -> let self = idE id obj_typ in [ letD self obj_e ], self
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

and block force_unit ds =
  let extra = extra_typDs ds in
  let prefix, last = Lib.List.split_last ds in
  match force_unit, last.it with
  | _, S.ExpD e ->
    (extra @ List.map dec prefix, exp e)
  | false, S.LetD ({it = S.VarP x; _}, e) ->
    (extra @ List.map dec ds, idE x e.note.S.note_typ)
  | false, S.LetD (p', e') ->
    let x = fresh_var (e'.note.S.note_typ) in
    (extra @ List.map dec prefix @ [letD x (exp e'); letP (pat p') x], x)
  | _, _ ->
    (extra @ List.map dec ds, tupE [])

and extra_typDs ds =
  match ds with
  | [] -> []
  | d::ds ->
    match d.it with
    | S.ClassD (id, _, _, _, _, _) ->
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
  | S.VarD (i, e) -> I.VarD (i, exp e)
  | S.TypD (id, typ_bind, t) ->
    let c = Lib.Option.value id.note in
    I.TypD c
  | S.ClassD (id, tbs, s, p, self_id, es) ->
    let id' = {id with note = ()} in
    let cc = Value.call_conv_of_typ n.S.note_typ in
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
    let varPat = {it = I.VarP id'; at = at; note = fun_typ } in
    let args, wrap = to_args cc p in
    let fn = {
      it = I.FuncE (id.it, cc, typ_binds tbs, args, obj_typ, wrap
         { it = obj at s (Some self_id) es obj_typ;
           at = at;
           note = { S.note_typ = obj_typ; S.note_eff = T.Triv } });
      at = at;
      note = { S.note_typ = fun_typ; S.note_eff = T.Triv }
    } in
    I.LetD (varPat, fn)

and cases cs = List.map case cs

and case c = phrase case' c

and case' c = S.{ I.pat = pat c.pat; I.exp = exp c.exp }

and pats ps = List.map pat ps

and pat p = phrase pat' p

and pat' = function
  | S.VarP v -> I.VarP v
  | S.WildP -> I.WildP
  | S.LitP l -> I.LitP !l
  | S.SignP (o, l) -> I.LitP (apply_sign o !l)
  | S.TupP ps -> I.TupP (pats ps)
  | S.OptP p -> I.OptP (pat p)
  | S.AltP (p1, p2) -> I.AltP (pat p1, pat p2)
  | S.AnnotP (p, _)
  | S.ParP p -> pat' p.it

and to_arg p : (Ir.arg * (Ir.exp -> Ir.exp)) =
  match p.it with
  | S.VarP i ->
    { i with note = p.note },
    (fun e -> e)
  | S.WildP ->
    let v = fresh_var p.note in
    arg_of_exp v,
    (fun e -> e)
  |  _ ->
    let v = fresh_var p.note in
    arg_of_exp v,
    (fun e -> blockE [letP (pat p) v] e)


and to_args cc p0 : (Ir.arg list * (Ir.exp -> Ir.exp)) =
  let p = match p0.it, p0.note with
    | S.ParP p1, _ -> p1
    | S.TupP [p1], Type.Tup [n] -> { p0 with it = p1.it; note = n }
    | _ -> p0 in

  let n = cc.Value.n_args in
  let tys = if n = 1 then [p.note] else T.as_seq p.note in

  let args, wrap =
    match n, p.it with
    | _, S.WildP ->
      let vs = List.map fresh_var tys in
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
      let vs = List.map fresh_var tys in
      List.map arg_of_exp vs,
      (fun e -> blockE [letP (pat p) (tupE vs)] e)
  in

  let wrap_under_async e =
    if cc.Value.sort = T.Sharable && cc.Value.control = T.Promises
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
    }

(* validation *)

let transform scope p = prog p

