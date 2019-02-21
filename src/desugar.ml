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
  | S.ObjE (s, i, es) ->
    let public_es = List.filter (fun e -> e.it.S.priv.it == Syntax.Public) es in
    let obj_typ =
      T.Obj(s.it,
            List.sort T.compare_field
              (List.map (fun {it = { Syntax.name; exp; mut; priv; _}; _} ->
                   let t = exp.note.S.note_typ in
                   let t = if mut.it = Syntax.Var then Type.Mut t else t in
                   { Type.name = S.string_of_name name.it;
                     Type.typ = t }) public_es))
    in
    obj at s None i es obj_typ
  | S.DotE (e, sr, n) ->
    begin match (!sr) with
    | Type.Actor -> I.ActorDotE (exp e, n)
    | _ -> I.DotE (exp e, n)
    end
  | S.AssignE (e1, e2) -> I.AssignE (exp e1, exp e2)
  | S.ArrayE (m, es) ->
    let t = Type.as_array note.S.note_typ in
    I.ArrayE (m, Type.as_immut t, exps es)
  | S.IdxE (e1, e2) -> I.IdxE (exp e1, exp e2)
  | S.CallE (e1, inst, e2) ->
    let cc = Value.call_conv_of_typ e1.Source.note.S.note_typ in
    let inst = List.map (fun t -> t.Source.note) inst in
    I.CallE (cc, exp e1, inst, exp e2)
  | S.BlockE ([{it = S.ExpD e; _}], _) -> exp' e.at e.note e.it
  | S.BlockE (ds, ot) -> I.BlockE (decs ds, !ot)
  | S.NotE e -> I.IfE (exp e, falseE, trueE)
  | S.AndE (e1, e2) -> I.IfE (exp e1, exp e2, falseE)
  | S.OrE (e1, e2) -> I.IfE (exp e1, trueE, exp e2)
  | S.IfE (e1, e2, e3) -> I.IfE (exp e1, exp e2, exp e3)
  | S.SwitchE (e1, cs) -> I.SwitchE (exp e1, cases cs)
  | S.WhileE (e1, e2) -> I.WhileE (exp e1, exp e2)
  | S.LoopE (e1, None) -> I.LoopE (exp e1, None)
  | S.LoopE (e1, Some e2) -> I.LoopE (exp e1, Some (exp e2))
  | S.ForE (p, e1, e2) -> I.ForE (pat p, exp e1, exp e2)
  | S.LabelE (l, t, e) -> I.LabelE (l, t.Source.note, exp e)
  | S.BreakE (l, e) -> I.BreakE (l, exp e)
  | S.RetE e -> I.RetE (exp e)
  | S.AsyncE e -> I.AsyncE (exp e)
  | S.AwaitE e -> I.AwaitE (exp e)
  | S.AssertE e -> I.AssertE (exp e)
  | S.AnnotE (e, _) -> assert false
  | S.DecE (d, ot) -> I.BlockE (decs [d], !ot)

and field_to_dec (f : S.exp_field) : Ir.dec =
  let {it={S.id;S.exp=e;S.mut;_};at;note} = f in
  let d = match mut.it with
    | S.Const ->
      letD (idE id (Effect.typ e))
        (exp e)
    | S.Var ->
      varD id (exp e)
  in
  { d with at = at }

and field_to_obj_entry (f : S.exp_field) =
  match f.it.S.priv.it with
  | S.Private -> []
  | S.Public -> [ (f.it.S.name, f.it.S.id) ]

and obj at s class_id self_id es obj_typ =
  match s.it with
  | Type.Object _ -> build_obj at None s self_id es obj_typ
  | Type.Actor -> I.ActorE (self_id, exp_fields es, obj_typ)

and build_obj at class_id s self_id es obj_typ =
  let self =  idE self_id obj_typ in
  I.BlockE (
      List.map (field_to_dec) es @
        [ letD self
            (newObjE s (List.concat (List.map field_to_obj_entry es)) obj_typ);
	  expD self
        ],
      obj_typ)

and exp_fields fs = List.map exp_field fs

and exp_field f = phrase exp_field' f

and exp_field' (f : S.exp_field') =
  S.{ I.name = f.name; I.id = f.id; I.exp = exp f.exp; I.mut = f.mut; I.priv = f.priv }


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

and decs ds =
  match ds with
  | [] -> []
  | d::ds ->
    match d.it with
    | S.ClassD(_, con_id, _, _, _, _, _) ->
      let (c,k) = match con_id.note with Some p -> p | _ -> assert false in
      let typD = { it = I.TypD (c,k);
                   at = d.at;
                   note = { S.note_typ = T.unit;
                            S.note_eff = T.Triv }
                 }
      in
      typD :: (phrase' dec' d) :: (decs ds)
    | _ -> (phrase' dec' d) :: (decs ds)

and dec' at n d = match d with
  | S.ExpD e -> I.ExpD (exp e)
  | S.LetD (p, e) -> I.LetD (pat p, exp e)
  | S.VarD (i, e) -> I.VarD (i, exp e)
  | S.FuncD (s, i, tbs, p, ty, e) ->
    let cc = Value.call_conv_of_typ n.S.note_typ in
    I.FuncD (cc, i, typ_binds tbs, pat p, ty.note, exp e)
  | S.TypD (con_id, typ_bind, t) ->
    let (c,k) = Lib.Option.value con_id.note in
    I.TypD (c,k)
  | S.ClassD (fun_id, typ_id, tbs, s, p, self_id, es) ->
    let cc = Value.call_conv_of_typ n.S.note_typ in
    let inst = List.map
                 (fun tb ->
                   match tb.note with
                   | None -> assert false
                   | Some c -> T.Con (c, []))
                 tbs in
    let obj_typ =
      match n.S.note_typ with
      | T.Func(s,c,bds,dom,[rng]) ->
        assert(List.length inst = List.length bds);
        T.open_ inst rng
      | _ -> assert false
    in
    I.FuncD (cc, fun_id, typ_binds tbs, pat p, obj_typ, (* TBR *)
             { it = obj at s (Some fun_id) self_id es obj_typ;
               at = at;
               note = { S.note_typ = obj_typ; S.note_eff = T.Triv } })

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

and prog p = (decs p.it,
  { I.has_await = true
  ; I.has_async_typ = true
  })

(* validation *)

let transform scope p = prog p

