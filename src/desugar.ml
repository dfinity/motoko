open Source
module S = Syntax
module I = Ir
module T = Type

(* Combinators used in the desguaring *)

let bool_lit b : Ir.exp =
  {Source.it = I.LitE (S.BoolLit b);
   Source.at = no_region;
   Source.note = {S.note_typ = T.bool;
                  S.note_eff = T.Triv}
  }

let true_lit : Ir.exp = bool_lit true
let false_lit : Ir.exp = bool_lit false

let apply_sign op l = Syntax.(match op, l with
  | PosOp, l -> l
  | NegOp, NatLit n -> NatLit (Value.Nat.sub Value.Nat.zero n)
  | NegOp, IntLit n -> IntLit (Value.Int.sub Value.Int.zero n)
  | _, _ -> raise (Invalid_argument "Invalid signed pattern")
  )

let phrase ce f x =  {x with it = f ce x.it}
let phrase' ce f x = {x with it = f ce x.at x.note x.it}

let
  rec exps ce es = List.map (exp ce) es
  and exp ce e = phrase' ce exp' e
  and exp' ce at note = function
    | S.PrimE p -> I.PrimE p
    | S.VarE i -> I.VarE i
    | S.LitE l -> I.LitE !l
    | S.UnE (ot, o, e) ->
      I.UnE (!ot, o, exp ce e)
    | S.BinE (ot, e1, o, e2) ->
      I.BinE (!ot, exp ce e1, o, exp ce e2)
    | S.RelE (ot, e1, o, e2) ->
      I.RelE (!ot, exp ce e1, o, exp ce e2)
    | S.TupE es -> I.TupE (exps ce es)
    | S.ProjE (e, i) -> I.ProjE (exp ce e, i)
    | S.OptE e -> I.OptE (exp ce e)
    | S.ObjE (s, i, es) -> obj ce at s None i es note.S.note_typ
    | S.DotE (e, n) ->
      begin match Type.as_immut (Type.promote ce (e.Source.note.S.note_typ)) with
      | Type.Obj (Type.Actor, _) -> I.ActorDotE (exp ce e, n)
      | Type.Obj (_,  _) | Type.Array _ -> I.DotE (exp ce e, n)
      | Type.Con _ -> raise (Invalid_argument ("Con in promoted type"))
      | _ -> raise (Invalid_argument ("non-object in dot operator"))
      end
    | S.AssignE (e1, e2) -> I.AssignE (exp ce e1, exp ce e2)
    | S.ArrayE (m, es) -> I.ArrayE (m, exps ce es)
    | S.IdxE (e1, e2) -> I.IdxE (exp ce e1, exp ce e2)
    | S.CallE (e1, inst, e2) ->
      let cc = Value.call_conv_of_typ e1.Source.note.S.note_typ in
      let inst = List.map (fun t -> t.Source.note) inst in
      I.CallE (cc, exp ce e1, inst, exp ce e2)
    | S.BlockE ds -> I.BlockE (decs ce ds)
    | S.NotE e -> I.IfE (exp ce e, false_lit, true_lit)
    | S.AndE (e1, e2) -> I.IfE (exp ce e1, exp ce e2, false_lit)
    | S.OrE (e1, e2) -> I.IfE (exp ce e1, true_lit, exp ce e2)
    | S.IfE (e1, e2, e3) -> I.IfE (exp ce e1, exp ce e2, exp ce e3)
    | S.SwitchE (e1, cs) -> I.SwitchE (exp ce e1, cases ce cs)
    | S.WhileE (e1, e2) -> I.WhileE (exp ce e1, exp ce e2)
    | S.LoopE (e1, None) -> I.LoopE (exp ce e1, None)
    | S.LoopE (e1, Some e2) -> I.LoopE (exp ce e1, Some (exp ce e2))
    | S.ForE (p, e1, e2) -> I.ForE (pat ce p, exp ce e1, exp ce e2)
    | S.LabelE (l, t, e) -> I.LabelE (l, t.Source.note, exp ce e)
    | S.BreakE (l, e) -> I.BreakE (l, exp ce e)
    | S.RetE e -> I.RetE (exp ce e)
    | S.AsyncE e -> I.AsyncE (exp ce e)
    | S.AwaitE e -> I.AwaitE (exp ce e)
    | S.AssertE e -> I.AssertE (exp ce e)
    | S.IsE (e1, e2) -> I.IsE (exp ce e1, exp ce e2)
    | S.AnnotE (e, _) -> exp' ce at note e.it
    | S.DecE d -> I.BlockE (decs ce [d])
    | S.DeclareE (i, t, e) -> I.DeclareE (i, t, exp ce e)
    | S.DefineE (i, m, e) -> I.DefineE (i, m, exp ce e)
    | S.NewObjE (s, fs) -> I.NewObjE (s, fs, note.S.note_typ)

  and field_to_dec ce (f : S.exp_field) : Ir.dec =
    match f.it.S.mut.it with
    | S.Const ->
      {it = I.LetD ({it = I.VarP f.it.S.id; at = no_region;
                     note = f.it.S.exp.note.S.note_typ
                    },
                    exp ce f.it.S.exp);
       at = f.at;
       note = { f.it.S.exp.note with S.note_typ = T.unit}
      }
    | S.Var   ->
      {it = I.VarD (f.it.S.id, exp ce f.it.S.exp);
       at = f.at;
       note = { f.it.S.exp.note with S.note_typ = T.unit}
      }

  and field_to_obj_entry (f : S.exp_field) =
    match f.it.S.priv.it with
    | S.Private -> []
    | S.Public -> [ (f.it.S.name, f.it.S.id) ]

  and obj ce at s class_id self_id es obj_typ =
    match s.it with
    | Type.Object _ -> build_obj ce at None self_id es obj_typ
    | Type.Actor -> I.ActorE (self_id, exp_fields ce es, obj_typ)

  and build_obj ce at class_id self_id es obj_typ =
    I.BlockE (
      List.map (field_to_dec ce) es @
        [ {it = I.LetD (
                    {it = I.VarP self_id;
                     at = at;
                     note = obj_typ
                    },
                    {it = I.NewObjE
                            (Type.Object Type.Local @@ at,
                             List.concat (List.map field_to_obj_entry es),
                             obj_typ
                            );
                     at = at;
                     note = {S.note_typ = obj_typ; S.note_eff = T.Triv}}
                  );
           at = no_region;
           note = {S.note_typ = T.unit;
                   S.note_eff = T.Triv}};
	  {it = I.ExpD {it = I.VarE self_id;
                        at;
                        note = {S.note_typ = obj_typ;
                                S.note_eff = T.Triv}};
           at = at;
           note = {S.note_typ = obj_typ;
                   S.note_eff = T.Triv}};
        ])

  and exp_fields ce fs = List.map (exp_field ce) fs
  and exp_field ce f = phrase ce exp_field' f
  and exp_field' ce (f : S.exp_field') =
    S.{ I.name = f.name; I.id = f.id; I.exp = exp ce f.exp; I.mut = f.mut; I.priv = f.priv}


  and typ_binds ce tbs = List.map (typ_bind ce) tbs
  and typ_bind ce tb =
    phrase' ce typ_bind' tb
  and typ_bind' ce at n {S.var; S.bound} = {Type.var = var.it; Type.bound = bound.note}
  and decs ce ds =
      match ds with
      | [] -> []
      | d::ds ->
        match d.it with
        | S.ClassD(_,con_id,_,_,_,_,_) ->
          let (c,k) = match con_id.note with Some p -> p | _ -> assert false in
          let typD = {it = I.TypD (c,k);
                      at = d.at;
                      note = {S.note_typ = T.unit; S.note_eff = T.Triv}}
          in
          typD::(phrase' ce dec' d)::(decs ce ds)
        | _ -> (phrase' ce dec' d)::(decs ce ds)
  and dec' ce at n d = match d with
    | S.ExpD e -> I.ExpD (exp ce e)
    | S.LetD (p, e) -> I.LetD (pat ce p, exp ce e)
    | S.VarD (i, e) -> I.VarD (i, exp ce e)
    | S.FuncD (s, i, tbs, p, ty, e) ->
      let cc = Value.call_conv_of_typ n.S.note_typ in
      I.FuncD (cc, i, typ_binds ce tbs, pat ce p, ty.note, exp ce e)
    | S.TypD (con_id, typ_bind, t) ->
      let (c,k) = Lib.Option.value con_id.note in
      I.TypD (c,k)
    | S.ClassD (fun_id, typ_id, tbs, s, p, self_id, es) ->
      let cc = Value.call_conv_of_typ n.S.note_typ in
      let inst = List.map
                   (fun tb ->
                     match tb.note with
                     | Type.Pre -> assert false
                     | t -> t)
                   tbs in
      let obj_typ =
        match n.S.note_typ with
        | T.Func(s,c,bds,dom,[rng]) ->
          assert(List.length inst = List.length bds);
          T.open_ inst rng 
        | _ -> assert false
      in
      I.FuncD (cc, fun_id, typ_binds ce tbs, pat ce p, obj_typ, (* TBR *)
               {it = obj ce at s (Some fun_id) self_id es obj_typ;
                at = at;
                note = {S.note_typ = obj_typ; S.note_eff = T.Triv}})

  and cases ce cs = List.map (case ce) cs
  and case ce c = phrase ce case' c
  and case' ce c = S.{ I.pat = pat ce c.pat; I.exp = exp ce c.exp}

  and pats ce ps = List.map (pat ce) ps
  and pat ce p = phrase ce pat' p
  and pat' ce = function
    | S.VarP v -> I.VarP v
    | S.WildP -> I.WildP
    | S.LitP l -> I.LitP !l
    | S.SignP (o, l) -> I.LitP (apply_sign o !l)
    | S.TupP ps -> I.TupP (pats ce ps)
    | S.OptP p -> I.OptP (pat ce p)
    | S.AltP (p1, p2) -> I.AltP (pat ce p1, pat ce p2)
    | S.AnnotP (p, _) -> pat' ce p.it

  and prog ce p = phrase ce decs p

