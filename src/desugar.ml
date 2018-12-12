module S = Syntax
module I = Ir


(* Combinators used in the desguaring *) 

let true_lit : Ir.exp =
  Source.(I.LitE (S.BoolLit true) @@ no_region)
let false_lit : Ir.exp =
  Source.(I.LitE (S.BoolLit false) @@ no_region)


let apply_sign op l = Syntax.(match op, l with
  | PosOp, l -> l
  | NegOp, NatLit n -> NatLit (Value.Nat.sub Value.Nat.zero n)
  | NegOp, IntLit n -> IntLit (Value.Int.sub Value.Int.zero n)
  | _, _ -> raise (Invalid_argument "Invalid signed pattern")
  )


let phrase f x = Source.(f x.it @@ x.at)
let phrase' f x = Source.(f x.note x.it @@ x.at)

let
  rec exps es = List.map exp es
  and exp e = phrase exp' e
  and exp' = function
    | S.PrimE p -> I.PrimE p
    | S.VarE i -> I.VarE i
    | S.LitE l -> I.LitE !l
    | S.UnE (o, e) -> I.UnE (o, exp e)
    | S.BinE (e1, o, e2) -> I.BinE (exp e1, o, exp e2)
    | S.RelE (e1, o, e2) -> I.RelE (exp e1, o, exp e2)
    | S.TupE es -> I.TupE (exps es)
    | S.ProjE (e, i) -> I.ProjE (exp e, i)
    | S.OptE e -> I.OptE (exp e)
    | S.ObjE (s, i, es) -> I.ObjE (s, i, exp_fields es)
    | S.DotE (e, n) -> I.DotE (exp e, n)
    | S.AssignE (e1, e2) -> I.AssignE (exp e1, exp e2)
    | S.ArrayE (m, es) -> I.ArrayE (m, exps es)
    | S.IdxE (e1, e2) -> I.IdxE (exp e1, exp e2)
    | S.CallE (e1, inst, e2) ->
      let cc = Value.call_conv_of_typ e1.Source.note.S.note_typ in
      I.CallE (cc, exp e1, inst, exp e2)
    | S.BlockE ds -> I.BlockE (decs ds)
    | S.NotE e -> I.IfE (exp e, false_lit, true_lit)
    | S.AndE (e1, e2) -> I.IfE (exp e1, exp e2, false_lit)
    | S.OrE (e1, e2) -> I.IfE (exp e1, true_lit, exp e2)
    | S.IfE (e1, e2, e3) -> I.IfE (exp e1, exp e2, exp e3)
    | S.SwitchE (e1, cs) -> I.SwitchE (exp e1, cases cs)
    | S.WhileE (e1, e2) -> I.WhileE (exp e1, exp e2)
    | S.LoopE (e1, None) -> I.LoopE (exp e1, None)
    | S.LoopE (e1, Some e2) -> I.LoopE (exp e1, Some (exp e2))
    | S.ForE (p, e1, e2) -> I.ForE (pat p, exp e1, exp e2)
    | S.LabelE (l, t, e) -> I.LabelE (l, t, exp e)
    | S.BreakE (l, e) -> I.BreakE (l, exp e)
    | S.RetE e -> I.RetE (exp e)
    | S.AsyncE e -> I.AsyncE (exp e)
    | S.AwaitE e -> I.AwaitE (exp e)
    | S.AssertE e -> I.AssertE (exp e)
    | S.IsE (e1, e2) -> I.IsE (exp e1, exp e2)
    | S.AnnotE (e, _) -> exp' e.Source.it
    | S.DecE d -> I.BlockE [dec d]
    | S.DeclareE (i, t, e) -> I.DeclareE (i, t, exp e)
    | S.DefineE (i, m, e) -> I.DefineE (i, m, exp e)
    | S.NewObjE (s, fs) -> I.NewObjE (s, fs)

  and exp_fields fs = List.map exp_field fs
  and exp_field f = phrase exp_field' f
  and exp_field' (f : S.exp_field') =
    S.{ I.name = f.name; I.id = f.id; I.exp = exp f.exp; I.mut = f.mut; I.priv = f.priv}

  and decs ds = List.map dec ds
  and dec d = phrase' dec' d
  and dec' n = function
    | S.ExpD e -> I.ExpD (exp e)
    | S.LetD (p, e) -> I.LetD (pat p, exp e)
    | S.VarD (i, e) -> I.VarD (i, exp e)
    | S.FuncD (s, i, tp, p, ty, e) ->
      let cc = Value.call_conv_of_typ n.S.note_typ in
      I.FuncD (cc, i, tp, pat p, ty, exp e)
    | S.TypD (i, ty, t) -> I.TypD (i, ty, t)
    | S.ClassD (i1, i2, ty, s, p, i3, es) -> I.ClassD (i1, i2, ty, s, pat p, i3, exp_fields es)


  and cases cs = List.map case cs
  and case c = phrase case' c
  and case' c = S.{ I.pat = pat c.pat; I.exp = exp c.exp}

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
    | S.AnnotP (p, _) -> pat' p.Source.it

  and prog p = phrase decs p

