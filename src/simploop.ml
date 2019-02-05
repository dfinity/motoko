(* 
   Simplify loop constructs.

   Mapping each occurence of a looping construct to an infinite loop.
 *)

open Source
module I = Ir

let phrase f x =  { x with it = f x.it }

let phrase' f x = { x with it = f x.at x.note x.it }

let rec exps es = List.map exp es

and exp e = phrase' exp' e

and exp' at note = function
  | I.AsyncE _ -> assert false
  | I.AwaitE _ -> assert false
  | I.PrimE p -> I.PrimE p
  | I.VarE i -> I.VarE i
  | I.LitE l -> I.LitE l
  | I.UnE (ot, o, e) ->
    I.UnE (ot, o, exp e)
  | I.BinE (ot, e1, o, e2) ->
    I.BinE (ot, exp e1, o, exp e2)
  | I.RelE (ot, e1, o, e2) ->
    I.RelE (ot, exp e1, o, exp e2)
  | I.TupE es -> I.TupE (exps es)
  | I.ProjE (e, i) -> I.ProjE (exp e, i)
  | I.OptE e -> I.OptE (exp e)
  | I.DotE (e, n) -> I.DotE(exp e, n)
  | I.AssignE (e1, e2) -> I.AssignE (exp e1, exp e2)
  | I.ArrayE (m, t, es) -> I.ArrayE (m, t, exps es)
  | I.IdxE (e1, e2) -> I.IdxE (exp e1, exp e2)
  | I.CallE (cc, e1, inst, e2) -> I.CallE (cc, exp e1, inst, exp e2)
  | I.BlockE (ds, ot) -> I.BlockE (decs ds, ot)
  | I.IfE (e1, e2, e3) -> I.IfE (exp e1, exp e2, exp e3)
  | I.SwitchE (e1, cs) -> I.SwitchE (exp e1, cases cs)
  | I.LabelE (l, t, e) -> I.LabelE (l, t, exp e)
  | I.BreakE (l, e) -> I.BreakE (l, exp e)
  | I.RetE e -> I.RetE (exp e)
  | I.AssertE e -> I.AssertE (exp e)
  | I.IsE (e1, e2) -> I.IsE (exp e1, exp e2)
  | I.ActorDotE(e, n) -> I.ActorDotE(exp e, n)
  | I.DeclareE(i, t, e) -> I.DeclareE(i, t, exp e)
  | I.DefineE(i, m, e) -> I.DefineE(i, m, exp e)
  | I.NewObjE(s, nis, t) -> I.NewObjE(s, nis, t)
  | I.ActorE(i, fs, t) -> I.ActorE(i, exp_fields fs, t)
  (*--- "Interesting" cases: The looping constructs. ---*)
  | I.LoopE (e1, None) -> I.LoopE (exp e1, None) (* <-- This form is simplest, and preferred *)
  | I.LoopE (e1, Some e2) -> I.LoopE (exp e1, Some (exp e2)) (* TODO: Transform this form to LoopE(_,None) *)
  | I.WhileE (e1, e2) -> I.WhileE (exp e1, exp e2) (* TODO: Transform this form to LoopE(_,None) *)
  | I.ForE (p, e1, e2) -> I.ForE (p, exp e1, exp e2) (* TODO: Transform this form to LoopE(_,None) *)

and exp_fields fs = List.map exp_field fs

and exp_field f = phrase exp_field' f

and exp_field' (f : Ir.exp_field') =
  Ir.{ I.name = f.name; I.id = f.id; I.exp = exp f.exp; I.mut = f.mut; I.priv = f.priv }

and decs ds =
  match ds with
  | [] -> []
  | d::ds -> (phrase' dec' d) :: (decs ds)

and dec' at n d = match d with
  | I.ExpD e -> I.ExpD (exp e)
  | I.LetD (p, e) -> I.LetD (p, exp e)
  | I.VarD (i, e) -> I.VarD (i, exp e)
  | I.FuncD (cc, i, tbs, p, ty, e) ->
    I.FuncD (cc, i, tbs, p, ty, exp e)
  | I.TypD (c,k) -> 
    I.TypD (c,k)

and cases cs = List.map case cs

and case c = phrase case' c

and case' c = I.{ I.pat = c.pat; I.exp = exp c.exp }

and prog p = phrase decs p

(* validation *)

let check_prog scope (prog:I.prog) =
  let env = Check_ir.env_of_scope scope  in
  Check_ir.check_prog env prog

let transform scope (p:I.prog) =
  let p' = prog p in
  check_prog scope p';
  p'
