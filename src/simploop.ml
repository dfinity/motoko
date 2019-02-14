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
  | I.ActorDotE(e, n) -> I.ActorDotE(exp e, n)
  | I.DeclareE(i, t, e) -> I.DeclareE(i, t, exp e)
  | I.DefineE(i, m, e) -> I.DefineE(i, m, exp e)
  | I.NewObjE(s, nis, t) -> I.NewObjE(s, nis, t)
  | I.ActorE(i, fs, t) -> I.ActorE(i, exp_fields fs, t)
  (*--- "Interesting" cases: The looping constructs. ---*)
  | I.LoopE (e1, None) -> I.LoopE (exp e1, None) (* <-- This form is simplest, and preferred *)

  (* Questions about the target of each transformation case below:

     1. Do we need the loop { } expression _and_ the label; does the
     label alone suffice?  (Can we label any subexpression and make it
     a jump target, or only loops?)

     Cladio: No.  Loops need not be labeled, except to break.

     2. Related to 1, if we do need the loop constructs and the
     labels, do we also need to use explicit "continue" expressions?

     3. The rewrite for for-loops is the most involved, and involves a
     subexpression with nontrivial (non-unit, non-boolean) type;
     namely, the first subexpression of the for-loop must be an
     iterator object.  Doing this rewrite without first doing this
     check would be wrong, but assuming the AST is typed, no extra
     type information seems required to inform or guide the
     transformation; it is "fully parametric" in all of the
     sub-expressions, it seems.

     Hence, all of these rewrites could probably happen in the desugar
     module, though they should only happen on a well-typed source
     AST.

     ----------------------------------------------------------------------------------------------------------
     Claudio's Answers, copied from here: https://github.com/dfinity-lab/actorscript/pull/146#commitcomment-32218790

     - I believe you can label any expression and make it a jump target.

       (Matthew: Ok. But because I'd rather not loose all structure, I guess I'll label loops, and not try to create, e.g., strange non-structural loops)

     - There is no need to label the loop unless you do and early break or continue.

       (Matthew: Ok, I seem to need to break every loop that I create, so I guess each needs a label?)

     - The continue in the while translation is redundant, I think; just fall through.

       (Matthew: Ok.)

     - Continue isn't special IRCC, it's just a special named label ("continue-" or something like that) introduced by the parser.
       (the loop(_,None) construct just loops forever, but you can still exit via break).

     - I would indeed just do this desugaring in the parser

       (Matthew: Ok, sounds good; I'll keep this separate until it works, then integrate there.)

     - you might even be able to extend construct_ir.ml with functions whileE etc for constructing primitive
       loops and modify the transformation to use those instead of WhileE etc.

       (Matthew: Ok. I'll define the RHS of each rewrite in that module, for potential use elsewhere in the pipeline too.)
   *)

  (* Transformation from loop-while to loop: *)
  | I.LoopE (e1, Some e2) -> Construct.loopWhileE' (exp e1) (exp e2)

  (* Transformation from while to loop: *)
  | I.WhileE (e1, e2) -> Construct.whileE' (exp e1) (exp e2)

  (* Transformation from for to loop: *)
  | I.ForE (p, e1, e2) -> Construct.forE' p (exp e1) (exp e2)

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
  | I.TypD c -> I.TypD c

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
