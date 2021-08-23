open Mo_types
open Type

(* `bi_match scope_opt tbs subs ret_opt` returns
   a minimal instantiation `ts` such that
     * |`ts`| = |`tbs`|
     * `ts` satisfies the bounds in `tbs`
     * every `(t1,t2)` in `subs` satisfies `open_ ts t1 <: open_ ts t2`
     * any `Scope` parameter in `tbs` is instantiated with scope_opt, iff scope_opt <> None.
   or fails with exception `Failure msg` if
     * `tbs` contains open bounds mentioning parameters in `tbs` (a limitation); or
     * no such instantiation exists due to
       * a subtype violation; or
       * some parameter in `tbs` being under constrained (`ts` is ambiguous, see below); or
       * some parameter in `tbs` being over constrained (no `ts` exists); or
   For every pair (t1, t2) in `subs`,  one of `t1` or `t2` must be closed
   w.r.t `tbs` and the other may be "open" mentioning parameters in `tbs`
   by index (Var i, i < |`tbs`|).
   (This a pre-condition that avoids the need for full unification.)

   The ordering on instantiations `ts` is determined pointwise depending on the
   occurrence of that variable in `subs` and is:
   * sub-typing ( _ <: _) on variables that occur strictly positively in `subs`
   * super-typing ( _ :> _) on variables that occur strictly negatively in `subs`
   * equivalence ( _ = _ ) on variables that occur both positively and negatively in `subs`
   * trivial relation {(Non,Non)} on variables that don't occur at all in subs

   The optional formal return type, `ret_typ`, used when synthesizing an application,
   and mentioning parameters in `tbs`, is used to determine whether the inferred instantiation is
   ambiguous, guided by the polarities of type parameters from `tbs` in `ret_opt`.

   The inference algorithm infers the lower and upper bounds of each instantiation given the constraints
   in `subs` (and initial bounds on tbs).

   Given a parameter, `T`, if its inferred upper and lower bound coincide, the instantiation is the lower (=upper) type.
   Otherwise:
   * if the lower bound is (a proper) supertype of the upper bound, reject (no sastifying instantiation of `T` exists).
   * if the lower bound is (a proper) subtype of the upper bound and `T` occurs in `ret_opt` as:
     * positive (use the (principal) lower bound)
     * negative (use the (principal) upper bound)
     * neutral (use the lower bound)
     * invariant : reject the instantiation of `T` as under-constrained - no principal solution exists.

   (modulo mixing my left foot with my right)
*)

exception Bimatch of string

(* General parameter inference for a conjunction of subtype problems *)
val bi_match_subs :
  scope option ->
  bind list ->               (* type parameters to instantiate *)
  (typ * typ) list ->        (* sub-type problems mentioning tbs either on
                                left or right, but never both sides *)
  typ option ->              (* optional return type mentioning tbs
                                determining polarities *)
  typ list (* raises Bimatch *)


(* Parameter inference for function calls *)
val bi_match_call :
  scope option ->
  (bind list * typ * typ) -> (* function type *)
  typ ->                     (* argument type *)
  typ option ->              (* optional expected result type *)
  typ list (* raises Bimatch *)
