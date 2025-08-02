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

(* Opaque context type for bi-matching *)
type ctx

(* General parameter inference for a conjunction of subtype problems.

 Solving can be done in two rounds:
 - Call the solver with the current set of sub-type problems to get the partial solution
 - Use the result to substitute solved type parameters (might contain remaining type variables)
 - Call the `finalize` function with the remaining sub-type problems (make sure the solved type parameters are substituted!)
   to get the final solution and the substitution of the remaining type variables from the 1st round
 *)
val bi_match_subs :
  scope option ->
  (* tbs: type parameters to instantiate *)
  bind list ->
  (* optional return type mentioning tbs determining polarities *)
  typ option ->
  (* sub-type problems mentioning tbs either on left or right, but never both sides *)
  (typ * typ) list ->
  (* types deferred until the next round;
   * types containing tbs that are part of the subtype problems in the 2nd round
   *)
  typ list ->
  (* solution and remaining context:
   * - Solution for all type parameters, remaining type variables are solved to Type.Con and must be solved in the 2nd round
   * - Remaining context for variables that need to be solved in the 2nd round. When None, all type variables are solved
   *)
  typ list * ctx option (* raises Bimatch *)

(* Finalizes the bi-match solution by solving remaining type variables and combining results *)
val finalize :
  (* Solution from the 1st round, used to produce the final combined solution *)
  typ list ->
  (* Remaining context for variables to be solved in this 2nd round *)
  ctx ->
  (* 2nd round sub-type problems *)
  (typ * typ) list ->
  (* Final solution and substitution of the remaining type variables from the 1st round *)
  typ list * typ ConEnv.t

(* Checks that all types are closed (no unresolved type variables) *)
val fail_when_types_are_not_closed : ctx -> typ list -> unit

(* Indicates whether debug prints are enabled *)
val debug : bool
