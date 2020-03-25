open Mo_types
open Type

(* `bi_match scope_opt tbs subs` returns
   a minimal instantiation `ts` such that
     * |`ts`| = |`tbs`|
     * `ts` satisfies the bounds in `tbs`
     * every `(t1,t2)` in `subs` satisfies `open_ ts t1 <: open_ ts t2`
     * any `Scope` parameter in `tbs` is instantiated with scope_opt, iff scope_opt <> None.
   or fails with exception `Failure msg` if
     * `tbs` contains open bounds mentioning parameters in `tbs` (a limitation); or
     * no such instantiation exists due to
       * a subtype violation; or
       * some parameter in `tbs` being under constrained (`ts` is ambiguous); or
       * some parameter in `tbs` being over constrained (no `ts` exists); or
   For every pair (t1, t2) in `subs`,  one of `t1` or `t2` must be closed
   w.r.t `tbs` and the other may be "open" mentioning parameters in `tbs`
   by index (Var i, i < |`tbs`|).
   (This a pre-condition that avoid the need for full unitication.)

   The ordering on instantiations `ts` is determined pointwise depending on the
   occurrence of that variable in `subs` and is:
   * sub-typing ( _ <: _) on variables that occur strictly positively in subs
   * super-typing ( _ :> _) on variables that occur strictly negatively in subs
   * equivalence ( _ = _ ) on variables that occur both positively and negatively in subs
   * trivial relation {(Non,Non)} on variables that don't occur at all in subs

   (modulo mixing my left foot with my right)
*)

val bi_match_subs :
  scope option -> bind list -> (typ * typ) list ->
  typ list (* raises Failure *)
