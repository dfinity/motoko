open Mo_types

open Type

(* Signature matching *)

let cat = "Compatibility"

(* signature matching with multiple error reporting
   c.f. (simpler) Types.match_sig.
*)

let display_typ_expand = Lib.Format.display Type.pp_typ_expand

let error_sub s tf1 tf2 =
  Diag.add_msg s
    (Diag.error_message Source.no_region "M0170" cat
      (Format.asprintf "stable variable %s of previous type%a\ncannot be consumed at new type%a"
        tf1.lab
        display_typ_expand tf1.typ
        display_typ_expand tf2.typ))

(* Relaxed rules with enhanced orthogonal persistence for more flexible upgrades.
   - Mutability of stable fields can be changed (because they are never aliased).
   - Stable fields can be dropped (abandoning the transitivity property of upgrades).

   Upgrade transitivity means that an upgrade from a program A to B and then from B to C 
   should have the same effect as directly upgrading from A to C. If B discards a field 
   and C re-adds it, this transitivity is no longer maintained. However, rigorous upgrade 
   transitivity was also not guaranteed before, since B may contain initialization logic
   or pre-/post-upgrade hooks that alter the stable data.
*)
let match_stab_sig tfs1 tfs2 : unit Diag.result =
  (* Assume that tfs1 and tfs2 are sorted. *)
  let res = Diag.with_message_store (fun s ->
    let rec go tfs1 tfs2 = match tfs1, tfs2 with
      | [], _ | _, [] -> 
        (* same amount of fields, new fields, or dropped fields ok *)
        Some ()
      | tf1::tfs1', tf2::tfs2' ->
        (match Type.compare_field tf1 tf2 with
         | 0 ->
            if not (sub (as_immut tf1.typ) (as_immut tf2.typ)) then
              error_sub s tf1 tf2;
            go tfs1' tfs2'
        | -1 ->
          go tfs1' tfs2 (* dropped field ok, recurse of tfs1' *)
        | _ ->
          go tfs1 tfs2' (* new field ok, recurse on tfs2' *)
        )
    in go tfs1 tfs2)
  in
  (* cross check with simpler definition *)
  match res with
  | Ok _ ->
    assert (Type.match_stab_sig tfs1 tfs2);
    res
  | Error _ ->
    assert (not (Type.match_stab_sig tfs1 tfs2));
    res
