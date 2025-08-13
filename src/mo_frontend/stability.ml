open Mo_types

open Type

let migration_link = "https://internetcomputer.org/docs/motoko/fundamentals/actors/compatibility#explicit-migration-using-a-migration-function"

(* Signature matching *)

let cat = "Compatibility"

(* signature matching with multiple error reporting
   c.f. (simpler) Types.match_sig.
*)

let display_typ = Lib.Format.display Type.pp_typ

let display_typ_expand = Lib.Format.display Type.pp_typ_expand

let error_discard s tf =
  Diag.add_msg s
    (Diag.error_message Source.no_region "M0169" cat
      (Format.asprintf "the stable variable %s of the previous program version cannot be implicitly discarded. The variable can only be dropped by an explicit migration function, please see %s"
        tf.lab
        migration_link))

let error_sub s tf1 tf2 explanation =
  Diag.add_msg s
    (Diag.error_message Source.no_region "M0170" cat
      (Format.asprintf "the type of stable variable %s is not compatible to the previous program version:\n %s.\n Write an explicit migration function, please see %s."
        tf1.lab
        (string_of_reason explanation)
        migration_link))

let error_stable_sub s tf1 tf2 explanation =
  Diag.add_msg s
    (Diag.error_message Source.no_region "M0216" cat
      (Format.asprintf "the type of stable variable %s implicitly drops data of the previous program version:\n %s.\n The data can only be dropped by an explicit migration function, please see %s."
        tf1.lab
        (string_of_reason explanation)
        migration_link))

let error_required s tf =
  Diag.add_msg s
    (Diag.error_message Source.no_region "M0169" cat
      (Format.asprintf "the previous program version does not contain the required stable variable %s."
        tf.lab))


(*
   - Mutability of stable fields can be changed because they are never aliased.
   - Stable fields cannot be dropped.
   - Lossy promotion to any or dropping record fields is rejected (stricter than subtyping to prevent data loss).
*)
let match_stab_sig sig1 sig2 : unit Diag.result =
  let tfs1 = post sig1 in
  let tfs2 = pre sig2 in
  (* Assume that tfs1 and tfs2 are sorted. *)
  let res = Diag.with_message_store (fun s ->
    let rec go tfs1 tfs2 = match tfs1, tfs2 with
      | [], _ ->
         List.iter (fun (required, tf) ->
           if required then error_required s tf) tfs2;
         Some () (* new fields ok *)
      | tf1 :: tfs1', [] ->
        (* dropped field rejected, recurse on tfs1' *)
        error_discard s tf1;
        go tfs1' []
      | tf1::tfs1', (is_required, tf2)::tfs2' ->
        (match Type.compare_field tf1 tf2 with
         | 0 ->
            let context = [StableVariable tf2.lab] in
            begin
              match Type.sub_explained context (as_immut tf1.typ) (as_immut tf2.typ) with
              | Incompatible explanation -> error_sub s tf1 tf2 explanation
              | Compatible ->
                match Type.stable_sub_explained context (as_immut tf1.typ) (as_immut tf2.typ) with
                | Incompatible explanation -> error_stable_sub s tf1 tf2 explanation
                | Compatible -> ()
            end;
            go tfs1' tfs2'
         | -1 ->
           (* dropped field rejected, recurse on tfs1' *)
           error_discard s tf1;
           go tfs1' tfs2
         | _ ->
            (if is_required then error_required s tf2);
           go tfs1 tfs2' (* new field ok, recurse on tfs2' *)
        )
    in go tfs1 tfs2)
  in
  (* cross check with simpler definition *)
  match res with
  | Ok _ ->
    assert (Type.match_stab_sig sig1 sig2);
    res
  | Error _ ->
    assert (not (Type.match_stab_sig sig1 sig2));
    res
