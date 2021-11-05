open Mo_types

open Type
(* Signatures *)

let cat = "Compatibility"

(* signature matching with multiple error reporting
   c.f. (simpler) Types.match_sig.
*)

let error_discard s tf =
  Diag.add_msg s
    (Diag.error_message Source.no_region "" ""
       (Printf.sprintf "field %s of previous type %s cannot be discarded; promote to `Any` instead" tf.lab (Type.string_of_typ tf.typ)))

let error_sub s tf1 tf2 =
  Diag.add_msg s
    (Diag.error_message Source.no_region cat ""
      (Printf.sprintf "field %s of previous type %s cannot be consumed at new type %s" tf1.lab (Type.string_of_typ tf1.typ) (Type.string_of_typ tf2.typ)))

let warn_mut s tf1 tf2 =
  Diag.add_msg s
   (Diag.warning_message Source.no_region cat ""
     (Printf.sprintf "change in mutability of field %s from previous type %s to new type %s" tf1.lab (Type.string_of_typ tf1.typ) (Type.string_of_typ tf2.typ)))

let match_sig tfs1 tfs2 : unit Diag.result =
  (* Assume that tfs1 and tfs2 are sorted. *)
  let res = Diag.with_message_store (fun s ->
    (* Should we insist on monotonic preservation of fields, or relax? *)
    let rec go tfs1 tfs2 = match tfs1, tfs2 with
      | [], _ ->
        Some () (* no or additional fields ok *)
      | tf1 :: tfs1', [] ->
        error_discard s tf1;
        go tfs1' []
      | tf1::tfs1', tf2::tfs2' ->
        (match Type.compare_field tf1 tf2 with
         | 0 ->
           (* Sshould we enforce equal mutability or not?
              Seems unnecessary since upgrade is read-once *)
            if Type.is_mut tf1.typ <> Type.is_mut tf2.typ then
              warn_mut s tf1 tf2;
            if not (sub (as_immut tf1.typ) (as_immut tf2.typ)) then
              error_sub s tf1 tf2;
            go tfs1' tfs2'
       | -1 ->
          error_discard s tf1;
          go tfs1' tfs2'
        | _ ->
          go tfs1 tfs2' (* new field ok, recurse on tfs2' *)
        )
    in go tfs1 tfs2)
  in
  (* cross check with simpler definition *)
  match res with
  | Ok _ ->
    assert (Type.match_sig tfs1 tfs2);
    res
  | Error _ ->
    assert (not (Type.match_sig tfs1 tfs2));
    res
