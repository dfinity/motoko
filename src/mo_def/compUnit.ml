open Mo_types

open Syntax


(* Compilation Units *)

(* Lib as pair of import decs and body decs *)
let obj_decs obj_sort at note id_opt fields =
  let open Source in
  match id_opt with
  | None -> [
    { it = ExpD {
        it = ObjE ({ it = obj_sort; at = no_region; note = ()}, fields);
        at;
        note };
      at; note }]
  | Some id -> [
    { it = LetD (
        { it = VarP id; at; note = note.note_typ },
        { it = ObjE ({ it = obj_sort; at = no_region; note = ()}, fields);
          at; note; });
      at; note
    };
    { it = ExpD { it = VarE id; at; note };
      at; note }
    ]

(* Happens after parsing, before type checking *)
let comp_unit_of_prog as_lib (prog : prog) : comp_unit =
  let open Source in
  let f = prog.note in

  let finish imports u = { it = (imports, u); note = f; at = no_region } in
  let prog_typ_note = { empty_typ_note with note_typ = Type.unit } in

  let rec go imports ds : comp_unit =
    match ds with
    (* imports *)
    | {it = LetD ({it = VarP n; _}, ({it = ImportE (url, ri); _} as e)); _} :: ds' ->
      let i : import = { it = (n, url, ri); note = e.note.note_typ; at = e.at } in
      go (imports @ [i]) ds'

    (* terminal expressions *)
    | [{it = ExpD ({it = ObjE ({it = Type.Module; _}, fields); _} as e); _}] when as_lib ->
      finish imports { it = ModuleU (None, fields); note = e.note; at = e.at }
    | [{it = ExpD ({it = ObjE ({it = Type.Actor; _}, fields); _} as e); _}] ->
      finish imports { it = ActorU (None, fields); note = e.note; at = e.at }
    | [{it = ClassD (sp, tid, tbs, p, typ_ann, {it = Type.Actor;_}, self_id, fields); _} as d] ->
      assert (tbs = []);
      finish imports { it = ActorClassU (sp, tid, p, typ_ann, self_id, fields); note = d.note; at = d.at }
    (* let-bound terminal expressions *)
    | [{it = LetD ({it = VarP i1; _}, ({it = ObjE ({it = Type.Module; _}, fields); _} as e)); _}] when as_lib ->
      finish imports { it = ModuleU (Some i1, fields); note = e.note; at = e.at }
    | [{it = LetD ({it = VarP i1; _}, ({it = ObjE ({it = Type.Actor; _}, fields); _} as e)); _}] ->
      finish imports { it = ActorU (Some i1, fields); note = e.note; at = e.at }

    (* Everything else is a program *)
    | ds' ->
      if as_lib
      then
        (* Deprecated syntax, see Typing.check_lib *)
        let fs = List.map (fun d -> {vis = Public @@ no_region; dec = d; stab = None} @@ d.at) ds' in
        finish imports {it = ModuleU (None, fs); at = no_region; note = empty_typ_note}
      else finish imports { it = ProgU ds; note = prog_typ_note; at = no_region }
  in
  go [] prog.it

(* To enable uniform definedness checking, typechecking and interpretation,
   present the unit as a list of declarations.
*)
let decs_of_comp_unit (cu : comp_unit) =
  let open Source in
  let (imports, cub) = cu.it in
  let import_decs = List.map (fun { it = (id, fp, ri); at; note}  ->
    { it = LetD (
      { it = VarP id; at; note; },
      { it = ImportE (fp, ri);
        at;
        note = { note_typ = note; note_eff = Type.Triv} });
      at;
      note = { note_typ = note; note_eff = Type.Triv } }) imports
  in
  import_decs,
  match cub.it with
  | ProgU ds -> ds
  | ModuleU (id_opt, fields) ->
    obj_decs Type.Module cub.at cub.note id_opt fields
  | ActorClassU (csp, i, p, t, i', efs) ->
    [{ it = ClassD (csp, i, [], p, t, { it = Type.Actor; at = no_region; note = ()}, i', efs);
       at = cub.at;
       note = cub.note;}];
  | ActorU (id_opt, fields) ->
    obj_decs Type.Actor cub.at cub.note id_opt fields

(* a hack to support compiling multiple files *)
let combine_progs progs : prog =
  let open Source in
  if progs = []
  then { it = []; at = no_region; note = "empty" }
  else { it = List.concat_map (fun p -> p.it) progs
       ; at = (Lib.List.last progs).at
       ; note = (Lib.List.last progs).note
       }
