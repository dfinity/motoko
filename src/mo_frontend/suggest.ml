
(* Suggestions *)
open Mo_types
open Mo_config
open Type

let oneof sep lastsep ss =
  let rest, last = Lib.List.split_last ss in
  ((if rest <> [] then (String.concat sep rest) ^ lastsep else "") ^ last)

let suggest_id desc id ids =
  if !Flags.ai_errors then
    Printf.sprintf
      "\nThe %s %s is not available. Try something else?"
      desc
      id
  else
  let suggestions =
    let limit = Lib.Int.log2 (String.length id) in
    let distance = Lib.String.levenshtein_distance id in
    let weighted_ids = List.filter_map (fun id0 ->
      let d = distance id0 in
      if Lib.String.starts_with id id0 || d <= limit then
        Some (d, id0)
      else None) ids in
    List.sort compare weighted_ids |> List.map snd
  in
  if suggestions = [] then ""
  else
  Printf.sprintf "\nDid you mean %s %s?" desc (oneof ", " " or " suggestions)

let search_obj desc path ty ty1 ty2 =
  let suggestions = ref [] in
  let seen = ref S.empty in
  let rec go path ty =
  if S.mem ty !seen then ()
  else begin
    seen := S.add ty (!seen);
    match promote ty with
    | Obj(_, tfs) ->
      tfs |>
      List.iter (fun {lab;typ;_} ->
        match normalize typ with
        | Func _ when
          (Lib.String.starts_with "to" lab ||
           Lib.String.starts_with "from" lab) &&
           sub typ (Func(Local, Returns,  [], [ty1], [ty2])) ->
          suggestions := Printf.sprintf "`%s.%s(_)`%s" path lab desc :: !suggestions
        | Obj(_, tfs) as ty1  ->
          go (path^"."^lab) ty1
        | _ -> ())
    | _ -> ()
    end
  in
  go path ty;
  !suggestions

let suggest_conversion libs vals ty1 ty2 =
  match promote ty1, promote ty2 with
  | Prim p1, Prim p2 ->
    let suggestions = ref [] in
    Env.iter (fun filename ty ->
      if Lib.String.starts_with "@" filename
      then () (* skip prim etc *)
      else
      let imported_name =
        (* try to determine imported name, if any *)
        Env.fold (fun id (ty1, _, _, _) acc ->
            if ty == ty1 (*HACK*)
            then Some id
            else acc)
          vals None
      in
      let lib_opt = match imported_name with
        | Some id -> Some (id, "")
        | None ->
          (* search libs for suggested import *)
          Flags.M.fold (fun package path acc  ->
              let base = Lib.FilePath.normalise path in
              match Lib.FilePath.relative_to base filename with
              | None -> acc
              | Some rel_path ->
                let rel_name = Filename.chop_extension rel_path in
                let id = Filename.basename rel_name in
                Some (
                  id,
                  Printf.sprintf  " after adding `import %s = \"mo:%s/%s\"`" id package rel_name))
             !Flags.package_urls None
      in
      match lib_opt with
      | None -> ()
      | Some (id, desc) ->
        suggestions := (search_obj desc id ty ty1 ty2) @ !suggestions)
      libs;
    if !suggestions = []
    then ""
    else
      Printf.sprintf "\nMaybe try conversion:\n  %s?"
      (oneof ",\n  " " or\n  " !suggestions)
  (* not primitive types, make no suggestion *)
  | _, _ -> ""

