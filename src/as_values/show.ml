module T = As_types.Type

(* Entry point for type checking: *)

let rec can_show t =
  let t = T.normalize t in
  match t with
  | T.Prim T.Bool
  | T.Prim T.Nat
  | T.Prim T.Int
  | T.Prim T.Text
  | T.Prim T.Null -> true
  | T.Tup ts' -> List.for_all can_show ts'
  | T.Opt t' -> can_show t'
  | T.Array t' -> can_show (T.as_immut t')
  | T.Obj (T.Object _, fs) ->
    List.for_all (fun f -> can_show (T.as_immut f.T.typ)) fs
  | T.Variant cts ->
    List.for_all (fun f -> can_show f.T.typ) cts
  | _ -> false

(* Entry point for the interpreter (reference implementation) *)

let rec show_val t v =
  let t = T.normalize t in
  match t, v with
  | T.Prim T.Bool, Value.Bool b -> if b then "true" else "false"
  | T.Prim T.Nat, Value.Int i -> Value.Int.to_string i
  | T.Prim T.Int, Value.Int i -> Value.Int.to_string i
  | T.Prim T.Text, Value.Text s -> "\"" ^ s ^ "\""
  | T.Prim T.Null, Value.Null -> "null"
  | T.Opt _, Value.Null -> "null"
  | T.Opt t', Value.Opt v -> "?(" ^ show_val t' v ^ ")"
  | T.Tup ts', Value.Tup vs ->
    Printf.sprintf "(%s%s)"
      (String.concat ", " (List.map2 show_val ts' vs))
      (if List.length vs = 1 then "," else "")
  | T.Array (T.Mut t'), Value.Array a ->
    Printf.sprintf "[var %s]"
      (String.concat ", " (List.map (fun v -> show_val t' !(Value.as_mut v)) (Array.to_list a)))
  | T.Array t', Value.Array a ->
    Printf.sprintf "[%s]"
      (String.concat ", " (List.map (show_val t') (Array.to_list a)))
  | T.Obj (_, fts), Value.Obj fs ->
    Printf.sprintf "{%s}" (String.concat "; " (List.map (show_field fs) fts))
  | T.Variant fs, Value.Variant (l, v) ->
    begin match List.find_opt (fun {T.lab = l'; _} -> l = l') fs with
    | Some {T.typ = T.Tup []; _} -> Printf.sprintf "(#%s)" l
    | Some {T.typ = t'; _} -> Printf.sprintf "(#%s %s)" l (show_val t' v)
    | _ -> assert false
    end
  | _ ->
    Printf.eprintf "show_val: %s : %s\n" (Value.string_of_val 2 v) (T.string_of_typ t);
    assert false

and show_field fs ft =
  let v = Value.Env.find ft.T.lab fs in
  let m, t', v' =
    match ft.T.typ with
    | T.Mut t' -> "var ", t', !(Value.as_mut  v)
    | t' -> "", t', v
  in
  (* With types:
  Printf.sprintf "%s%s : %s = %s" m ft.T.name (T.string_of_typ t') (show_val t' v')
  *)
  Printf.sprintf "%s = %s" ft.T.lab (show_val t' v')
