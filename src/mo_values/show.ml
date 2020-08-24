module T = Mo_types.Type

(* Entry point for type checking: *)

let can_show t =
  let open T in
  let seen = ref T.S.empty in
  let rec go t =
    S.mem t !seen ||
    begin
      seen := S.add t !seen;
      match normalize t with
      | Prim (Bool|Nat|Int|Text|Blob|Char|Null|Principal) -> true
      | Prim (Nat8|Int8|Word8)
      | Prim (Nat16|Int16|Word16)
      | Prim (Nat32|Int32|Word32)
      | Prim (Nat64|Int64|Word64) -> true
      | Prim Float -> true
      | Tup ts' -> List.for_all go ts'
      | Opt t' -> go t'
      | Array t' -> go (as_immut t')
      | Obj (Object, fs) ->
        List.for_all (fun f -> go (as_immut f.typ)) fs
      | Variant cts ->
        List.for_all (fun f -> go f.typ) cts
      | Non -> true
      | _ -> false
    end
  in go t

(* Entry point for the interpreter (reference implementation) *)

let needs_parens s = s.[0] = '+' || s.[0] = '-' || s.[0] = '?' || s.[0] = '#'
let parens s = if needs_parens s then "(" ^ s ^ ")" else s
let sign b s = (if b then "+" else "") ^ s

let rec show_val t v =
  let t = T.normalize t in
  match t, v with
  | T.(Prim Bool), Value.Bool b -> if b then "true" else "false"
  | T.(Prim Nat), Value.Int i -> Value.Int.to_string i
  | T.(Prim Nat8), Value.Nat8 i -> Value.Nat8.to_string i
  | T.(Prim Nat16), Value.Nat16 i -> Value.Nat16.to_string i
  | T.(Prim Nat32), Value.Nat32 i -> Value.Nat32.to_string i
  | T.(Prim Nat64), Value.Nat64 i -> Value.Nat64.to_string i
  | T.(Prim Int), Value.Int i -> Value.Int.(sign (gt i zero) (to_string i))
  | T.(Prim Int8), Value.Int8 i -> Value.Int_8.(sign (gt i zero) (to_string i))
  | T.(Prim Int16), Value.Int16 i -> Value.Int_16.(sign (gt i zero) (to_string i))
  | T.(Prim Int32), Value.Int32 i -> Value.Int_32.(sign (gt i zero) (to_string i))
  | T.(Prim Int64), Value.Int64 i -> Value.Int_64.(sign (gt i zero) (to_string i))
  | T.(Prim Word8), Value.Word8 i -> "0x" ^ Value.Word8.to_string i
  | T.(Prim Word16), Value.Word16 i -> "0x" ^ Value.Word16.to_string i
  | T.(Prim Word32), Value.Word32 i -> "0x" ^ Value.Word32.to_string i
  | T.(Prim Word64), Value.Word64 i -> "0x" ^ Value.Word64.to_string i
  | T.(Prim Float), Value.Float i -> Value.Float.to_string i
  | T.(Prim Text), Value.Text s -> "\"" ^ s ^ "\""
  | T.(Prim Blob), Value.Blob s -> "\"" ^ Value.Blob.escape s ^ "\""
  | T.(Prim Char), Value.Char c -> "\'" ^ Wasm.Utf8.encode [c] ^ "\'"
  | T.(Prim Principal), Value.Blob s -> Ic.Url.encode_principal s
  | T.(Prim Null), Value.Null -> "null"
  | T.Opt _, Value.Null -> "null"
  | T.Opt t', Value.Opt v -> "?" ^ parens (show_val t' v)
  | T.Tup ts', Value.Tup vs ->
    Printf.sprintf "(%s%s)"
      (String.concat ", " (List.map2 show_val ts' vs))
      (if List.length vs = 1 then "," else "")
  | T.Array (T.Mut t'), Value.Array a ->
    if a = [||] then "[var]" else
    Printf.sprintf "[var %s]"
      (String.concat ", " (List.map (fun v -> show_val t' !(Value.as_mut v)) (Array.to_list a)))
  | T.Array t', Value.Array a ->
    Printf.sprintf "[%s]"
      (String.concat ", " (List.map (show_val t') (Array.to_list a)))
  | T.Obj (_, fts), Value.Obj fs ->
    Printf.sprintf ".{%s}" (String.concat "; " (List.map (show_field fs) fts))
  | T.Variant fs, Value.Variant (l, v) ->
    begin match List.find_opt (fun {T.lab = l'; _} -> l = l') fs with
    | Some {T.typ = T.Tup []; _} -> Printf.sprintf "#%s" l
    | Some {T.typ = T.Tup _ as t'; _} -> Printf.sprintf "#%s%s" l (show_val t' v)
    | Some {T.typ = t'; _} -> Printf.sprintf "#%s(%s)" l (show_val t' v)
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
