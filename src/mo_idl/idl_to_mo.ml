open Idllib.Syntax
open Idllib.Exception
open Source
module M = Mo_types.Type
module I = Idllib.Typing


let check_prim at p =
  match p with
  | Null -> M.Prim M.Null
  | Bool -> M.Prim M.Bool
  | Int -> M.Prim M.Int
  | Int8 -> M.Prim M.Int8
  | Int16 -> M.Prim M.Int16
  | Int32 -> M.Prim M.Int32
  | Int64 -> M.Prim M.Int64
  | Nat -> M.Prim M.Nat
  | Nat8 -> M.Prim M.Nat8
  | Nat16 -> M.Prim M.Nat16
  | Nat32 -> M.Prim M.Nat32
  | Nat64 -> M.Prim M.Nat64
  | Float32 -> raise (UnsupportedCandidFeature
     (Diag.error_message at "M0161" "import"
       "Candid 'float32' type cannot be imported as a Motoko type"))
  | Float64 -> M.Prim M.Float
  | Text -> M.Prim M.Text
  | Reserved -> M.Any
  | Empty -> M.Non

let check_modes ms =
  match ms with
  | [] -> (M.Write, M.Promises)
  | [{it=Oneway; _}] -> (M.Write, M.Returns)
  | [{it=Query; _}] -> (M.Query, M.Promises)
  | _ -> assert false

let check_label lab : M.lab =
  match lab.it with
  | Named name -> Idllib.Escape.escape name
  | Id id -> Idllib.Escape.escape_num id
  | Unnamed id -> Idllib.Escape.escape_num id

let is_tuple fs =
  List.length fs > 1 &&
    let fs = List.mapi (fun i f -> (i, f)) fs in
    List.for_all (fun (i, f) ->
        match f.it.label.it with
        | Unnamed id -> Lib.Uint32.to_int id = i
        | _ -> false) fs

let rec check_typ' env occs t =
  match t.it with
  | PrimT p -> check_prim t.at p
  | PrincipalT -> M.Prim M.Principal
  | VarT {it=id; _} ->
     (match M.Env.find_opt id !occs with
      | None ->
         let con = Mo_types.Cons.fresh id (M.Abs ([], M.Pre)) in
         let res_t = M.Con (con, []) in
         occs := M.Env.add id res_t !occs;
         let t' = I.Env.find id env in
         let t' = check_typ' env occs t' in
         M.set_kind con (M.Def ([], t'));
         res_t
      | Some t -> t
     )
  | OptT t -> M.Opt (check_typ' env occs t)
  | VecT t -> M.Array (check_typ' env occs t)
  | BlobT -> M.Prim M.Blob
  | RecordT fs ->
     if is_tuple fs then
       M.Tup (List.map (fun f -> check_typ' env occs f.it.typ) fs)
     else
       let fs = List.map (check_field env occs) fs in
       M.Obj (M.Object, List.sort M.compare_field fs)
  | VariantT fs ->
     let fs = List.map (check_variant_field env occs) fs in
     M.Variant (List.sort M.compare_field fs)
  | FuncT (ms, ts1, ts2) ->
     let (s, c) = check_modes ms in
     M.Func (M.Shared s, c, [M.scope_bind], check_typs' env occs ts1, check_typs' env occs ts2)
  | ServT ms ->
     let fs = List.map (check_meth env occs) ms in
     M.Obj (M.Actor, List.sort M.compare_field fs)
  | ClassT _ -> raise (UnsupportedCandidFeature
     (Diag.error_message t.at "M0162" "import" "Candid service constructor type not supported as Motoko type"))
  | PreT -> assert false
and check_typs' env occs ts = List.map (check_typ' env occs) ts
and check_field env occs f =
  M.{lab = check_label f.it.label; typ = check_typ' env occs f.it.typ; depr = None}
and check_variant_field env occs f =
  match f.it.typ.it with
  | PrimT Null -> M.{lab = check_label f.it.label; typ = M.Tup []; depr = None}
  | _ -> check_field env occs f
and check_meth env occs (m: typ_meth) =
  M.{lab = Idllib.Escape.escape_method m.it.var.at m.it.var.it; typ = check_typ' env occs m.it.meth; depr = None}

let check_prog (env: typ I.Env.t) actor : M.typ =
  let occs = ref M.Env.empty in
  match actor with
  | Some {it=ServT ms; _} ->
     let fs = List.map (check_meth env occs) ms in
     (* TODO: why do we only check and include the mentioned types (occs),
        and not all of the .did declared ones (available to the caller, if not here? *)
     let fs = M.Env.fold (fun id t fs ->
       match t with
       | M.Con (c, _) ->
          (* TODO: consider adding deprecation as types can disappear even
             across compatible .dids *)
          M.{lab = id; typ = M.Typ c; depr = None}::fs
       | _ -> assert false) !occs fs in
     M.Obj (M.Actor, List.sort M.compare_field fs)
  | Some {it=ClassT _; at; _} ->
     raise (UnsupportedCandidFeature
       (Diag.error_message at "M0163" "import" "cannot import a Candid service constructor"))
  | None -> assert false
  | _ -> assert false

let check_typ env t = check_typ' env (ref M.Env.empty) t
let check_typs env t = check_typs' env (ref M.Env.empty) t
