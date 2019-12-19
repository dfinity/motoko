open Idllib.Syntax
open Source
module M = Mo_types.Type
module I = Idllib.Typing

let m_env = ref M.Env.empty

let check_prim p =
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
  | Float32 -> assert false
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

let is_tuple fs =
  List.length fs > 1 &&
  let fs = List.mapi (fun i f -> (i, f)) fs in
  List.fold_left
    (fun is_tuple (i, f) -> is_tuple &&
       match f.it.label.it with
       | Unnamed id -> Lib.Uint32.to_int id = i
       | _ -> false
    ) true fs

let rec check_typ env t =
  match t.it with
  | PrimT p -> check_prim p
  | VarT {it=id; _} ->
     (match M.Env.find_opt id !m_env with
      | None ->
         let con = Mo_types.Con.fresh id (M.Abs ([], M.Pre)) in
         let res_t = M.Con (con, []) in
         m_env := M.Env.add id res_t !m_env;
         let t' = I.Env.find id env in
         let t' = check_typ env t' in
         M.set_kind con (M.Def ([], t'));
         res_t
      | Some t -> t
     )
  | OptT t -> M.Opt (check_typ env t)
  | VecT t -> M.Array (check_typ env t)
  | RecordT fs ->
     if is_tuple fs then
       M.Tup (List.map (fun f -> check_typ env f.it.typ) fs)
     else
       let fs = List.map (check_field env) fs in
       M.Obj (M.Object, List.sort M.compare_field fs)
  | VariantT fs ->
     let fs = List.map (check_field env) fs in
     M.Variant (List.sort M.compare_field fs)
  | FuncT (ms, ts1, ts2) ->
     let (s, c) = check_modes ms in
     M.Func (M.Shared s, c, [], List.map (check_typ env) ts1, List.map (check_typ env) ts2)
  | ServT ms ->
     let fs = List.map (check_meth env) ms in
     M.Obj (M.Actor, List.sort M.compare_field fs)
  | PreT -> assert false
and check_field env f =
  match f.it.label.it with
  | Named name -> M.{lab = Idllib.Escape.escape name; typ = check_typ env f.it.typ}
  | Id id -> M.{lab = Idllib.Escape.escape_num id; typ = check_typ env f.it.typ}
  | Unnamed id -> M.{lab = Idllib.Escape.escape_num id; typ = check_typ env f.it.typ}
and check_meth env (m: typ_meth) =
  M.{lab = Idllib.Escape.escape m.it.var.it; typ = check_typ env m.it.meth}

let prog (env: typ I.Env.t) actor : M.typ =
  match actor with
  | Some {it=ServT ms; _} ->
     let fs = List.map (check_meth env) ms in
     let fs = M.Env.fold (fun id t fs ->
         match t with
         | M.Con (c, _) -> M.{lab = id; typ = M.Typ c}::fs
         | _ -> assert false) !m_env fs in
     M.Obj (M.Actor, List.sort M.compare_field fs)
  | None -> assert false
  | Some _ ->  assert false
