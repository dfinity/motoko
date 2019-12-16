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

let rec check_typ env t =
  match t.it with
  | PrimT p -> check_prim p
  | VarT id ->
     (match M.Env.find_opt id.it !m_env with
      | None ->
         m_env := M.Env.add id.it M.Pre !m_env;
         let t' = I.Env.find id.it env in
         let t' = check_typ env t' in
         let con = Mo_types.Con.fresh (id.it) (M.Def ([], t')) in
         let t' = M.Con (con, []) in
         m_env := M.Env.add id.it t' !m_env;
         t'
      | Some t -> t
     )
  | OptT t -> M.Opt (check_typ env t)
  | VecT t -> M.Array (check_typ env t)
  | _ -> assert false

let check_meth env (m: typ_meth) =
  M.{lab = m.it.var.it; typ = check_typ env m.it.meth}

let prog (env: typ I.Env.t) actor : M.typ * M.typ M.Env.t =
  match actor with
  | Some (ServT ms) ->
     let fs = List.map (check_meth env) ms in
     (M.Obj (M.Actor, fs), !m_env)
  | None -> assert false    
  | Some _ ->  assert false
