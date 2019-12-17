open Idllib.Syntax
open Source
module M = Mo_types.Type
module I = Idllib.Typing

let m_env = ref M.Env.empty
let con_set = ref M.ConSet.empty          

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
         con_set := M.ConSet.add con !con_set;
         res_t
      | Some t -> t
     )
  | OptT t -> M.Opt (check_typ env t)
  | VecT t -> M.Array (check_typ env t)
  | FuncT (ms, ts1, ts2) ->
     let (s, c) = check_modes ms in
     M.Func (M.Shared s, c, [], List.map (check_typ env) ts1, List.map (check_typ env) ts2)
  | _ -> assert false

let check_meth env (m: typ_meth) =
  M.{lab = m.it.var.it; typ = check_typ env m.it.meth}

let prog (env: typ I.Env.t) actor : M.typ * Mo_types.Scope.con_env =
  match actor with
  | Some {it=ServT ms; _} ->
     let fs = List.map (check_meth env) ms in
     (M.Obj (M.Actor, fs), !con_set)
  | None -> assert false    
  | Some _ ->  assert false
