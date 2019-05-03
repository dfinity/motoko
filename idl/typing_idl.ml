open Syntax_idl
open Source
open Arrange_idl

(* Environments *)
module FieldEnv = Env_idl.Make(struct type t = Stdint.uint64 let compare = Stdint.Uint64.compare end)   
module Env = Env_idl.Make(String)
           
(* Error recovery *)

exception Recover

let recover_with (x : 'a) (f : 'b -> 'a) (y : 'b) = try f y with Recover -> x
let recover_opt f y = recover_with None (fun y -> Some (f y)) y
let recover f y = recover_with () f y
  
(* Scopes *)

type val_env = typ Env.t

type scope = val_env

let empty_scope : scope = Env.empty;

(* Contexts (internal) *)

type env =
  { vals : val_env;
    msgs : Diag.msg_store;
  }

let env_of_scope msgs scope =
  { vals = scope;
    msgs;
  }

(* Error bookkeeping *)

let type_error at text : Diag.message = Diag.{sev = Diag.Error; at; cat = "type"; text}
let type_warning at text : Diag.message = Diag.{sev = Diag.Warning; at; cat = "type"; text}

let local_error env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_error at s)) fmt
let error env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_error at s); raise Recover) fmt
let warn env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_warning at s)) fmt

(* Context extension *)

let add_val env x t = {env with vals = Env.add x t env.vals}

let adjoin env scope =
  { env with
    vals = Env.adjoin env.vals scope;
  }

let adjoin_vals env ve = {env with vals = Env.adjoin env.vals ve}

let disjoint_union env at fmt env1 env2 =
  try Env.disjoint_union env1 env2
  with Env.Clash k -> error env at fmt k

(* Types *)

let compare_field (f1: typ_field) (f2: typ_field) = compare f1.it.id f2.it.id
let is_record t = match t.it with RecordT _ -> true | _ -> false
let is_func t = match t.it with FuncT _ -> true | _ -> false
let is_serv t = match t.it with ServT _ -> true | _ -> false
let is_pre t = match t.it with PreT -> true | _ -> false

let rec check_typ env t =
  match t.it with
  | PrimT prim -> t
  | VarT id ->
     (match Env.find_opt id.it env.vals with
      | None ->
         error env id.at "unbound type identifier %s" id.it;
      | Some t' -> if is_pre t' then t else t'
     )
  | FuncT (ms, t1, t2) ->
     let t1' = check_typ env t1 in
     let t2' = check_typ env t2 in
     let modes' = List.map (fun m -> m.it) ms in
     if List.mem Pure modes' && List.mem Updatable modes' then
       error env (List.hd ms).at "function mode cannot be pure and update at the same time";
     if not (is_record t1') then
       error env t1.at "function has non-record parameter type\n %s" (string_of_typ t1');
     if not (is_record t2') then
       error env t2.at "function has non-record result type\n %s" (string_of_typ t2');     
     FuncT (ms, t1', t2') @@ t.at
  | TupT ts -> TupT (List.map (check_typ env) ts) @@ t.at
  | OptT t -> OptT (check_typ env t) @@ t.at
  | VecT t -> VecT (check_typ env t) @@ t.at
  | RecordT fs ->
     let fs' = check_fields env fs in
     RecordT (List.sort compare_field fs') @@ t.at
  | VariantT fs ->
     let fs' = check_fields env fs in
     VariantT (List.sort compare_field fs') @@ t.at
  | ServT meths -> ServT (List.map (check_meth env) meths) @@ t.at
  | PreT -> assert false

and check_fields env fs =
  let _, fields =
    List.fold_left (fun (fenv, fields) f ->
        match FieldEnv.find_opt f.it.id fenv with
        | Some name' ->
           error env f.it.name.at "field name %s hash collision with field %s" f.it.name.it name'
        | None ->
           let t' = check_typ env f.it.typ in
           let f' = {id=f.it.id; name=f.it.name; typ=t'} @@ f.at in
           FieldEnv.disjoint_add f.it.id f.it.name.it fenv, f'::fields
      ) (FieldEnv.empty, []) fs
  in fields

and check_meth env meth =
  let t' = check_typ env meth.it.bound in
  if not (is_func t') then
    error env meth.it.bound.at "%s is a non-function type\n %s" meth.it.var.it (string_of_typ t');
  {var=meth.it.var; bound=t'} @@ meth.at
  
(* Declarations *)
                    
and check_def env dec =
  match dec.it with
  | TypD (id, t) ->
     let t' = check_typ env t in
     Env.singleton id.it t'
  | ActorD (id, meth_list) ->
     let sigs = List.map (check_meth env) meth_list in
     Env.singleton id.it (ServT sigs @@ dec.at)
  | ActorVarD (id, var) ->
     (match Env.find_opt var.it env.vals with
      | None -> error env var.at "unbound service reference type %s" var.it
      | Some t ->
         if not (is_serv t) then
           error env var.at "%s is a non-service reference type\n %s" var.it (string_of_typ t);
         Env.singleton id.it t)
                    
and check_defs env decs =
  let _, ve =
    List.fold_left (fun (env, ve) dec ->
        let ve' = check_def env dec in
        adjoin_vals env ve', Env.adjoin ve ve'
      ) (env, Env.empty) decs.it
  in ve

and gather_id dec =
  match dec.it with
  | TypD (id, _) -> id
  | ActorD (id, _) -> id
  | ActorVarD (id, _) -> id

and gather_decs decs =
  List.fold_left (fun ve dec ->
      let id = gather_id dec in
      let ve' = Env.singleton id.it (PreT @@ id.at) in
      Env.adjoin ve ve'
    ) Env.empty decs.it

(* Programs *)

let check_prog scope prog : scope Diag.result =
  Diag.with_message_store
    (fun msgs ->
      recover_opt
        (fun prog ->
          let init_scope = gather_decs prog in
          let scope = Env.adjoin scope init_scope in
          let env = env_of_scope msgs scope in
          let res = check_defs env prog in
          res
        )
        prog
    )
