open Syntax_idl
open Source
open Arrange_idl

(* Environments *)
module FieldEnv = Env_idl.Make(struct type t = Stdint.uint64 let compare = Stdint.Uint64.compare end)   
module Env = Env_idl.Make(String)
module TS = Set.Make(String)           
           
(* Error recovery *)

exception Recover

let recover_with (x : 'a) (f : 'b -> 'a) (y : 'b) = try f y with Recover -> x
let recover_opt f y = recover_with None (fun y -> Some (f y)) y
let recover f y = recover_with () f y
  
(* Scopes *)

type typ_env = typ Env.t

type scope = typ_env

let empty_scope : scope = Env.empty;

(* Contexts (internal) *)

type env =
  { typs : typ_env;
    msgs : Diag.msg_store;
    pre_phase: bool;
  }

let env_of_scope msgs scope =
  { typs = scope;
    msgs;
    pre_phase = true;
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

let adjoin env scope =
  { env with
    typs = Env.adjoin env.typs scope;
  }

let adjoin_typs env te = {env with typs = Env.adjoin env.typs te}

let disjoint_union env at fmt env1 env2 =
  try Env.disjoint_union env1 env2
  with Env.Clash k -> error env at fmt k

(* Types *)

let compare_field (f1: typ_field) (f2: typ_field) = compare f1.it.id f2.it.id
let compare_meth (m1: typ_meth) (m2: typ_meth) = compare m1.it.var m2.it.var
let find_type env id =
  match Env.find_opt id.it env.typs with
  | None -> error env id.at "unbound type identifier %s" id.it
  | Some t -> t
let rec is_func env t =
  match t.it with
  | FuncT _ -> true, t
  | VarT id -> is_func env (find_type env id)
  | _ -> false, t
let is_func env t = if env.pre_phase then true, t else is_func env t
                  
let rec is_serv env t =
  match t.it with
  | ServT _ -> true, t
  | VarT id -> is_serv env (find_type env id)
  | _ -> false, t
let is_serv env t = if env.pre_phase then true, t else is_serv env t
       
let check_cycle env =
  Env.iter (fun x t ->
      let rec has_cycle seen t =
        match t.it with
        | VarT id ->
           TS.mem id.it seen ||
             begin
               let seen = TS.add id.it seen in
               let t' = find_type env id in
               has_cycle seen t'
             end
        | _ -> false
      in
      if has_cycle TS.empty t then error env t.at "%s has a cyclic type definition" x
    ) env.typs
       
let rec check_typ env t =
  match t.it with
  | PrimT prim -> t
  | VarT id ->
     (match Env.find_opt id.it env.typs with
      | None -> error env id.at "unbound type identifier %s" id.it
      | Some t' -> t
     )
  | FuncT (ms, t1, t2) ->
     let t1' = check_fields env t1 in
     let t2' = check_fields env t2 in
     FuncT (ms, t1', t2') @@ t.at
  | OptT t -> OptT (check_typ env t) @@ t.at
  | VecT t -> VecT (check_typ env t) @@ t.at
  | RecordT fs ->
     let fs' = check_fields env fs in
     RecordT (List.sort compare_field fs') @@ t.at
  | VariantT fs ->
     let fs' = check_fields env fs in
     VariantT (List.sort compare_field fs') @@ t.at
  | ServT meths ->
     let ms' = check_meths env meths in
     ServT (List.sort compare_meth ms') @@ t.at
  | PreT -> if env.pre_phase then t else assert false

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
  let t' = check_typ env meth.it.meth in
  match is_func env t' with
  | false, t' ->
     error env meth.it.meth.at "%s is a non-function type\n %s" meth.it.var.it (string_of_typ t');
  | true, _ ->
     {var=meth.it.var; meth=t'} @@ meth.at

and check_meths env meths =
  let _, meths =
    List.fold_left (fun (name_env, meths) meth ->
        if TS.mem meth.it.var.it name_env then
          error env meth.it.var.at "duplicate binding for %s in service" meth.it.var.it
        else
          let meth' = check_meth env meth in
          (TS.add meth.it.var.it name_env, meth'::meths)
      ) (TS.empty, []) meths
  in meths
  
(* Declarations *)
                    
and check_def env dec =
  match dec.it with
  | TypD (id, t) ->
     let t' = check_typ env t in
     Env.singleton id.it t'

and check_actor env actor_opt =
  match actor_opt with
  | None -> Env.empty
  | Some {it=ActorD (id, t); at; _} ->
     (match is_serv env t with
      | false, t' ->
         error env at "%s is a non-service reference type\n %s" (string_of_typ t) (string_of_typ t')
      | true, {it=ServT meths; _} ->
         let meths' = check_meths env meths in
         Env.singleton id.it (ServT (List.sort compare_meth meths') @@ at)
      | true, _ -> assert false
     )
     
and check_defs env decs =
  let _, te =
    List.fold_left (fun (env, te) dec ->
        let te' = check_def env dec in
        adjoin_typs env te', Env.adjoin te te'
      ) (env, Env.empty) decs
  in te

and gather_id dec =
  match dec.it with
  | TypD (id, _) -> id

and gather_decs env decs =
  List.fold_left (fun te dec ->
      let id = gather_id dec in
      let te' = Env.singleton id.it (PreT @@ id.at) in
      disjoint_union env id.at "duplicate binding for %s in type definitions" te te'
    ) env.typs decs

(* Programs *)

let check_prog scope prog : scope Diag.result =
  Diag.with_message_store
    (fun msgs ->
      recover_opt
        (fun prog ->
          let env = env_of_scope msgs scope in          
          let init_scope = gather_decs env prog.it.decs in
          let env = adjoin env init_scope in
          let te = check_defs env prog.it.decs in
          let env = env_of_scope msgs te in
          let _ = check_cycle env in
          let te = check_defs {env with pre_phase = false} prog.it.decs in
          let _ = check_actor (env_of_scope msgs te) prog.it.actor in
          te
        )
        prog
    )
