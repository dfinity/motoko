open Syntax_idl
open Source

(* Environments *)

module Env = Env.Make(String)

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
  
(* Programs *)
let check_def env dec =
  match dec.it with
  | TypD (id, t) -> Env.singleton id.it t
  | ActorD _ -> Env.empty
                    
let check_defs env decs =
  let _, ve =
    List.fold_left (fun (env, ve) dec ->
        let ve' = check_def env dec in
        adjoin_vals env ve', Env.adjoin ve ve'
      ) (env, Env.empty) decs
  in ve

let check_decs env decs at : scope =
  let scope = check_defs env decs in
  scope
                      
let check_prog scope prog : scope Diag.result =
  Diag.with_message_store
    (fun msgs ->
      recover_opt
        (fun prog ->
          let env = env_of_scope msgs scope in
          let res = check_decs env prog.it prog.at in
          res
        )
        prog
    )
