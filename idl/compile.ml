
open Source

open Syntax_idl
module I = Ir
module T = Typing_idl

module Env = T.Env

type coder = Encode | Decode

let rec find_type env id =
  match Env.find_opt id.it env with
  | Some t ->
     (match t.it with
      | VarT id -> find_type env id
      | _ -> t)
  | None -> assert false
                    
let prim coder env p = fun x ->
  match coder, p with
  | Encode, Nat -> I.Prim (I.WriteLEB x)
  | Decode, Nat -> I.Prim (I.ReadLEB x)
  | Encode, Text ->
     I.Seq [
         I.Let ("len", I.Data (I.Length x));
         I.Prim (I.WriteLEB "len");
         I.Prim (I.WriteByte x);
       ]
  | Decode, Text ->
     I.Seq [
         I.Let ("len", I.Prim (I.ReadLEB x));
         I.Prim (I.ReadByte ("utf8", "len"))
       ]
  | _ -> I.Prim I.Null
  
let rec typ coder env t x =
  match t.it with  
  | PrimT p -> prim coder env p x
  | VarT id ->
     let t' = find_type env id in
     typ coder env t' x
  | FuncT (ms, f1, f2) ->
     I.Seq [
         I.Let ("input", fields coder env f1 x);
         I.Let ("output", fields coder env f2 x)
       ]
  | RecordT fs -> fields coder env fs x
  | _ -> I.Prim I.Null

and fields coder env fs x =
  match coder with
  | Decode ->
     let new_obj = "new_x" in
     let obj_init = I.Let (new_obj, I.Data I.NewRecord) in
     let fs = List.map (fun f ->
                  I.Data (I.SetField(new_obj, f.it.name.it, typ coder env f.it.typ x))) fs
     in I.Seq (obj_init::fs)
  | Encode ->
     let fs = List.map (fun f ->
                  let x' = "x_"^string_of_pos f.at.left in
                  I.Seq [
                      I.Let (x', I.Data (I.GetField (x, f.it.name.it)));
                      typ coder env f.it.typ x']) fs
     in I.Seq fs
       
let meths env ms =
  let methods =
    List.map (fun m ->
        let x = "x" in
        let b = "b" in
        let type_name = String.map (fun c -> if c = ' ' then '_' else c) (Arrange_idl.string_of_typ m.it.meth) in
        let meth_name = m.it.var.it ^ "_" ^ type_name in
        I.Seq [
            I.Let ("encode_"^meth_name, I.Fun (x, typ Encode env m.it.meth x));            
            I.Let ("decode_"^meth_name, I.Fun (b, typ Decode env m.it.meth b));
          ]) ms
  in I.Seq methods
         
let actor env actor_opt =
  match actor_opt with
  | None -> [], I.Prim I.Null
  | Some actor ->
     (match actor.it with
      | ActorD (x, {it=ServT ms; _}) ->
         [], meths env ms
      | _ -> [], I.Prim I.Null
     )
         
let transform (env : T.scope) (prog : prog) : I.prog =
  actor env prog.it.actor 
