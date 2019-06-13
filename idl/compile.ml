
open Source

open Syntax_idl
module I = Ir
module T = Typing_idl

module Env = T.Env

type coder = Encode | Decode

let invalid s = raise (Invalid_argument ("Compile."^s))
let todo = I.Var "TODO"              
                    
let rec find_type env id =
  match Env.find_opt id.it env with
  | Some t ->
     (match t.it with
      | VarT id -> find_type env id
      | _ -> t)
  | None -> invalid "find_type"

let rec as_func env t =
  match t.it with
  | FuncT (ms, args, rets) -> ms, args, rets
  | VarT id -> as_func env (find_type env id)
  | _ -> invalid "as_func"

let prim coder env p = fun x ->
  match coder, p with
  | Encode, Nat -> I.Prim (I.WriteLEB x)
  | Decode, Nat -> I.Let (x, I.Prim (I.ReadLEB))
  | Encode, Text ->
     I.Seq [
         I.Let ("len", I.Data (I.StrLength x));
         I.Prim (I.WriteLEB "len");
         I.Prim (I.WriteByte x);
       ]
  | Decode, Text ->
     I.Seq [
         I.Let ("len", I.Prim (I.ReadLEB));
         I.Let (x, I.Prim (I.ReadByte ("utf8", "len")))
       ]
  | _ -> todo
  
let rec typ coder env t x =
  match coder, t.it with  
  | _, PrimT p -> prim coder env p x
  | _, VarT id ->
     let t' = find_type env id in
     typ coder env t' x
  | Encode, OptT t ->
     I.If (I.BinOp (I.Eq, I.Var x, I.Data I.Null),
         I.Prim (I.WriteByte "'00'"),
         I.Seq [I.Prim (I.WriteByte "'01'");
                typ Encode env t x]
       )
  | Decode, OptT t ->
     I.Seq [
         I.Let ("isnull", I.Prim (I.ReadByte ("hex", "1")));
         I.If (I.BinOp (I.Eq, I.Var "isnull", I.Var "'00'"),
               I.Let (x, I.Data I.Null),
               typ Decode env t x)
       ]
  | Encode, VecT t ->
     let x' = "x_" ^ string_of_pos t.at.left in
     I.Seq [
         I.Let ("len", I.Data (I.VecLength x));
         I.Prim (I.WriteLEB "len");
         I.Data (I.GetItem (x, x', typ Encode env t x'))
       ]
  | Decode, VecT t ->
     let x' = "x_" ^ string_of_pos t.at.left in
     let len = "len_" ^ string_of_pos t.at.left in
     I.Seq [
         I.Let (len, I.Prim I.ReadLEB);
         I.Data (I.NewVec (x, len,
                           I.Seq [
                               typ Decode env t x';
                               I.Data (I.PushItem (x, x'))
                             ]
           ))
       ]
  | _, FuncT (ms, f1, f2) -> I.Var "TODO: Func ref"
  | _, RecordT fs -> fields coder env fs x
  | _ -> todo

and fields coder env fs x =
  match coder with
  | Decode ->
     let new_obj = "new_record" in
     let obj_init = I.Let (new_obj, I.Data I.NewRecord) in
     let fs = List.map (fun f ->
                  let x' = "x_"^string_of_pos f.at.left in
                  I.Seq [
                      typ coder env f.it.typ x';
                      I.Data (I.SetField(new_obj, f.it.name.it, x'))
                    ]) fs
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
        let type_name t =
          String.map (fun c -> if c = ' ' then '_' else c)
            (Arrange_idl.string_of_typ t) in
        let (_, args, rets) = as_func env m.it.meth in
        I.Block (m.it.var.it ^ type_name m.it.meth,
                 I.Seq [
                     I.Let ("encode_args", I.Fun (x, fields Encode env args x));
                     I.Let ("decode_args", I.Fun (b, fields Decode env args b));
                     I.Let ("encode_rets", I.Fun (x, fields Encode env rets x));
                     I.Let ("decode_rets", I.Fun (b, fields Decode env rets b));
          ])
      ) ms
  in I.Seq methods
         
let actor env actor_opt =
  match actor_opt with
  | None -> [], todo
  | Some actor ->
     (match actor.it with
      | ActorD (x, {it=ServT ms; _}) ->
         [], meths env ms
      | _ -> [], todo
     )
         
let transform (env : T.scope) (prog : prog) : I.prog =
  actor env prog.it.actor 
