open Idllib.Syntax
open Source
open Printf
module M = Mo_types.Type

let prog actor : M.t =
  match actor with
  | Some (ServT _) ->
     M.Obj (M.Actor, [])
  | None -> assert false    
  | Some _ ->  assert false
