(*
This module provides some convenience to assemble WASM instruction lists. The
features are

 * O(1) concatenation (using difference list internally)
 * Managing of label depths.
*)

open Wasm.Types
open Wasm.Ast
open Wasm.Source

(* When we do not care about the generate source region *)
let nr x = x @@ Wasm.Source.no_region

(* The main type of this module *)
type t = int32 -> instr list -> instr list

let to_instr_list (is : t) : instr list =
  is 0l []

(* The concatenation operator *)
let (^^) (is1 : t) (is2 : t) : t = fun d rest -> is1 d (is2 d rest)

let nop : t = fun _ rest -> rest

(* Singletons *)
let i (instr : instr) : t = fun _ rest -> instr :: rest
let i_ (instr : instr') = i (instr @@ Wasm.Source.no_region)

(* Depths-managing combinators *)

let if_ (ty : stack_type) (thn : t) (els : t) : t =
  fun d rest ->
    nr (If (ty, thn Int32.(add d 1l) [], els Int32.(add d 1l) [])) :: rest

let block_ (ty : stack_type) (body : t) : t =
  fun d rest ->
    nr (Block (ty, body Int32.(add d 1l) [])) :: rest

let loop_ (ty : stack_type) (body : t) : t =
  fun d rest ->
    nr (Loop (ty, body Int32.(add d 1l) [])) :: rest

(* Remember depth *)
type depth = int32 Lib.Promise.t

let with_currrent_depth (k : depth -> t) : t =
  let p = Lib.Promise.make () in
  k p

let branch_to_ (p : depth) : t =
  fun d rest ->
    nr (Br (nr Int32.(sub d (Lib.Promise.value p)))) :: rest


(* map and concat *)
let concat_map f xs = List.fold_right (^^) (List.map f xs) nop
let concat_mapi f xs = List.fold_right (^^) (List.mapi f xs) nop

