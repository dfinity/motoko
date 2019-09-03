open Stdio.In_channel
open Lazy

(* reading at byte-level *)
let epsilon : unit -> unit = ignore

(* reading at byte-level *)

let read_byte () : int =
  let b = input_byte stdin in
  match b with
  | Some b -> b
  | None -> failwith "EOF"

let read_signed_byte () : bool * int =
  let b = read_byte () in
  if b > 127 then true, b - 128 else false, b

let read_known char : unit = (* TODO: use read_byte *)
  let b = input_byte stdin in
  match b with
  | Some b when b = Char.code char -> ()
  | None -> failwith "EOF"
  | _ -> failwith "unexpected"


let read_leb128 () : int = (* TODO: should be bigint *)
  match read_signed_byte () with
  | (true, n) -> n
  | _ -> failwith "multi-byte LEB128 not implemented yet"

let read_sleb128 () : int = (* TODO: should be bigint *)
  match read_signed_byte () with
  | (true, n) -> if n > 63 then n - 64 else n
  | _ -> failwith "multi-byte SLEB128 not implemented yet"

(* bool: M(b : bool)     = i8(if b then 1 else 0) *)
let read_bool () : bool =
  match read_byte () with
  | 0 -> false
  | 1 -> true
  | _ -> failwith "invalid boolean"

let read_int8 () : int =
  match read_signed_byte () with
  | (true, n) -> n - 128
  | (_, n) -> n

let read_int16 () : int = assert false
let read_int32 () : int = assert false
let read_int64 () : int = assert false

(* Magic *)

let read_magic () : unit =
  read_known 'D';
  read_known 'I';
  read_known 'D';
  read_known 'L'

(* Repetition *)

let read_t_star_ (t : unit -> unit) : unit =
  let rep = read_leb128 () in
  for _ = 1 to rep do
    t ()
  done

let read_t_star (t : unit -> 'a) : 'a array =
  let rep = read_leb128 () in
  Array.init rep (function _ -> t ())


type typ = Null | Bool | Nat | NatN of int
         | Int | IntN of int | Reserved | Empty
         | Opt of typ | Vec of typ | Record of (int * typ) array

(* type index/ground type (negative) *)

let read_type_index = read_sleb128

let read_assoc () = let hash = read_leb128 () in
                    let tynum = read_type_index () in
                    hash, tynum

(* outputters *)
let output_nat int = () (* TODO: output formatted *)
let output_int int = ()
let output_bool b = ()
let output_nil () = ()
let output_some d = ()
let output_byte b = ()
let output_2byte b = ()
let output_4byte b = ()
let output_8byte b = ()
let output_int8 i = ()
let output_int16 i = ()
let output_int32 i = ()
let output_int64 i = ()

(* primitive type

T : <primtype> -> i8*
T(null)     = sleb128(-1)
T(bool)     = sleb128(-2)
T(nat)      = sleb128(-3)
T(int)      = sleb128(-4)
T(nat<N>)   = sleb128(-5 - log2(N/8))
T(int<N>)   = sleb128(-9 - log2(N/8))
T(float<N>) = sleb128(-13 - log2(N/32))
T(text)     = sleb128(-15)
T(reserved) = sleb128(-16)
T(empty)    = sleb128(-17)
 *)
let decode_primitive_type : int -> typ * (unit -> unit) =
  function
  | -1 -> Null, epsilon (* TODO: i8(0) *)
  | -2 -> Bool, (function () -> output_bool (read_bool ()))
  | -3 -> Nat, (function () -> output_nat (read_leb128 ()))
  | -4 -> Int, (function () -> output_int (read_sleb128 ()))
  | -5 -> NatN 8, (function () -> output_byte (read_byte ()))
  | -6 -> NatN 16, (function () -> output_2byte (read_byte ())) (* FIXME *)
  | -7 -> NatN 32, (function () -> output_4byte (read_byte ())) (* FIXME *)
  | -8 -> NatN 64, (function () -> output_8byte (read_byte ())) (* FIXME *)
  | -9 -> IntN 8, (function () -> output_int8 (read_int8 ()))
  | -10 -> IntN 16, (function () -> output_int16 (read_int16 ()))
  | -11 -> IntN 32, (function () -> output_int32 (read_int32 ()))
  | -12 -> IntN 64, (function () -> output_int64 (read_int64 ()))
  | -16 -> Reserved, ignore
  | -17 -> Empty, ignore
  | _ -> failwith "unrecognised primitive type"

(*let type_table = ref (Array.make 0 (Empty, epsilon))*)


(*let lookup_type_index type_table indx = lazy (Array.get !type_table indx) *)

let read_type lookup : (typ * (unit -> unit)) Lazy.t =
  match read_sleb128 () with
  | p when p < 0 -> from_val (decode_primitive_type p)
(*

T(opt <datatype>) = sleb128(-18) I(<datatype>)
T(vec <datatype>) = sleb128(-19) I(<datatype>)
T(record {<fieldtype>^N}) = sleb128(-20) T*(<fieldtype>^N)
T(variant {<fieldtype>^N}) = sleb128(-21) T*(<fieldtype>^N)


*)
  | -18 ->
    begin
      let reader consumer () =
        match read_byte () with
        | 0 -> output_nil ()
        | 1 -> output_some (consumer ())
        | _ -> failwith "invalid optional" in
      match read_type_index () with
           | p when p < 0 -> let t, consumer = decode_primitive_type p in
                             from_val (Opt t, reader consumer)
           | i -> lazy (let lazy (t, consumer) = lookup i in
                        Opt t, reader consumer)
           end
  | -19 -> begin match read_type_index () with
           | p when p < 0 -> let t, consumer = decode_primitive_type p in
                             from_val (Vec t, function () -> read_t_star_ consumer)
           | i -> lazy (let lazy (t, consumer) = lookup i in
                        Vec t, function () -> read_t_star_ consumer)
           end
  | -20 -> let assocs = read_t_star read_assoc in
           let prim_or_lookup = function
             | p when p < 0 -> decode_primitive_type p
             | i -> force (lookup i) in
           lazy (let members = Array.map (function (i, tynum) -> i, fst (prim_or_lookup tynum)) assocs in
                 let consumers = Array.map (function (_, tynum) -> snd (prim_or_lookup tynum)) assocs in
                 Record members, function () -> Array.iter (function f -> f ()) consumers)
  | _ -> failwith "unrecognised structured type"


let read_type_table (t : unit -> (typ * (unit -> unit)) Lazy.t) : (typ * (unit -> unit)) Lazy.t array =
  let rep = read_leb128 () in
  Array.init rep (function _ -> t ())

(* Top-level *)

let top_level () : unit =
  read_magic ();
  let rec tab = lazy (read_type_table (function () -> read_type lookup))
      and lookup = function indx -> Array.get (force tab) indx in
  let tyindx = read_type_index () in
  let lazy (ty, m) = Array.get (force tab) tyindx in
  m ()





