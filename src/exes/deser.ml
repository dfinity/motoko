open Stdio.In_channel
open Lazy

(* read nothing *)

let epsilon : unit -> unit = ignore

(* reading at byte-level *)

let read_byte () : int =
  let b = input_byte stdin in
  match b with
  | Some b -> b
  | None -> failwith "EOF"

let read_2byte () : int =
  let lsb = read_byte () in
  let msb = read_byte () in
  msb * 256 + lsb

let read_4byte () : int =
  let lsb = read_2byte () in
  let msb = read_2byte () in
  msb * 65536 + lsb

let read_8byte () : int = (* TODO: should be bigint *)
  let lsb = read_4byte () in
  let msb = read_4byte () in
  msb * 4294967296 + lsb

let read_signed_byte () : bool * int =
  let b = read_byte () in
  if b > 127 then true, b - 128 else false, b

let read_known char : unit =
  match read_byte () with
  | b when b = Char.code char -> ()
  | _ -> failwith "unexpected"

(* reading numbers *)

let read_leb128 () : int = (* TODO: should be bigint *)
  let rec leb128 w : int =
  match read_signed_byte () with
  | (true, n) -> w * n + leb128 (w * 128)
  | (_, n) -> w * n in
  leb128 1

let read_sleb128 () : int = (* TODO: should be bigint *)
  let rec sleb128 w : int =
    match read_signed_byte () with
    | (true, n) -> w * n + sleb128 (w * 128)
    | (_, n) -> w * if n > 63 then n - 128 else n in
  sleb128 1

let read_int8 () : int =
  match read_signed_byte () with
  | (true, n) -> n - 128
  | (_, n) -> n

let read_int16 () : int =
  let lsb = read_byte () in
  let msb = read_int8 () in
  msb * 256 + lsb

let read_int32 () : int =
  let lsb = read_2byte () in
  let msb = read_int16 () in
  msb * 65536 + lsb

let read_int64 () : int = (* TODO: should be bigint *)
  let lsb = read_4byte () in
  let msb = read_int32 () in
  msb * 4294967296 + lsb

(* bool: M(b : bool)     = i8(if b then 1 else 0) *)
let read_bool () : bool =
  match read_byte () with
  | 0 -> false
  | 1 -> true
  | _ -> failwith "invalid boolean"

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

(* Annotations *)

type ann = Pure | Oneway

let read_annotation () : ann =
  match read_byte () with
  | 1 -> Pure
  | 2 -> Oneway
  | _ -> failwith "invalid annotation"

type typ = Null | Bool | Nat | NatN of int
         | Int | IntN of int | Text | Reserved | Empty
         | Opt of typ | Vec of typ
         | Record of (int * typ) array
         | Variant of (int * typ) array
         | Function of (int * typ) array * (int * typ) array * ann array

(* type index/ground type (negative) *)

let read_type_index = read_sleb128

let read_assoc () = let hash = read_leb128 () in
                    let tynum = read_type_index () in
                    Printf.printf "hash: %d, tynum: %d\n" hash tynum; hash, tynum

(* outputters *)
let output_nat nat = Printf.printf "output_nat: %d\n" nat
let output_int int = Printf.printf "output_int: %d\n" int
let output_bool b = Printf.printf "output_bool: %s\n" (if b then "true" else "false")
let output_nil () = Printf.printf "null (0 bytes)"
let output_some consumer = Printf.printf "Some: value follows on the next line\n"; consumer ()
let output_byte b = ()
let output_2byte b = ()
let output_4byte b = ()
let output_8byte b = ()
let output_int8 i = ()
let output_int16 i = ()
let output_int32 i = ()
let output_int64 i = ()
let output_text bytes from tostream =
      let buf = Buffer.create 0 in
      let text = input_buffer from buf ~len:bytes in
      Printf.printf "Text: %d bytes follow on next line\n" bytes;
      Stdio.Out_channel.output_buffer tostream buf;
      Printf.printf "\n"

let read_type lookup : (typ * (unit -> unit)) Lazy.t =
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
    | -1 -> Null, output_nil
    | -2 -> Bool, (function () -> output_bool (read_bool ()))
    | -3 -> Nat, (function () -> output_nat (read_leb128 ()))
    | -4 -> Int, (function () -> output_int (read_sleb128 ()))
    | -5 -> NatN 8, (function () -> output_byte (read_byte ()))
    | -6 -> NatN 16, (function () -> output_2byte (read_2byte ()))
    | -7 -> NatN 32, (function () -> output_4byte (read_4byte ()))
    | -8 -> NatN 64, (function () -> output_8byte (read_8byte ()))
    | -9 -> IntN 8, (function () -> output_int8 (read_int8 ()))
    | -10 -> IntN 16, (function () -> output_int16 (read_int16 ()))
    | -11 -> IntN 32, (function () -> output_int32 (read_int32 ()))
    | -12 -> IntN 64, (function () -> output_int64 (read_int64 ()))
    | -13 | -14 -> failwith "no floats yet" (* TODO *)
    | -15 -> Text, (function () -> let len = read_leb128 () in output_text len stdin stdout)
    | -16 -> Reserved, ignore
    | -17 -> Empty, ignore
    | _ -> failwith "unrecognised primitive type" in

  let prim_or_lookup = function
    | p when p < 0 -> decode_primitive_type p
    | i -> force (lookup i) in

  match read_sleb128 () with
  | p when p < 0 && p > -18 -> from_val (decode_primitive_type p)
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
        | 1 -> output_some consumer
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
           lazy (let members = Array.map (function (i, tynum) -> i, fst (prim_or_lookup tynum)) assocs in
                 let consumers = Array.map (function (_, tynum) -> snd (prim_or_lookup tynum)) assocs in
                 Record members, function () -> Array.iter (function f -> f ()) consumers)
  | -21 -> let assocs = read_t_star read_assoc in
           lazy (let alts = Array.map (function (i, tynum) -> i, fst (prim_or_lookup tynum)) assocs in
                 let consumers = Array.map (function (_, tynum) -> snd (prim_or_lookup tynum)) assocs in
                 Variant alts, function () -> let i = read_leb128 () in Array.get consumers i ())
(*
T(func (<fieldtype1>* ) -> (<fieldtype2>* ) <funcann>* ) =
  sleb128(-22) T*(<fieldtype1>* ) T*(<fieldtype2>* ) T*(<funcann>* )
T(service {<methtype>*}) =
  sleb128(-23) T*(<methtype>* )
                   *)
  | -22 -> let assocs1 = read_t_star read_assoc in
           let assocs2 = read_t_star read_assoc in
           let anns = read_t_star read_annotation in
           lazy (let args = Array.map (function (i, tynum) -> i, fst (prim_or_lookup tynum)) assocs1 in
                 let rslts = Array.map (function (i, tynum) -> i, fst (prim_or_lookup tynum)) assocs2 in
                 Function (args, rslts, anns), epsilon)

  | t -> failwith (Printf.sprintf "unrecognised structured type: %d" t)


let read_type_table (t : unit -> (typ * (unit -> unit)) Lazy.t) : (typ * (unit -> unit)) Lazy.t array =
  let rep = read_leb128 () in
  Array.init rep (function i -> Printf.printf "read_type_table: %d\n" i;t ())

(* Top-level *)

type mode = Legacy | Default

let top_level md : unit =
  Printf.printf "DESER, to your service!\n";
  read_magic ();
  let rec tab' = lazy (read_type_table (function () -> read_type lookup))
      and lookup = function indx -> Array.get (force tab') indx in
  let tab = force tab' in
  match md with
  | Default ->
    let argtys = read_t_star read_type_index in
    Printf.printf "ARGS: %d\n" (Array.length argtys);
    let consumers = Array.map (function tynum -> let lazy (ty, m) = Array.get tab tynum in m) argtys in
    Array.iter (function f -> f ()) consumers
  | Legacy ->
    let argty = read_type_index () in
    Printf.printf "ARGTY: %d\n" argty;
    snd (force (Array.get tab argty)) ()

(* CLI *)

let name = "deser"
let version = "0.1"
let banner = "Interface Description Language (IDL) " ^ version ^ " message dumper"
let usage = "Usage: " ^ name ^ " [option] [file ...]"

let mode = ref Default

let set_mode m () =
  if !mode <> Default then begin
    Printf.eprintf "deser: multiple execution modes specified"; exit 1
  end;
  mode := m

let add_arg source = () (* args := !args @ [source] *)

let argspec = Arg.align
[
  "--legacy", Arg.Unit (set_mode Legacy), " decode legacy message API";
  "--version",
    Arg.Unit (fun () -> Printf.printf "%s\n" banner; exit 0), " show version";
]

(* run it *)

let () =
  Arg.parse argspec add_arg usage;
  top_level !mode;
  match input_byte stdin with
  | Some _ -> failwith "surplus bytes in input"
  | None -> ()
