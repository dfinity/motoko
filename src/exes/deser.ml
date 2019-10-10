open Stdio.In_channel
open Lazy

type outputter = unit -> unit

(* read nothing *)

let epsilon : outputter = ignore

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

let read_star_heralding a (heralder : int -> outputter * ('a -> int -> outputter -> outputter)) (t : outputter) : unit =
  let rep = read_leb128 () in
  let herald_vector, herald_member = heralder rep in
  herald_vector ();
  for i = 0 to rep - 1 do
    herald_member a i t ()
  done

let read_t_star (t : unit -> 'a) : 'a array =
  let rep = read_leb128 () in
  Array.init rep (fun _ -> t ())

(* Annotations *)

type ann = Pure | Oneway

let read_annotation () : ann =
  match read_byte () with
  | 1 -> Pure
  | 2 -> Oneway
  | _ -> failwith "invalid annotation"

type typ = Null | Bool | Nat | NatN of int
         | Int | IntN of int | Text | Reserved | Empty
         | Opt of typ Lazy.t | Vec of typ Lazy.t
         | Record of fields
         | Variant of alts
         | Function of (int * typ) array * (int * typ) array * ann array

and alts = (int * typ Lazy.t) array
and fields = (int * typ Lazy.t) array

(* type index/ground type (negative) *)

let read_type_index = read_sleb128

let read_assoc () = let hash = read_leb128 () in
                    let tynum = read_type_index () in
                    assert (tynum > -18);
                    Printf.printf "hash: %d, tynum: %d\n" hash tynum; hash, tynum

module type Dump =
sig
val output_nat : int -> unit
val output_int : int -> unit
val output_bool : bool -> unit
val output_nil : outputter
val output_byte : int -> unit
val output_2byte : int -> unit
val output_4byte : int -> unit
val output_8byte : int -> unit
val output_int8 : int -> unit
val output_int16 : int -> unit
val output_int32 : int -> unit
val output_int64 : int -> unit
val output_text : int -> Stdio.In_channel.t -> Stdio.Out_channel.t -> unit
val output_some : outputter -> unit
val output_arguments : int -> outputter * (unit -> int -> outputter -> outputter)
val output_vector : int -> outputter * (unit -> int -> outputter -> outputter)
val output_record : int -> outputter * (fields -> int -> outputter -> outputter)
val output_variant : int -> outputter * (alts -> int -> outputter -> outputter)
end


module OutputVerbatim : Dump = struct

(* indentation *)

let indent_amount : int = 4
let indentation : int ref = ref 0
let continue_line : bool ref = ref false

let indent () = indentation := !indentation + indent_amount
let outdent () = indentation := !indentation - indent_amount
let ind i = if i = 0 then indent ()
let outd max i = if i + 1 = max then outdent ()
let bracket max g p i f () = ind i; g p i f; outd max i

let fill () = if !continue_line then (continue_line := false; "") else String.make !indentation ' '

let output_string what (s : string) = Printf.printf "%s%s: %s\n" (fill ()) what s
let output_decimal what (i : int) = Printf.printf "%s%s: %d\n" (fill ()) what i

(* outputters *)
let output_nat nat = output_decimal "output_nat" nat
let output_int int = output_decimal "output_int" int
let output_bool b = output_string "output_bool" (if b then "true" else "false")
let output_nil () = Printf.printf "%snull (0 bytes)\n" (fill ())
let output_some consumer = Printf.printf "%sSome: value follows on the next line\n"  (fill ()); consumer ()
let output_byte b = output_decimal "output_byte" b
let output_2byte b = output_decimal "output_2byte" b
let output_4byte b = output_decimal "output_4byte" b
let output_8byte b = output_decimal "output_8byte" b
let output_int8 i = output_decimal "output_int8" i
let output_int16 i = output_decimal "output_int16" i
let output_int32 i = output_decimal "output_int32" i
let output_int64 i = output_decimal "output_int64" i
let output_text bytes from tostream =
      let buf = Buffer.create 0 in
      ignore (input_buffer from buf ~len:bytes);
      Printf.printf "%sText: %d bytes follow on next line\n" (fill ()) bytes;
      Printf.printf "%s---->" (fill ()); (* TODO: puts? *)
      Stdio.Out_channel.output_buffer tostream buf;
      Printf.printf "\n"

let output_arguments args : outputter * (unit -> int -> outputter -> outputter) =
  let herald_arguments = function
    | () when args = 0 -> Printf.printf "%sNo arguments...\n" (fill ())
    | _ when args = 1 -> Printf.printf "%s1 argument follows\n" (fill ())
    | _ -> Printf.printf "%s%d arguments follow\n" (fill ()) args in
  let herald_member () i f = Printf.printf "%sArgument #%d%s: " (fill ()) i (if i + 1 = args then " (last)" else ""); continue_line := true; f () in
  herald_arguments, bracket args herald_member

let output_vector members : outputter * (unit -> int -> outputter -> outputter) =
  let herald_vector () = if members = 0 then Printf.printf "%sEmpty Vector\n" (fill ())
                         else Printf.printf "%sVector with %d members follows\n" (fill ()) members in
  let herald_member () i f = Printf.printf "%sVector member %d%s: " (fill ()) i (if i + 1 = members then " (last)" else ""); continue_line := true; f () in
  herald_vector, bracket members herald_member

let output_record members : outputter * (fields -> int -> outputter -> outputter) =
  let herald_record () = if members = 0 then Printf.printf "%sEmpty Record\n" (fill ())
                         else Printf.printf "%sRecord with %d members follows\n" (fill ()) members in
  let herald_member fields i f = Printf.printf "%sRecord member %d%s: " (fill ()) (fst (Array.get fields i)) (if i + 1 = members then " (last)" else ""); continue_line := true; f () in
  herald_record, bracket members herald_member

let output_variant members : outputter * (alts -> int -> outputter -> outputter) =
  let herald_variant () = assert (members <> 0);
                          Printf.printf "%sVariant with %d members follows\n" (fill ()) members in
  let herald_member alts i f () = indent (); Printf.printf "%sVariant member %d: " (fill ()) (fst (Array.get alts i)); continue_line := true; f (); outdent () in
  herald_variant, herald_member

end

module OutputIdl : Dump = struct

let output_string (s : string) = print_string s
let output_string_space (s : string) = Printf.printf "%s " s
let output_decimal (i : int) = Printf.printf "%d" i

let output_bool b = output_string (if b then "true" else "false")
let output_nil () = output_string "null"
let output_some consumer = output_string_space "opt"; consumer ()
let output_byte = output_decimal
let output_2byte = output_decimal
let output_4byte = output_decimal
let output_8byte = output_decimal
let output_nat = output_decimal
let output_int = output_decimal
let output_int8 = output_decimal
let output_int16 = output_decimal
let output_int32 = output_decimal
let output_int64 = output_decimal
let output_text n froms tos = output_string "\"<some text>\""


let output_arguments args : outputter * (unit -> int -> outputter -> outputter) =
  let herald_arguments = function
    | () when args = 0 -> output_string "# No arguments...\n"
    | _ when args = 1 -> output_string "# 1 argument follows\n"
    | _ -> Printf.printf "# %d arguments follow\n" args in
  let herald_member () i f () = Printf.printf "# Argument #%d%s:\n" i (if i + 1 = args then " (last)" else ""); f () in
  herald_arguments, (*bracket args*) herald_member

let start i = if i = 0 then output_string_space "{"
let stop max i = if i + 1 = max then output_string " }"
let bracket max g p i f () = start i; g p i f; stop max i

let output_vector members : outputter * (unit -> int -> outputter -> outputter) =
  let herald_vector () = if members = 0 then output_string_space "vec { }"
                         else output_string_space "vec" in
  let herald_member () i f = f (); output_string_space ";" in
  herald_vector, bracket members herald_member

let output_record members : outputter * (fields -> int -> outputter -> outputter) =
  let herald_record () = if members = 0 then output_string_space "record { }"
                         else output_string_space "record" in
  let herald_member fields i f = Printf.printf "%d : " (fst (Array.get fields i)); f (); output_string_space ";" in
  herald_record, bracket members herald_member

let output_variant members : outputter * (alts -> int -> outputter -> outputter) =
  let herald_variant () = assert (members <> 0);
                          output_string_space "variant" in
  let herald_member alts i f () = start 0; Printf.printf "%d : " (fst (Array.get alts i)); f (); stop 1 0 in
  herald_variant, herald_member
end

module Output : Dump = OutputIdl

let decode_primitive_type : int -> typ * outputter =
  let open Output in
  function
  | -1 -> Null, output_nil
  | -2 -> Bool, (fun () -> output_bool (read_bool ()))
  | -3 -> Nat, (fun () -> output_nat (read_leb128 ()))
  | -4 -> Int, (fun () -> output_int (read_sleb128 ()))
  | -5 -> NatN 8, (fun () -> output_byte (read_byte ()))
  | -6 -> NatN 16, (fun () -> output_2byte (read_2byte ()))
  | -7 -> NatN 32, (fun () -> output_4byte (read_4byte ()))
  | -8 -> NatN 64, (fun () -> output_8byte (read_8byte ()))
  | -9 -> IntN 8, (fun () -> output_int8 (read_int8 ()))
  | -10 -> IntN 16, (fun () -> output_int16 (read_int16 ()))
  | -11 -> IntN 32, (fun () -> output_int32 (read_int32 ()))
  | -12 -> IntN 64, (fun () -> output_int64 (read_int64 ()))
  | -13 | -14 -> failwith "no floats yet" (* TODO *)
  | -15 -> Text, (fun () -> let len = read_leb128 () in output_text len stdin stdout)
  | -16 -> Reserved, ignore
  | -17 -> Empty, ignore
  | _ -> failwith "unrecognised primitive type"


let read_type lookup : (typ * outputter) Lazy.t =
  let lprim_or_lookup = function
    | p when p < 0 -> lazy (decode_primitive_type p)
    | i -> lookup i in
  let prim_or_lookup ty = force (lprim_or_lookup ty) in

  let lfst p = lazy (let lazy (f, _) = p in f) in
  let lsnd p = lazy (let lazy (_, s) = p in s) in

  let open Output in
  match read_sleb128 () with
  | p when p < 0 && p > -18 -> from_val (decode_primitive_type p)
  | -18 ->
    begin
      let reader consumer () =
        match read_byte () with
        | 0 -> output_nil ()
        | 1 -> output_some (force consumer)
        | _ -> failwith "invalid optional" in
      match read_type_index () with
           | p when p < -17 -> assert false
           | p when p < 0 -> let t, consumer = decode_primitive_type p in
                             from_val (Opt (lazy t), reader (lazy consumer))
           | i -> lazy (let p = lookup i in
                        Opt (lfst p), reader (lsnd p))
    end
  | -19 -> begin match read_type_index () with
           | p when p < -17 -> assert false
           | p when p < 0 -> let t, consumer = decode_primitive_type p in
                             from_val (Vec (lazy t), fun () -> read_star_heralding () output_vector consumer)
           | i ->
             lazy (let p = lookup i in
                   Vec (lfst p), fun () -> read_star_heralding () output_vector (force (lsnd p)))
           end
  | -20 -> let assocs = read_t_star read_assoc in
           lazy (let herald_record, herald_member = output_record (Array.length assocs) in
                 let members = Array.map (fun (i, tynum) -> i, lfst (lprim_or_lookup tynum)) assocs in
                 let consumers = Array.mapi (herald_member members) (Array.map (fun (_, tynum) () -> snd (prim_or_lookup tynum) ()) assocs) in
                 Record members, fun () -> herald_record (); Array.iter (fun f -> f ()) consumers)
  | -21 -> let assocs = read_t_star read_assoc in
           lazy (let herald_variant, herald_member = output_variant (Array.length assocs) in
                 let alts = Array.map (fun (i, tynum) -> i, lfst (lprim_or_lookup tynum)) assocs in
                 let consumers = Array.map (fun (_, tynum) () -> snd (prim_or_lookup tynum) ()) assocs in
                 Variant alts, fun () -> herald_variant (); let i = read_leb128 () in herald_member alts i (Array.get consumers i) ())
  | -22 -> let assocs1 = read_t_star read_assoc in
           let assocs2 = read_t_star read_assoc in
           let anns = read_t_star read_annotation in
           lazy (let args = Array.map (fun (i, tynum) -> i, fst (prim_or_lookup tynum)) assocs1 in
                 let rslts = Array.map (fun (i, tynum) -> i, fst (prim_or_lookup tynum)) assocs2 in
                 Function (args, rslts, anns), epsilon)
(*
T(service {<methtype>*}) =
  sleb128(-23) T*(<methtype>* )
                   *)

  | t -> failwith (Printf.sprintf "unrecognised structured type: %d" t)


let read_type_table (t : unit -> (typ * outputter) Lazy.t) : (typ * outputter) Lazy.t array =
  let rep = read_leb128 () in
  Array.init rep (fun i -> Printf.printf "read_type_table: %d\n" i;t ())

(* Top-level *)

type mode = Legacy | Default

let top_level md : unit =
  Printf.printf "\nDESER, to your service!\n";
  read_magic ();
  Printf.printf "\n========================== Type section\n";
  let rec tab' = lazy (read_type_table (fun () -> read_type lookup))
      and lookup = fun indx -> (*Printf.printf "{indx: %d}" indx; *)Array.get (force tab') indx in
  let tab = Array.map force (force tab') in
  Printf.printf "\n========================== Value section\n";
  let open Output in
  begin match md with
  | Default ->
    let argtys = read_t_star read_type_index in
    let herald_arguments, herald_member = output_arguments (Array.length argtys) in
    herald_arguments ();
    let typ_ingester = function
      | prim when prim < 0 -> decode_primitive_type prim
      | index -> Array.get tab index in
    let consumers = Array.map (fun tynum -> let (ty, m) = typ_ingester tynum in m) argtys in
    Array.iteri (fun i f -> herald_member () i f ()) consumers
  | Legacy ->
    let argty = read_type_index () in
    Printf.printf "ARGTY: %d\n" argty;
    snd (Array.get tab argty) ()
  end;
  Printf.printf "\n-------- DESER DONE\n"
  
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



(* TODOs:
  - use bigint where necessary
  - customisable formatters
  - floats
  - service types
  - break lazy cycles (the way Opt does it) everywhere (Function!)
  - add `--idl` (default), `--json` and `--verbose`
 *)
