open Stdio.In_channel
open Lazy

(* The type of outputters

 While decoding a type, we simultaneously build an outputter (IO action),
 that reads bytes from the stdin and dumps to stdout in a formatted way.
*)

type outputter = unit -> unit

(* noise reduction *)

let chatty = ref false

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
  msb lsl 8 + lsb

let read_4byte () : int =
  let lsb = read_2byte () in
  let msb = read_2byte () in
  msb lsl 16 + lsb

let read_8byte () : Big_int.big_int =
  let lsb = read_4byte () in
  let msb = read_4byte () in
  Big_int.(add_int_big_int lsb (mult_int_big_int 4294967296 (big_int_of_int msb)))

let read_signed_byte () : bool * int =
  let b = read_byte () in
  if b > 127 then true, b - 128 else false, b

let read_known char : unit =
  match read_byte () with
  | b when b = Char.code char -> ()
  | _ -> failwith "unexpected"

(* encoding numbers *)

let add_uint8 b v =
  let open Buffer in
  add_char b (char_of_int (v land 0xFF))

let rec encode_leb128 n acc =
  let open Big_int in
  let lim = big_int_of_int 128 in
  if lt_big_int n lim then
    add_uint8 acc (int_of_big_int n land 0x7F)
  else
    let q, m = quomod_big_int n lim in
    add_uint8 acc (128 lor int_of_big_int n);
    encode_leb128 q acc

let rec encode_sleb128 n acc =
  let open Big_int in
  let lim_pos, lim_neg = big_int_of_int 64, big_int_of_int (-64) in
  if lt_big_int n lim_pos && ge_big_int n lim_neg then
    add_uint8 acc (int_of_big_int n land 0x7F)
  else
    let q, m = quomod_big_int n (big_int_of_int 128) in
    add_uint8 acc (128 lor int_of_big_int n);
    encode_sleb128 q acc

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
  msb lsl 8 lor lsb

let read_int32 () : int =
  let lsb = read_2byte () in
  let msb = read_int16 () in
  msb lsl 16 lor lsb

let read_int64 () : Big_int.big_int =
  let lsb = read_4byte () in
  let msb = read_int32 () in
  Big_int.(add_int_big_int lsb (mult_int_big_int 4294967296 (big_int_of_int msb)))

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
         | Function of typ Lazy.t array * typ Lazy.t array * ann array
         | Future of int * Buffer.t

and alts = (int * typ Lazy.t) array
and fields = (int * typ Lazy.t) array

(* type index/ground type (negative) *)

let read_type_index () = let ty = read_sleb128 () in assert (ty > -18); ty

let read_assoc () = let hash = read_leb128 () in
                    let tynum = read_type_index () in
                    if !chatty then Printf.printf "hash: %d, tynum: %d\n" hash tynum; hash, tynum

module type Dump =
sig
val output_nat : int -> unit
val output_int : int -> unit
val output_bool : bool -> unit
val output_nil : outputter
val output_byte : int -> unit
val output_2byte : int -> unit
val output_4byte : int -> unit
val output_8byte : Big_int.big_int -> unit
val output_int8 : int -> unit
val output_int16 : int -> unit
val output_int32 : int -> unit
val output_int64 : Big_int.big_int -> unit
val output_text : int -> Stdio.In_channel.t -> Stdio.Out_channel.t -> unit
val output_some : outputter -> unit
val output_arguments : int -> outputter * (unit -> int -> outputter -> outputter)
val output_vector : int -> outputter * (unit -> int -> outputter -> outputter)
val output_record : int -> outputter * (fields -> int -> outputter -> outputter)
val output_variant : int -> outputter * (alts -> int -> outputter -> outputter)
end


module OutputProse : Dump = struct

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
let output_big_decimal what (i : Big_int.big_int) = Printf.printf "%s%s: %s\n" (fill ()) what (Big_int.string_of_big_int i)

(* outputters *)
let output_nat nat = output_decimal "output_nat" nat
let output_int int = output_decimal "output_int" int
let output_bool b = output_string "output_bool" (if b then "true" else "false")
let output_nil () = Printf.printf "%snull (0 bytes)\n" (fill ())
let output_some consumer = Printf.printf "%sSome: value follows on the next line\n"  (fill ()); consumer ()
let output_byte b = output_decimal "output_byte" b
let output_2byte b = output_decimal "output_2byte" b
let output_4byte b = output_decimal "output_4byte" b
let output_8byte b = output_big_decimal "output_8byte" b
let output_int8 i = output_decimal "output_int8" i
let output_int16 i = output_decimal "output_int16" i
let output_int32 i = output_decimal "output_int32" i
let output_int64 i = output_big_decimal "output_int64" i
let output_text bytes from tostream =
      let buf = Buffer.create 0 in
      ignore (input_buffer from buf ~len:bytes);
      Printf.printf "%sText: %d bytes follow on next line\n" (fill ()) bytes;
      Printf.printf "%s---->" (fill ());
      Stdio.Out_channel.output_buffer tostream buf;
      print_string "\n"

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
let chat_string s = if !chatty then output_string s
let output_string_space (s : string) = output_string s; output_string " "
let output_decimal (i : int) = Printf.printf "%d" i
let output_big_decimal (i : Big_int.big_int) = output_string (Big_int.string_of_big_int i)
(* let output_bignum (i : Big_int.big_int) = Printf.printf "%s" (Big_int.string_of_big_int i) *)
let output_bignum (i : int) = output_decimal i (* for now, later: output_big_decimal *)
let casted ty f v = match ty with
  | IntN n -> f v; Printf.printf " : int%d" n
  | NatN n -> f v; Printf.printf " : nat%d" n
  | _ -> assert false

let output_bool b = output_string (if b then "true" else "false")
let output_nil () = output_string "null"
let output_some consumer = output_string_space "opt"; consumer ()
let output_byte, output_2byte, output_4byte =
  casted (NatN 8) output_decimal, casted (NatN 16) output_decimal, casted (NatN 32) output_decimal
let output_8byte = casted (NatN 64) output_big_decimal
let output_nat, output_int = output_bignum, output_bignum
let output_int8, output_int16, output_int32 =
  casted (IntN 8) output_decimal, casted (IntN 16) output_decimal, casted (IntN 32) output_decimal
let output_int64 = casted (IntN 64) output_big_decimal
let output_text n froms tos =
  output_string "\"";
  let buf = Buffer.create 0 in
  ignore (input_buffer froms buf ~len:n);
  Stdio.Out_channel.output_buffer tos buf;
  output_string "\""


let output_arguments args : outputter * (unit -> int -> outputter -> outputter) =
  let last i = i + 1 = args in
  let herald_arguments = function
    | () when args = 0 -> chat_string "// No arguments...\n"; output_string "()"
    | _ when args = 1 -> chat_string "// 1 argument follows\n"
    | _ -> if !chatty then Printf.printf "// %d arguments follow\n" args in
  let herald_member () i f () =
    if !chatty then Printf.printf "// Argument #%d%s:\n" i (if last i then " (last)" else "");
    output_string (if i = 0 then "( " else ", ");
    f ();
    output_string (if last i then "\n)" else "\n") in
  herald_arguments, herald_member

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


module OutputJson : Dump = struct

let output_string (s : string) = print_string s
let output_string_space (s : string) = Printf.printf "%s " s
let output_decimal (i : int) = Printf.printf "%d" i
let output_big_decimal (i : Big_int.big_int) = output_string (Big_int.string_of_big_int i)
let output_bignum (i : int) = output_decimal i (* for now *)

let output_bool b = output_string (if b then "true" else "false")
let output_nil () = output_string "null"
let output_some consumer = output_string "["; consumer (); output_string "]"
let output_byte, output_2byte, output_4byte = output_decimal, output_decimal, output_decimal
let output_8byte = output_big_decimal
let output_nat, output_int = output_bignum, output_bignum
let output_int8, output_int16, output_int32 = output_decimal, output_decimal, output_decimal
let output_int64 = output_big_decimal
let output_text n froms tos =
  output_string "\"";
  let buf = Buffer.create 0 in
  ignore (input_buffer froms buf ~len:n);
  Stdio.Out_channel.output_buffer tos buf;
  output_string "\""


let output_arguments args : outputter * (unit -> int -> outputter -> outputter) =
  let herald_arguments = function
    | () when args = 0 -> output_string "# No arguments...\n"
    | _ when args = 1 -> output_string "# 1 argument follows"
    | _ -> Printf.printf "# %d arguments follow" args in
  let herald_member () i f () = Printf.printf "\n# Argument #%d%s:\n" i (if i + 1 = args then " (last)" else ""); f () in
  herald_arguments, (*bracket args*) herald_member

let start punct i = if i = 0 then output_string (String.make 1 punct)
let stop punct max i = if i + 1 = max then output_string (String.make 1 punct)
let bracket punct max g p i f () = start (punct.[0]) i; g p i f; stop (punct.[1]) max i

let output_vector members : outputter * (unit -> int -> outputter -> outputter) =
  let punct = "[]" in
  let herald_vector () = if members = 0 then output_string_space punct in
  let herald_member () i f = if (i > 0) then output_string_space ","; f () in
  herald_vector, bracket punct members herald_member

let output_record members : outputter * (fields -> int -> outputter -> outputter) =
  let punct = "{}" in
  let herald_record () = if members = 0 then output_string_space punct in
  let herald_member fields i f = if (i > 0) then output_string_space ","; Printf.printf "\"_%d_\": " (fst (Array.get fields i)); f () in
  herald_record, bracket punct members herald_member

let output_variant members : outputter * (alts -> int -> outputter -> outputter) =
  let herald_variant () = assert (members <> 0) in
  let herald_member alts i f () = start '{' 0; Printf.printf "_%d_ : " (fst (Array.get alts i)); f (); stop '}' 1 0 in
  herald_variant, herald_member
end

(* IDL binary mode:
   - Legacy: top-level encoded as one value
   - Default: top-level are several values
 *)
type mode = Unary | Nary

module MakeOutputter(F : Dump) = struct

let decode_primitive_type : int -> typ * outputter =
  let open F in
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
    | p when p < -17 -> assert false
    | p when p < 0 -> lazy (decode_primitive_type p)
    | i -> lookup i in
  let prim_or_lookup ty = force (lprim_or_lookup ty) in

  let lfst p = lazy (let lazy (f, _) = p in f) in
  let lsnd p = lazy (let lazy (_, s) = p in s) in

  let open F in
  match read_sleb128 () with
  | p when p < 0 && p > -18 -> from_val (decode_primitive_type p)
  | -18 ->
    let reader consumer () =
      match read_byte () with
      | 0 -> output_nil ()
      | 1 -> output_some (force consumer)
      | _ -> failwith "invalid optional" in
    let i = read_type_index () in
    lazy (let p = lprim_or_lookup i in
          Opt (lfst p), reader (lsnd p))
  | -19 -> let i = read_type_index () in
           lazy
             (let p = lprim_or_lookup i in
              Vec (lfst p), fun () -> read_star_heralding () output_vector (force (lsnd p)))
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
  | -22 -> let types1 = read_t_star read_type_index in
           let types2 = read_t_star read_type_index in
           let anns = read_t_star read_annotation in
           lazy (let args = Array.map (fun tynum -> lfst (lprim_or_lookup tynum)) types1 in
                 let rslts = Array.map (fun tynum -> lfst (lprim_or_lookup tynum)) types2 in
                 Function (args, rslts, anns), epsilon)

(*
T(service {<methtype>*}) = sleb128(-23) T*(<methtype>* )
*)
  | -23 -> failwith "service types not supported yet"

  | t -> (* future type *)
    let bytes = read_leb128 () in
    let buf = Buffer.create 0 in
    ignore (input_buffer stdin buf ~len:bytes);
    let ingest () =
      let bytes = read_leb128 () in
      let refs = read_leb128 () in
      let buf = Buffer.create 0 in
      assert (refs = 0);
      ignore (input_buffer stdin buf ~len:bytes) in
    lazy (Future (t, buf), ingest)


let read_type_table (t : unit -> (typ * outputter) Lazy.t) : (typ * outputter) Lazy.t array =
  let rep = read_leb128 () in
  Array.init rep (fun i -> if !chatty then Printf.printf "read_type_table: %d\n" i; t ())

(* Utilities *)

let chat_string = if !chatty then print_string else ignore

(* Top-level *)

let top_level md : unit =
  chat_string "\nDESER, to your service!\n";
  read_magic ();
  chat_string "\n========================== Type section\n";
  let tab =
    let rec tab = lazy (read_type_table (fun () -> read_type lookup))
    and lookup = fun indx -> (*Printf.printf "{indx: %d}" indx; *)Array.get (force tab) indx in
    Array.map force (force tab) in
  chat_string "\n========================== Value section\n";
  let open F in
  begin match md with
  | Nary ->
    let argtys = read_t_star read_type_index in
    let herald_arguments, herald_member = output_arguments (Array.length argtys) in
    herald_arguments ();
    let typ_ingester = function
      | prim when prim < 0 -> decode_primitive_type prim
      | index -> Array.get tab index in
    let consumers = Array.map (fun tynum -> let (ty, m) = typ_ingester tynum in m) argtys in
    Array.iteri (fun i f -> herald_member () i f ()) consumers
  | Unary ->
    let argty = read_type_index () in
    if !chatty then Printf.printf "# ARGTY: %d\n" argty;
    snd (Array.get tab argty) ()
  end;
  chat_string "\n-------- DESER DONE\n"

end

(* CLI *)

let name = "deser"
let version = "0.1"
let banner = "Interface Description Language (IDL) " ^ version ^ " message dumper"
let usage = "Usage: " ^ name ^ " [option] [file ...]"

let mode = ref Nary

let set_mode m () =
  if !mode <> Nary then begin
    Printf.eprintf "deser: multiple execution modes specified"; exit 1
  end;
  mode := m

type format = Idl | Prose | Json

let output_format = ref Idl

let set_format f () =
  if !output_format <> Idl then begin
    Printf.eprintf "deser: multiple output formats specified"; exit 1
  end;
  output_format := f

let reverse_mode = ref false

let set_reverse f () =
  reverse_mode := f

let argspec = Arg.align
[
  "--unary", Arg.Unit (set_mode Unary), " decode legacy (unary) message API";
  "--prose", Arg.Unit (set_format Prose), " output indented prose";
  "--json", Arg.Unit (set_format Json), " output JSON values";
  "--idl", Arg.Unit (set_format Idl), " output IDL values (default)";
  "--reverse", Arg.Unit (set_reverse true), " parse IDL values";
  "--verbose", Arg.Unit (fun () -> chatty := true), " amend commentary";
  "--version",
    Arg.Unit (fun () -> Printf.printf "%s\n" banner; exit 0), " show version";
]

let add_arg source = () (* args := !args @ [source] *)


(* type checker/inferencer -- SHOULD MOVE TO IDLLIB *)
module Typer = struct
open Idllib.Syntax
open Source

(* (s)leb128 helpers *)

let write_leb128 i =
    let buf = Buffer.create 0 in
    encode_leb128 i buf;
    Buffer.output_buffer stdout buf

let write_int_leb128 n = write_leb128 (Big_int.big_int_of_int n)

let write_sleb128 i =
    let buf = Buffer.create 0 in
    encode_sleb128 i buf;
    Buffer.output_buffer stdout buf

let write_int_sleb128 n = write_sleb128 (Big_int.big_int_of_int n)

(* IDL value writing *)

let write_nil (v : value') = ()
let write_text = function
  | TextV str ->
    let open Buffer in
    let buf = create 0 in
    add_string buf str;
    write_int_leb128 (length buf);
    output_buffer stdout buf
  | _ -> assert false

let rec write_bool = function
  | AnnotV (v, t) -> write_bool v.it
  | FalseV ->
    let buf = Buffer.create 0 in
    add_uint8 buf 0; Buffer.output_buffer stdout buf
  | TrueV ->
    let buf = Buffer.create 0 in
    add_uint8 buf 1; Buffer.output_buffer stdout buf
  | _ -> assert false

let rec write_byte (v : value') =
  match v with
  | AnnotV (v, t) -> write_byte v.it
  | IntegralV i ->
    let buf = Buffer.create 0 in
    add_uint8 buf (Big_int.int_of_big_int i);
    Buffer.output_buffer stdout buf
  | _ -> assert false

let rec write_2byte (v : value') =
  match v with
  | AnnotV (v, t) -> write_2byte v.it
  | IntegralV i ->
    let buf = Buffer.create 0 in
    let i = Big_int.int_of_big_int i in
    add_uint8 buf (i land 0xFF);
    add_uint8 buf (i lsr 8);
    Buffer.output_buffer stdout buf
  | _ -> assert false

let rec write_4byte (v : value') =
  match v with
  | AnnotV (v, t) -> write_4byte v.it
  | IntegralV i ->
    let buf = Buffer.create 0 in
    let i = Big_int.int_of_big_int i in
    add_uint8 buf (i land 0xFF);
    add_uint8 buf (i lsr 8 land 0xFF);
    add_uint8 buf (i lsr 16 land 0xFF);
    add_uint8 buf (i lsr 24);
    Buffer.output_buffer stdout buf
  | _ -> assert false

let rec write_8byte (v : value') =
  match v with
  | AnnotV (v, t) -> write_8byte v.it
  | IntegralV i ->
    let open Big_int in
    let lim = big_int_of_int64 Int64.max_int in
    let i = if gt_big_int i lim then add_big_int i (mult_int_big_int 2 (big_int_of_int64 Int64.min_int)) else i in
    let i = int64_of_big_int i in
    let slice_at n = Int64.(to_int (shift_right_logical i n)) land 0xFF in
    let buf = Buffer.create 0 in
    add_uint8 buf (slice_at 0);
    add_uint8 buf (slice_at 8);
    add_uint8 buf (slice_at 16);
    add_uint8 buf (slice_at 24);
    add_uint8 buf (slice_at 32);
    add_uint8 buf (slice_at 40);
    add_uint8 buf (slice_at 48);
    add_uint8 buf (slice_at 56);
    Buffer.output_buffer stdout buf
  | _ -> assert false

let rec write_signedbyte (v : value') =
  match v with
  | AnnotV (v, t) -> write_signedbyte v.it
  | IntegralV i ->
    let buf = Buffer.create 0 in
    let i = Big_int.int_of_big_int i in
    add_uint8 buf (i land 0xFF);
    Buffer.output_buffer stdout buf
  | _ -> assert false

let rec write_2signedbyte (v : value') =
  match v with
  | AnnotV (v, t) -> write_2signedbyte v.it
  | IntegralV i ->
    let buf = Buffer.create 0 in
    let i = Big_int.int_of_big_int i in
    add_uint8 buf (i land 0xFF);
    let i = i asr 8 in
    add_uint8 buf (i land 0xFF);
    Buffer.output_buffer stdout buf
  | _ -> assert false

let rec write_4signedbyte (v : value') =
  match v with
  | AnnotV (v, t) -> write_4signedbyte v.it
  | IntegralV i ->
    let buf = Buffer.create 0 in
    let i = Big_int.int_of_big_int i in
    add_uint8 buf (i land 0xFF);
    let i = i asr 8 in
    add_uint8 buf (i land 0xFF);
    let i = i asr 8 in
    add_uint8 buf (i land 0xFF);
    let i = i asr 8 in
    add_uint8 buf (i land 0xFF);
    Buffer.output_buffer stdout buf
  | _ -> assert false

let rec write_8signedbyte (v : value') =
  match v with
  | AnnotV (v, t) -> write_8signedbyte v.it
  | IntegralV i ->
    let open Big_int in
    let i = int64_of_big_int i in
    let slice_at n = Int64.(to_int (shift_right i n)) land 0xFF in
    let buf = Buffer.create 0 in
    add_uint8 buf (slice_at 0);
    add_uint8 buf (slice_at 8);
    add_uint8 buf (slice_at 16);
    add_uint8 buf (slice_at 24);
    add_uint8 buf (slice_at 32);
    add_uint8 buf (slice_at 40);
    add_uint8 buf (slice_at 48);
    add_uint8 buf (slice_at 56);
    Buffer.output_buffer stdout buf
  | _ -> assert false

let rec write_nat (v : value') =
  match v with
  | AnnotV (v, t) -> write_nat v.it
  | IntegralV i -> write_leb128 i
  | _ -> assert false

let rec write_int (v : value') =
  match v with
  | AnnotV (v, t) -> write_int v.it
  | IntegralV i -> write_sleb128 i
  | _ -> assert false

let type_assoc = List.map (fun (prim, snd) -> (PrimT prim, snd))
  [ Null, (-1, write_nil)
  ; Bool, (-2, write_bool)
  ; Nat, (-3, write_nat)
  ; Int, (-4, write_int)
  ; Nat8, (-5, write_byte)
  ; Nat16, (-6, write_2byte)
  ; Nat32, (-7, write_4byte)
  ; Nat64, (-8, write_8byte)
  ; Int8, (-9, write_signedbyte)
  ; Int16, (-10, write_2signedbyte)
  ; Int32, (-11, write_4signedbyte)
  ; Int64, (-12, write_8signedbyte)
  (* | -13 | -14 -> failwith "no floats yet" (* TODO *) *)
  ; Text, (-15, write_text)
  ; Reserved, (-16, write_nil)
  ; Empty, (-17, write_nil)
  ]

let lookup_prim_tynum (ty : typ) = List.assoc ty.Source.it type_assoc

(* the type of bidirectional checker/inferencer
Every IDL value corresponds to such a function.

 - for inferencing the most general type: call with `PreT`
 - for type checking against `t`: call with `t`

NOPE: Guaranteed to return a `typ` that is `<=` in the IDL type lattice.
 *)
type bid = typ' -> typ'

let rec glb t u =
  match t, u with
  | PrimT p, PrimT q when p = q -> t
  | PrimT Empty, _ -> u
  | _, PrimT Empty -> t
  | PrimT Reserved, _ | _, PrimT Reserved -> PrimT Reserved
  | PrimT Nat, PrimT Int -> u
  | PrimT Int, PrimT Nat -> t
  | VecT p, VecT q -> VecT (glb p.it q.it @@ p.at)
  | PrimT Null, OptT _ -> u
  | OptT _, PrimT Null -> t
  | _ -> failwith "glb TODO"

let rec lub t u =
  match t, u with
  | PrimT Reserved, _ -> u
  | _, PrimT Reserved -> t
  | PrimT Empty, _ | _, PrimT Empty -> PrimT Empty
  | PrimT p, PrimT q when p = q -> t
  | PrimT Nat, PrimT Int -> t
  | PrimT Int, PrimT Nat -> u
  | PrimT _, PrimT _ -> PrimT Empty
  | VecT p, VecT q -> VecT (lub p.it q.it @@ p.at)
  | _ -> failwith "lub TODO"


let rec to_bid (v : value) : bid =
  let bottom = PrimT Empty in
  match v.it with
  | FalseV
  | TrueV -> (function
              | PreT | PrimT Bool -> PrimT Bool
              | _ -> bottom)
  | NullV -> (function
              | PreT | PrimT Null -> PrimT Null
              | OptT _ as opt -> opt
              | _ -> bottom)
  | TextV _ -> (function
                | PreT | PrimT Text -> PrimT Text
                | _ -> bottom)
  | IntegralV i -> let negative = Big_int.sign_big_int i < 0 in
                   (function
                    | PreT | PrimT Int -> PrimT (if negative then Int else Nat)
                    | PrimT Nat -> PrimT (if negative then Empty else Nat) (* TODO: report error *)
                    | PrimT (Int8|Int16|Int32|Int64|Nat8|Nat16|Nat32|Nat64 as t) when in_range t i -> PrimT t
                    | _ -> bottom)
  | OptV v -> (function
               | PreT -> OptT (infer v)
               | OptT t -> OptT (to_bid v t.it @@ v.at)
               | _ -> bottom)
  | VecV vs -> (function
                | PreT -> VecT List.(fold_left glb (PrimT Empty) (map infer' vs) @@ v.at)
                | _ -> failwith "cannot VecT yet")
  | RecordV vfs -> (function
                    | PreT -> RecordT (List.map infer_field vfs)
                    | RecordT tfs ->
                      let identity x = x in
                      let compose g f x = g (f x) in
                      let refiners = List.map refine_record_field tfs in
                      List.fold_left compose identity refiners (RecordT (List.map infer_field vfs))
                    | _ -> bottom)
  | VariantV vf -> (function
                    | PreT ->
                      VariantT [{label = Id vf.it.hash @@ vf.at; typ = infer vf.it.value} @@ vf.at]
                    | VariantT tfs -> refine_variant_field vf tfs
                    | _ -> bottom)
  | FuncV _ -> failwith "cannot FuncV yet"

  | AnnotV (v, t) -> (function
                      | PreT -> to_bid v t.it
                      | t' -> to_bid v (lub t' t.it))
  | ServiceV uri -> (function
                     | PreT -> ServT []
                     | ServT _ as serv -> serv
                     | _ -> bottom)

and infer v = infer' v @@ v.at
and infer' v = to_bid v PreT

and infer_field vf = match vf.it with
  | { hash; name = Some id; value } -> { label = Named id.it @@ id.at; typ = infer value } @@ vf.at
  | { hash; name = None; value } -> { label = Id hash @@ vf.at; typ = infer value } @@ vf.at

and same_label = function
  | Named s, Named t -> s = t
  | (Id s | Unnamed s), Named t -> s = Idllib.IdlHash.idl_hash t
  | Named s, (Id t | Unnamed t) -> Idllib.IdlHash.idl_hash s = t
  | (Id s | Unnamed s), (Unnamed t | Id t) -> s = t

and refine_record_field tf t =
  match t with
  | RecordT tfs ->
    let {label; typ} = tf.it in
    let comb tf' (tfs, absent) =
      let {label = label'; typ = typ'} = tf'.it in
      if same_label (label.it, label'.it)
      then ({label = label'; typ = lub typ.it typ'.it @@ typ'.at} @@ tf'.at) :: tfs, false
      else tf' :: tfs, absent in
    let tfs, bad = List.fold_right comb tfs ([], true) in
    if bad then PrimT Empty else RecordT tfs
  | PrimT Empty -> t
  | _ -> assert false

and refine_variant_field vf tfs =
  let {hash; value; _} = vf.it in
  if List.exists (fun {it={label;_};_} -> same_label (Id hash, label.it)) tfs then
    let trans tf =
      let {label; typ} = tf.it in
      if same_label (Id vf.it.hash, label.it) then
        begin
          let inf = to_bid value typ.it in
          {label; typ = inf @@ vf.at} @@ vf.at
        end
      else tf in
    VariantT (List.map trans tfs)
  else PrimT Empty


and in_range t v =
  let open Big_int in
  let two_raised = power_int_positive_int 2 in
  let neg_two_raised n = minus_big_int (two_raised n) in
  let between low up = le_big_int low v && lt_big_int v up in
                    (* TODO: report error on v *)
  match t with
  | Nat8 -> between zero_big_int (two_raised 8)
  | Nat16 -> between zero_big_int (two_raised 16)
  | Nat32 -> between zero_big_int (two_raised 32)
  | Nat64 -> between zero_big_int (two_raised 64)
  | Int8 -> between (neg_two_raised 7) (two_raised 7)
  | Int16 -> between (neg_two_raised 15) (two_raised 15)
  | Int32 -> between (neg_two_raised 31) (two_raised 31)
  | Int64 -> between (neg_two_raised 63) (two_raised 63)
  | _ -> assert false

end


(* type table *)

module TypeTable = struct
open Idllib
open Syntax
open Source

let rec fold_typ (f : Idllib.Syntax.typ' -> 'a -> 'a) a ty =
  match ty with
   | PrimT _ -> a
   | OptT t | VecT t -> fold_typ f (f ty a) t.it
   | RecordT tfs | VariantT tfs -> List.fold_left (fun a {it;_} -> fold_field f a it) (f ty a) tfs
   | _ -> a (* TODO *)

and fold_field (f : Idllib.Syntax.typ' -> 'a -> 'a) a ({ label; typ } : Idllib.Syntax.typ_field') =
  match typ.it with
  | PrimT _ -> a
  | t -> f t a

module M = Map.Make (struct type t = Idllib.Syntax.typ' let compare = compare end)

let prim = function
  | {it = PrimT _; _} -> true
  | _ -> false

let build_typ_map =
  let open M in
  let augment ty m = if mem ty m then m else add ty (cardinal m) m in
  fold_typ augment

let lookup_tynum t m =
  if prim t then
    fst (Typer.lookup_prim_tynum t)
  else
    M.find t.it m

let write_label = function
  | Id i | Unnamed i -> Typer.write_leb128 (Big_int.big_int_of_int32 (Lib.Uint32.to_int32 i))
  | Named n -> Typer.write_leb128 (Big_int.big_int_of_int32 (Lib.Uint32.to_int32 (Idllib.IdlHash.idl_hash n)))

let write_type_fields key m tfs =
  let open Typer in
  write_int_sleb128 key;
  write_int_leb128 (List.length tfs);
  List.iter
    (fun { it = { label; typ }; _ } ->
      write_label label.it;
      write_int_sleb128 (lookup_tynum typ m))
    tfs

let write_typ_index m (ty, i) =
  let open Typer in
  match ty with
  | OptT t ->
    write_int_sleb128 (-18);
    write_int_sleb128 (lookup_tynum t m)
  | VecT t ->
    write_int_sleb128 (-19);
    write_int_sleb128 (lookup_tynum t m)
  | RecordT tfs -> write_type_fields (-20) m tfs
  | VariantT tfs -> write_type_fields (-21) m tfs
  | PrimT _ -> assert false
  | _ -> assert false (* TODO: function, service *)

let rec write_typed_value t v = match t.it, v.it with
  | _, AnnotV (v', _) -> write_typed_value t v'
  | PrimT _, v -> snd (Typer.lookup_prim_tynum t) v
  | OptT _, NullV -> Typer.write_int_leb128 0
  | OptT t', OptV v' -> Typer.write_int_leb128 1; write_typed_value t' v'
  | VecT t', VecV vs -> Typer.write_int_leb128 (List.length vs); List.iter (write_typed_value t') vs
  | RecordT tfs, RecordV vfs -> List.iter2 write_typed_value_field tfs vfs
  | VariantT tfs, VariantV vf -> write_typed_variant_field tfs vf
  | _ -> assert false

and write_typed_value_field tf vf = match tf.it, vf.it with
  | { label; typ }, { hash; value; _ } ->
    assert (Typer.same_label (Id vf.it.hash, tf.it.label.it));
    write_typed_value typ value

and write_typed_variant_field tfs vf =
  let matching i tf =
    if Typer.same_label (Id vf.it.hash, tf.it.label.it) then
      begin
        Typer.write_int_leb128 i;
        write_typed_value_field tf vf
      end in
  List.iteri matching tfs

end

module Sanitise = struct
open Idllib
open Syntax
open Source

let rec sanitise_value (v : value) : value =
  match v.it with
  | NullV | TrueV | FalseV | TextV _ | IntegralV _ | FuncV _ | ServiceV _ -> v
  | OptV v' -> { v with it = OptV (sanitise_value v') }
  | VecV vs -> { v with it = VecV (List.map sanitise_value vs) }
  | RecordV vfs -> { v with it = RecordV (sanitise_value_fields vfs) }
  | VariantV vf -> { v with it = VariantV (sanitise_value_field vf) }
  | AnnotV (v', t) -> { v with it = AnnotV (sanitise_value v', sanitise_type t) }

and sanitise_value_fields vfs =
  let hash_compare a b = Lib.Uint32.compare a.it.hash b.it.hash in
  let vfs' = List.sort_uniq hash_compare vfs in
  if List.(length vfs' <> length vfs) then
    begin
      Printf.eprintf "deser: record value fields repeated";
      exit 1
    end;
  List.map sanitise_value_field vfs'
and sanitise_value_field vf = { vf with it = { vf.it with value = sanitise_value vf.it.value } }

and sanitise_type t =
  match t.it with
  | PrimT _ | VarT _ | PreT -> t
  | OptT t' -> { t with it = OptT (sanitise_type t') }
  | VecT t' -> { t with it = VecT (sanitise_type t') }
  | RecordT tfs -> { t with it = RecordT (sanitise_type_fields tfs) }
  | VariantT tfs -> { t with it = VariantT (sanitise_type_fields tfs) }
  | FuncT _ | ServT _ -> assert false (* TODO *)

and sanitise_type_fields tfs =
  let hash_of = function
    | Id h | Unnamed h -> h
    | Named s -> Idllib.IdlHash.idl_hash s in
  let hash_compare a b = Lib.Uint32.compare (hash_of a.it.label.it) (hash_of b.it.label.it) in
  let tfs' = List.sort_uniq hash_compare tfs in
  if List.(length tfs' <> length tfs) then
    begin
      Printf.eprintf "deser: type fields repeated";
      exit 1
    end;
  List.map sanitise_type_field tfs'
and sanitise_type_field tf = { tf with it = { tf.it with typ = sanitise_type tf.it.typ} }

(* given a type, decide if there is a value inhabiting it *)
(* TODO: Should we relax this to be constructor aware? *)
let rec value_realisable = function
  | PrimT Empty | PreT -> false
  | PrimT _ | VarT _ -> true
  | OptT t' | VecT t' -> value_realisable t'.it
  | RecordT tfs | VariantT tfs -> List.for_all (fun t -> value_realisable t.it.typ.it) tfs
  | FuncT _ | ServT _ -> assert false (* TODO *)

end

(* run it *)

let () =
  Arg.parse argspec add_arg usage;
  if !reverse_mode then
    begin
      let open Idllib in
      let open Source in
      Printf.printf "\nDESER, reading!\n";
      let lexer = Lexing.from_channel stdin in
      let {it = vs; _} = Parser.parse_arg Lexer.value_token lexer "<stdin>" in
      Wasm.Sexpr.print 80 Arrange_idl.("Arg" $$ List.map value vs);
      Printf.printf "\nDESER, parsed!\n";
      let vs = List.map Sanitise.sanitise_value vs in
      Printf.printf "\nDESER, sanitised!\n";
      Wasm.Sexpr.print 80 Arrange_idl.("(sanitised) Arg" $$ List.map value vs);
      let open Typer in
      let ts = List.map infer vs in
      Wasm.Sexpr.print 80 Arrange_idl.("ArgTy" $$ List.map typ ts);
      Printf.printf "\nDESER, type inferred!\n";
      if List.for_all (fun t -> Sanitise.value_realisable t.it) ts then
        begin
          let open TypeTable in
          let m = List.fold_left (fun m ty -> build_typ_map m ty.it) M.empty ts in
          print_string "DIDL"; write_int_leb128 (M.cardinal m);
          List.iter (write_typ_index m) (List.sort (fun (_, i1) (_, i2) -> compare i1 i2) (M.bindings m));
          write_int_leb128 (List.length vs);
          List.iter (fun t -> write_int_sleb128 (lookup_tynum t m)) ts;
          List.iter2 write_typed_value ts vs;
          if List.for_all TypeTable.prim ts then () else Printf.printf "\nDESER, term traversed!\n";
        end
      else
        begin
          Printf.eprintf "deser: inferred type is not realisable";
          exit 1
        end
    end
  else
    begin
      begin match !output_format with
      | Prose -> let module Prose = MakeOutputter(OutputProse) in Prose.top_level !mode;
      | Idl -> let module Idl = MakeOutputter(OutputIdl) in Idl.top_level !mode;
      | Json -> let module Json = MakeOutputter(OutputJson) in Json.top_level !mode;
      end;
      match input_byte stdin with
      | Some _ -> failwith "surplus bytes in input"
      | None -> ()
    end


(* TODOs:
  - use bigint where necessary
  - floats
  - service types
  - escaping in text
  - heralding/outputting of type table
  - sort type/value fields (optionally?) `--sanitize`/`--verbatim`
  - shrink type table (union-find)
  - `+42` infers as `nat`, should be `int`
  - shorthands
 *)
