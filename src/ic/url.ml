(* Parsing known URLs from mo: and ic: URLs *)

(*
   For usage examples take a look at url_test.ml
*)


(* Decode a principal according to https://docs.dfinity.systems/public/#textual-ids *)
let checkbytes s : string =
  let buf = Buffer.create 4 in
  Buffer.add_int32_be buf (Lib.CRC.crc32 s); (* NB: big endian *)
  Buffer.contents buf

let decode_prinicpal principal : (string, string) result =
  let open Stdlib.String in

  if equal principal "" then Error "principal cannot be empty" else
  if lowercase_ascii principal <> principal then Error "principal must be lowercase" else
  let isBase32 c = c == '-' || c >= '0' && c <= '9' || c >= 'a' && c <= 'z' in
  if not (Lib.Seq.for_all isBase32 (to_seq principal)) then Error "principal must only contain lowercase letters, digits and dashes" else
  (* TODO: We could check that dashes appear at suitable grouping *)
  match Lib.Base32.decode principal with
  | Error e -> Error e
  | Ok bytes ->
    if length bytes < 4 then Error "principal too short" else
    let payload = sub bytes 4 (length bytes - 4) in
    if sub bytes 0 4 <> checkbytes payload then Error "invalid checksum in principal ID, please check for typos" else
    Ok payload

let rec group s =
  let open String in
  if length s <= 5 then s else
  sub s 0 5 ^ "-" ^ group (sub s 5 (length s - 5))

let encode_principal bytes : string =
  group (Lib.Base32.encode (checkbytes bytes ^ bytes))

type parsed =
  | Package of (string * string)
  | Relative of string
  | Ic of string
  | IcAlias of string
  | Prim

let string_of_parsed = function
  | Package (x, y) -> Printf.sprintf "Package (%s, %s)" x y
  | Relative x -> Printf.sprintf "Relative %s" x
  | Ic x -> Printf.sprintf "Ic %s" x
  | IcAlias x -> Printf.sprintf "IcAlias %s" x
  | Prim -> "Prim"

let parse (f: string) : (parsed, string) result =
  match Lib.String.chop_prefix "mo:" f with
  | Some suffix ->
    begin match Stdlib.String.index_opt suffix '/' with
    | None ->
      if suffix = "prim"
      then Ok Prim
      else Ok (Package (suffix, ""))
    | Some i ->
      if suffix = "prim"
      then Error "The prim package has no modules"
      else
        let pkg = Stdlib.String.sub suffix 0 i in
        let path = Stdlib.String.sub suffix (i+1) (Stdlib.String.length suffix - (i+1)) in
        Ok (Package (pkg, path))
    end
  | None ->
    match Lib.String.chop_prefix "ic:" f with
    | Some principal-> begin match decode_prinicpal f with
      | Ok bytes -> Ok (Ic bytes)
      | Error err -> Error err
      end
    | None ->
      match Lib.String.chop_prefix "canister:" f with
      | Some suffix -> Ok (IcAlias suffix)
      | None ->
        begin match Stdlib.String.index_opt f ':' with
        | Some _ -> Error "Unrecognized URL"
        | None -> Ok (Relative (Lib.FilePath.normalise f))
        end


(* Basename of the IDL file searched (see DFX-Interface.md) *)
let idl_basename_of_blob bytes =
  Lib.Hex.hex_of_bytes bytes ^ Lib.Hex.hex_of_byte (Lib.CRC.crc8 bytes) ^ ".did"
