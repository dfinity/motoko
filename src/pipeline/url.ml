(* Parsing known URLs from mo: and ic: URLs *)

(*
   parse "mo:std/list"    = Ok (Package ("std", "list"))
   parse "mo:std/foo/bar" = Ok (Package ("std", "foo/bar"))
   parse "mo:foo/bar"     = Ok (Package ("foo", "bar"))
   parse "mo:foo"         = Ok (Package ("foo", ""))

   parse "ic:DEADBEEF"    = Ok (Ic "\DE\AD\BE\EF")

   parse "ic-alias:foo"   = Ok (IcAlias "foo")

   parse "std/foo"        = Ok (Relative "std/foo")
   parse "foo"            = Ok (Relative "foo")
   parse "./foo"          = Ok (Relative "foo")

   parse "something:else" = Error …

   TODO: This could be the place to reject things like
     ic: mo: http:std/foo
  *)


(* helper (only to be used on "ic:…" urls) *)
let decode_actor_url url : (string, string) result =
  let open Stdlib.String in
  let hex = Option.get (Lib.String.chop_prefix "ic:" url) in

  if equal hex "" then Error "principal ID must not be empty" else
  if uppercase_ascii hex <> hex then Error "principal ID must be uppercase" else
  let isHex c = (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') in
  if not (Lib.Seq.for_all isHex (to_seq hex)) then Error "principal ID must contain uppercase hexadecimal digits" else
  if length hex mod 2 = 1 then Error "principal ID must contain an even number of hexadecimal digits" else
  let blob, crc = sub hex 0 (length hex - 2), sub hex (length hex - 2) 2 in
  let bs = Lib.Hex.bytes_of_hex blob in
  let checksum = Lib.CRC.crc8 bs in
  if checksum <> Lib.Hex.int_of_hex_byte crc then Error "invald checksum in principal ID, please check for typos" else
  Ok bs

type parsed =
  | Package of (string * string)
  | Relative of string
  | Ic of string
  | IcAlias of string


let parse (f: string) : (parsed, string) result =
  match Lib.String.chop_prefix "mo:" f with
  | Some suffix ->
    begin match Stdlib.String.index_opt suffix '/' with
    | None -> Ok (Package (suffix, ""))
    | Some i ->
      let pkg = Stdlib.String.sub suffix 0 i in
      let path = Stdlib.String.sub suffix (i+1) (Stdlib.String.length suffix - (i+1)) in
      Ok (Package (pkg, path))
    end
  | None ->
    match Lib.String.chop_prefix "ic:" f with
    | Some _suffix -> begin match decode_actor_url f with
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
