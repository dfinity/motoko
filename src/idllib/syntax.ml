
(* Identifiers *)

type id = string Source.phrase

(* Types *)

type prim =
  | Nat
  | Nat8
  | Nat16
  | Nat32
  | Nat64
  | Int
  | Int8
  | Int16
  | Int32
  | Int64
  | Float32
  | Float64
  | Bool
  | Text
  | Null
  | Reserved
  | Empty

type func_mode = func_mode' Source.phrase
and func_mode' = Oneway | Query

type field_label = field_label' Source.phrase
and field_label' = Id of Lib.Uint32.t | Named of string | Unnamed of Lib.Uint32.t

type typ = typ' Source.phrase
and typ' =
  | PrimT of prim                                (* primitive *)
  | VarT of id                                    (* type name *)
  | FuncT of func_mode list * typ list * typ list   (* function *)
  | OptT of typ   (* option *)
  | VecT of typ   (* vector *)
  | BlobT (* vec nat8 *)
  | RecordT of typ_field list  (* record *)
  | VariantT of typ_field list (* variant *)
  | ServT of typ_meth list (* service reference *)
  (* ClassT can only appear in the main actor. *)
  (* This is guarded by the parser and type checker *)
  | ClassT of typ list * typ (* service constructor *)
  | PrincipalT
  | PreT   (* pre-type *)

and typ_field = typ_field' Source.phrase
and typ_field' = { label: field_label; typ : typ }

and typ_meth = typ_meth' Source.phrase
and typ_meth' = {var : id; meth : typ}

(* Declarations *)

and dec = dec' Source.phrase
and dec' =
  | TypD of id * typ             (* type *)
  | ImportD of string * string ref  (* import *)

(* Program *)

type prog = (prog', string) Source.annotated_phrase
and prog' = { decs : dec list; actor : typ option }

(* Values *)

(* This value AST is not to be taken serious. It is just good enough
to translate Candid textual values into morally equivalent Motoko
source code. See mo_idl/idl_to_mo_value.ml *)
type value = value' Source.phrase
and value' =
  | NumV of string (* Candid and Motoko syntax matches, so re-use. Includes floats. *)
  | TextV of string
  | BlobV of string
  | BoolV of bool
  | NullV
  | OptV of value
  | VecV of value list
  | RecordV of field_value list
  | VariantV of field_value
  | ServiceV of string
  | FuncV of (string * string)
  | PrincipalV of string
and field_value = (field_label * value) Source.phrase

type args = value list Source.phrase

(* Tests *)

type input =
  | BinaryInput of string
  | TextualInput of string

type test_assertion =
  | ParsesAs of (bool * input)
  | ParsesEqual of (bool * input * input)

type test' = {
  assertion : test_assertion;
  ttyp : typ list;
  desc : string option;
}
type test = test' Source.phrase

type tests = (tests', string) Source.annotated_phrase
and tests' = { tdecs : dec list; tests : test list }
