open Mo_types
open Mo_values

open Operator


(* Notes *)

type typ_note = {note_typ : Type.typ; note_eff : Type.eff}

let empty_typ_note = {note_typ = Type.Pre; note_eff = Type.Triv}

(* Resolved imports (filled in separately after parsing) *)

type resolved_import =
  | Unresolved
  | LibPath of string
  | IDLPath of (string * string) (* filepath * bytes *)
  | PrimPath (* the built-in prim module *)

(* Identifiers *)

type id = string Source.phrase
type typ_id = (string, Type.con option) Source.annotated_phrase


(* Types *)

type obj_sort = Type.obj_sort Source.phrase
type func_sort = Type.func_sort Source.phrase

type mut = mut' Source.phrase
and mut' = Const | Var

and path = (path', Type.typ) Source.annotated_phrase
and path' =
  | IdH  of id
  | DotH of path * id

type typ = (typ', Type.typ) Source.annotated_phrase
and typ' =
  | PathT of path * typ list                       (* type path *)
  | PrimT of string                                (* primitive *)
  | ObjT of obj_sort * typ_field list              (* object *)
  | ArrayT of mut * typ                            (* array *)
  | OptT of typ                                    (* option *)
  | VariantT of typ_tag list                       (* variant *)
  | TupT of typ_item list                          (* tuple *)
  | FuncT of func_sort * typ_bind list * typ * typ (* function *)
  | AsyncT of scope * typ                          (* future *)
  | AndT of typ * typ                              (* intersection *)
  | OrT of typ * typ                               (* union *)
  | ParT of typ                                    (* parentheses, used to control function arity only *)
  | NamedT of id * typ                             (* parenthesized single element named "tuple" *)

and scope = typ
and typ_field = typ_field' Source.phrase
and typ_field' = {id : id; typ : typ; mut : mut}

and typ_tag = typ_tag' Source.phrase
and typ_tag' = {tag : id; typ : typ}

and bind_sort = Type.bind_sort Source.phrase
and typ_bind = (typ_bind', Type.con option) Source.annotated_phrase
and typ_bind' = {var : id; sort : bind_sort; bound : typ;}

and typ_item = id option * typ


(* Literals *)

type lit =
  | NullLit
  | BoolLit of bool
  | NatLit of Numerics.Nat.t
  | Nat8Lit of Numerics.Nat8.t
  | Nat16Lit of Numerics.Nat16.t
  | Nat32Lit of Numerics.Nat32.t
  | Nat64Lit of Numerics.Nat64.t
  | IntLit of Numerics.Int.t
  | Int8Lit of Numerics.Int_8.t
  | Int16Lit of Numerics.Int_16.t
  | Int32Lit of Numerics.Int_32.t
  | Int64Lit of Numerics.Int_64.t
  | FloatLit of Numerics.Float.t
  | CharLit of Value.unicode
  | TextLit of string
  | BlobLit of string
  | PreLit of string * Type.prim


(* Patterns *)

type pat = (pat', Type.typ) Source.annotated_phrase
and pat' =
  | WildP                                      (* wildcard *)
  | VarP of id                                 (* variable *)
  | LitP of lit ref                            (* literal *)
  | SignP of unop * lit ref                    (* signed literal *)
  | TupP of pat list                           (* tuple *)
  | ObjP of pat_field list                     (* object *)
  | OptP of pat                                (* option *)
  | TagP of id * pat                           (* tagged variant *)
  | AltP of pat * pat                          (* disjunctive *)
  | AnnotP of pat * typ                        (* type annotation *)
  | ParP of pat                                (* parenthesis *)
(*
  | AsP of pat * pat                           (* conjunctive *)
*)

and pat_field = pat_field' Source.phrase
and pat_field' = {id : id; pat : pat}


(* Expressions *)

type vis = vis' Source.phrase
and vis' =
  | Public of string option
  | Private
  | System

let is_public vis = match vis.Source.it with Public _ -> true | _ -> false

type stab = stab' Source.phrase
and stab' = Stable | Flexible

type op_typ = Type.typ ref (* For overloaded resolution; initially Type.Pre. *)


type inst = (typ list option, Type.typ list) Source.annotated_phrase (* For implicit scope instantiation *)

type sort_pat = (Type.shared_sort * pat) Type.shared Source.phrase

type sugar = bool (* Is the source of a function body a block `<block>`,
                     subject to further desugaring during parse,
                     or the invariant form `= <exp>`.
                     In the final output of the parser, the exp in FuncE is
                     always in its fully desugared form and the
                     value of the sugar field is irrelevant.
                     This flag is used to correctly desugar an actor's
                     public functions as oneway, shared functions *)

type exp = (exp', typ_note) Source.annotated_phrase
and exp' =
  | PrimE of string                            (* primitive *)
  | VarE of id                                 (* variable *)
  | LitE of lit ref                            (* literal *)
  | ActorUrlE of exp                           (* actor reference *)
  | UnE of op_typ * unop * exp                 (* unary operator *)
  | BinE of op_typ * exp * binop * exp         (* binary operator *)
  | RelE of op_typ * exp * relop * exp         (* relational operator *)
  | ShowE of (op_typ * exp)                    (* debug show operator *)
  | ToCandidE of exp list                      (* to_candid operator *)
  | FromCandidE of exp                         (* from_candid operator *)
  | TupE of exp list                           (* tuple *)
  | ProjE of exp * int                         (* tuple projection *)
  | OptE of exp                                (* option injection *)
  | DoOptE of exp                              (* option monad *)
  | BangE of exp                               (* scoped option projection *)
  | ObjBlockE of obj_sort * dec_field list     (* object block *)
  | ObjE of exp list * exp_field list          (* record literal/extension *)
  | TagE of id * exp                           (* variant *)
  | DotE of exp * id                           (* object projection *)
  | AssignE of exp * exp                       (* assignment *)
  | ArrayE of mut * exp list                   (* array *)
  | IdxE of exp * exp                          (* array indexing *)
  | FuncE of string * sort_pat * typ_bind list * pat * typ option * sugar * exp  (* function *)
  | CallE of exp * inst * exp                  (* function call *)
  | BlockE of dec list                         (* block (with type after avoidance)*)
  | NotE of exp                                (* negation *)
  | AndE of exp * exp                          (* conjunction *)
  | OrE of exp * exp                           (* disjunction *)
  | IfE of exp * exp * exp                     (* conditional *)
  | SwitchE of exp * case list                 (* switch *)
  | WhileE of exp * exp                        (* while-do loop *)
  | LoopE of exp * exp option                  (* do-while loop *)
  | ForE of pat * exp * exp                    (* iteration *)
  | LabelE of id * typ * exp                   (* label *)
  | BreakE of id * exp                         (* break *)
  | RetE of exp                                (* return *)
  | DebugE of exp                              (* debugging *)
  | AsyncE of typ_bind * exp                   (* async *)
  | AwaitE of exp                              (* await *)
  | AssertE of exp                             (* assertion *)
  | AnnotE of exp * typ                        (* type annotation *)
  | ImportE of (string * resolved_import ref)  (* import statement *)
  | ThrowE of exp                              (* throw exception *)
  | TryE of exp * case list                    (* catch exception *)
  | IgnoreE of exp                             (* ignore *)
(*
  | FinalE of exp * exp                        (* finally *)
  | AtomE of string                            (* atom *)
*)

and dec_field = dec_field' Source.phrase
and dec_field' = {dec : dec; vis : vis; stab: stab option}

and exp_field = exp_field' Source.phrase
and exp_field' = {mut : mut; id : id; exp : exp}

and case = case' Source.phrase
and case' = {pat : pat; exp : exp}


(* Declarations *)

and dec = (dec', typ_note) Source.annotated_phrase
and dec' =
  | ExpD of exp                                (* plain unit expression *)
  | LetD of pat * exp                          (* immutable *)
  | VarD of id * exp                           (* mutable *)
  | TypD of typ_id * typ_bind list * typ       (* type *)
  | ClassD of                                  (* class *)
      sort_pat * typ_id * typ_bind list * pat * typ option * obj_sort * id * dec_field list


(* Program (pre unit detection) *)

type prog_note = { filename : string; trivia : Trivia.triv_table }
type prog = (prog', prog_note) Source.annotated_phrase
and prog' = dec list

(* Signatures (stable variables) *)

type stab_sig = (stab_sig', prog_note) Source.annotated_phrase
and stab_sig' = (dec list * typ_field list)      (* type declarations & stable actor fields *)

(* Compilation units *)

type import = (import', Type.typ) Source.annotated_phrase
and import' = pat * string * resolved_import ref

type comp_unit_body = (comp_unit_body', typ_note) Source.annotated_phrase
and comp_unit_body' =
 | ProgU of dec list                         (* main programs *)
 | ActorU of id option * dec_field list      (* main IC actor *)
 | ModuleU of id option * dec_field list     (* module library *)
 | ActorClassU of                            (* IC actor class, main or library *)
     sort_pat * typ_id * typ_bind list * pat * typ option * id * dec_field list

type comp_unit = (comp_unit', prog_note) Source.annotated_phrase
and comp_unit' = {
  imports : import list;
  body : comp_unit_body;
  }

type lib = comp_unit


(* Helpers *)

let (@@) = Source.(@@)
let (@?) it at = Source.({it; at; note = empty_typ_note})
let (@!) it at = Source.({it; at; note = Type.Pre})
let (@=) it at = Source.({it; at; note = None})


(* NB: This function is currently unused *)
let string_of_lit = function
  | BoolLit false -> "false"
  | BoolLit true  ->  "true"
  | IntLit n
  | NatLit n      -> Numerics.Int.to_pretty_string n
  | Int8Lit n     -> Numerics.Int_8.to_pretty_string n
  | Int16Lit n    -> Numerics.Int_16.to_pretty_string n
  | Int32Lit n    -> Numerics.Int_32.to_pretty_string n
  | Int64Lit n    -> Numerics.Int_64.to_pretty_string n
  | Nat8Lit n     -> Numerics.Nat8.to_pretty_string n
  | Nat16Lit n    -> Numerics.Nat16.to_pretty_string n
  | Nat32Lit n    -> Numerics.Nat32.to_pretty_string n
  | Nat64Lit n    -> Numerics.Nat64.to_pretty_string n
  | CharLit c     -> string_of_int c
  | NullLit       -> "null"
  | TextLit t     -> t
  | BlobLit b     -> b
  | FloatLit f    -> Numerics.Float.to_pretty_string f
  | PreLit _      -> assert false



(* Miscellaneous *)
(* TODO: none of what follows should probably be in this file *)

open Source


(* Identifiers *)

let anon_id sort at = "anon-" ^ sort ^ "-" ^ string_of_pos at.left
let is_anon_id id = Lib.String.chop_prefix "anon-" id.it <> None

(* Types & Scopes *)

let arity t =
  match t.Source.it with
  | TupT ts -> List.length ts
  | _ -> 1

let is_any t =
  match t.it with
  | PrimT "Any" -> true
  | _ -> false

let scopeT at =
  PathT (IdH {it = Type.default_scope_var; at; note = ()} @! at, []) @! at


(* Expressions *)

let asyncE tbs e =
  AsyncE (tbs, e) @? e.at

let ignore_asyncE tbs e =
  IgnoreE (
    AnnotE (AsyncE (tbs, e) @? e.at,
      AsyncT (scopeT e.at, TupT [] @! e.at) @! e.at) @? e.at ) @? e.at

let is_asyncE e =
  match e.it with
  | AsyncE _ -> true
  | _ -> false

let is_ignore_asyncE e =
  match e.it with
  | IgnoreE
      {it = AnnotE ({it = AsyncE _; _},
        {it = AsyncT (_, {it = TupT []; _}); _}); _} ->
    true
  | _ -> false
