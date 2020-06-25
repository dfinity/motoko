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
  | ParT of typ                                    (* parentheses, used to control function arity only *)
  | NamedT of (id * typ)                           (* parenthesized single element named "tuple" *)

and scope = typ
and typ_field = typ_field' Source.phrase
and typ_field' = {id : id; typ : typ; mut : mut}

and typ_tag = typ_tag' Source.phrase
and typ_tag' = {tag : id; typ : typ}

and bind_sort = Type.bind_sort Source.phrase
and typ_bind = (typ_bind', Type.con option) Source.annotated_phrase
and typ_bind' = {var : id; sort : bind_sort; bound : typ;}

and typ_item = (id option * typ)


(* Literals *)

type lit =
  | NullLit
  | BoolLit of bool
  | NatLit of Value.Nat.t
  | Nat8Lit of Value.Nat8.t
  | Nat16Lit of Value.Nat16.t
  | Nat32Lit of Value.Nat32.t
  | Nat64Lit of Value.Nat64.t
  | IntLit of Value.Int.t
  | Int8Lit of Value.Int_8.t
  | Int16Lit of Value.Int_16.t
  | Int32Lit of Value.Int_32.t
  | Int64Lit of Value.Int_64.t
  | Word8Lit of Value.Word8.t
  | Word16Lit of Value.Word16.t
  | Word32Lit of Value.Word32.t
  | Word64Lit of Value.Word64.t
  | FloatLit of Value.Float.t
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
and vis' = Public | Private | System

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
  | TupE of exp list                           (* tuple *)
  | ProjE of exp * int                         (* tuple projection *)
  | OptE of exp                                (* option injection *)
  | ObjE of obj_sort * exp_field list          (* object *)
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
(*
  | FinalE of exp * exp                        (* finally *)
  | AtomE of string                            (* atom *)
*)

and exp_field = exp_field' Source.phrase
and exp_field' = {dec : dec; vis : vis; stab: stab option}

and case = case' Source.phrase
and case' = {pat : pat; exp : exp}


(* Declarations *)

and dec = (dec', typ_note) Source.annotated_phrase
and dec' =
  | ExpD of exp                                (* plain unit expression *)
  | IgnoreD of exp                             (* plain any expression *)
  | LetD of pat * exp                          (* immutable *)
  | VarD of id * exp                           (* mutable *)
  | TypD of typ_id * typ_bind list * typ       (* type *)
  | ClassD of                                  (* class *)
      typ_id * typ_bind list * pat * typ option * obj_sort * id * exp_field list


(* Program *)

type prog = (prog', string) Source.annotated_phrase
and prog' = dec list


(* Libraries *)

type lib = (exp, string) Source.annotated_phrase


(* n-ary arguments/result sequences *)

let arity t =
  match t.Source.it with
  | TupT ts -> List.length ts
  | _ -> 1

(* Literals *)

(* NB: This function is currently unused *)
let string_of_lit = function
  | BoolLit false -> "false"
  | BoolLit true  ->  "true"
  | IntLit n
  | NatLit n      -> Value.Int.to_pretty_string n
  | Int8Lit n     -> Value.Int_8.to_pretty_string n
  | Int16Lit n    -> Value.Int_16.to_pretty_string n
  | Int32Lit n    -> Value.Int_32.to_pretty_string n
  | Int64Lit n    -> Value.Int_64.to_pretty_string n
  | Nat8Lit n     -> Value.Nat8.to_pretty_string n
  | Nat16Lit n    -> Value.Nat16.to_pretty_string n
  | Nat32Lit n    -> Value.Nat32.to_pretty_string n
  | Nat64Lit n    -> Value.Nat64.to_pretty_string n
  | Word8Lit n    -> Value.Word8.to_pretty_string n
  | Word16Lit n   -> Value.Word16.to_pretty_string n
  | Word32Lit n   -> Value.Word32.to_pretty_string n
  | Word64Lit n   -> Value.Word64.to_pretty_string n
  | CharLit c     -> string_of_int c
  | NullLit       -> "null"
  | TextLit t     -> t
  | BlobLit b     -> b
  | FloatLit f    -> Value.Float.to_pretty_string f
  | PreLit _      -> assert false


open Source
let (@@) = Source.(@@)
let (@?) it at = Source.({it; at; note = empty_typ_note})
let (@!) it at = Source.({it; at; note = Type.Pre})
let (@=) it at = Source.({it; at; note = None})

let scope_typ region =
  Source.(
    { it = PathT (
      { it = IdH { it = Type.default_scope_var; at = region; note = () };
        at = no_region;
        note = Type.Pre },
      []);
    at = region;
    note = Type.Pre })

let scope_bind var =
  { var = Type.scope_var var @@ no_region;
    sort = Type.Scope @@ no_region;
    bound = PrimT "Any" @! no_region
  } @= no_region

let ensure_scope_bind var tbs =
  match tbs with
  | tb::_ when tb.it.sort.it = Type.Scope ->
    tbs
  | _ ->
    scope_bind var::tbs


let funcT (sort, tbs, t1, t2) =
  match sort.it, t2.it with
  | Type.Local, AsyncT _ ->
    FuncT(sort, ensure_scope_bind "" tbs, t1, t2)
  | Type.Shared _, _ ->
    FuncT(sort, ensure_scope_bind "" tbs, t1, t2)
  | _ ->
    FuncT(sort, tbs, t1, t2)

let is_Async e =
  match e.it with
  | AsyncE _ -> true
  | _ -> false

let is_IgnoreAsync e =
  match e.it with
  | BlockE [ { it = IgnoreD
      { it = AnnotE ({ it = AsyncE _; _},
        { it = AsyncT (_, { it = TupT[]; _}); _}); _}; _}] ->
    true
  | _ -> false

let async f e =
  AsyncE (scope_bind f, e)  @? e.at

let ignoreAsync f e =
  BlockE [ IgnoreD (
    AnnotE (AsyncE (scope_bind f, e)  @? e.at,
      AsyncT (scope_typ e.at,TupT[] @! e.at) @! e.at) @? e.at ) @? e.at] @? e.at

let desugar sp f t_opt (sugar, e) =
  match sugar, e with
  | (false, e) -> false, e (* body declared as EQ e *)
  | (true, e) -> (* body declared as immediate block *)
    match sp.it, t_opt with
    | _, Some {it = AsyncT _; _} ->
      true, async f.it e
    | Type.Shared _, (None | Some { it = TupT []; _}) ->
      true, ignoreAsync f.it e
    | _, _ -> (true, e)

let funcE (f, s, tbs, p, t_opt, sugar, e) =
  match s.it, t_opt, e with
  | Type.Local, Some { it = AsyncT _; _}, {it = AsyncE _; _}
  | Type.Shared _, _, _ ->
    FuncE(f, s, ensure_scope_bind "" tbs, p, t_opt, sugar, e)
  | _ ->
    FuncE(f, s, tbs, p, t_opt, sugar, e)

let is_any t =
  match t.it with
  | PrimT "Any" -> true
  | _ -> false
