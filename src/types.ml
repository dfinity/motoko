
(*we represent a n-bit integers using k-bit (n<=k) integers by shifting left/right by k-n bits *)
module SubRep(Rep:Wasm.Int.RepType)(Width:sig val bitwidth:int end):Wasm.Int.RepType with type t = Rep.t =
struct
    type t = Rep.t
    let bitwidth = Width.bitwidth
    let _ = assert(Width.bitwidth < Rep.bitwidth)
    let bitdiff = Rep.bitwidth - Width.bitwidth
    let inj r  = Rep.shift_left r bitdiff
    let proj i = Rep.shift_right i bitdiff
    let zero = inj Rep.zero
    let one = inj Rep.one
    let minus_one = inj Rep.minus_one
    let max_int = inj (Rep.shift_right_logical Rep.max_int bitdiff)
    let min_int = inj (Rep.shift_right_logical Rep.min_int bitdiff)
    let neg i = inj (Rep.neg (proj i))
    let add i j = inj (Rep.add (proj i) (proj j))
    let sub i j = inj (Rep.sub (proj i) (proj j))
    let mul i j = inj (Rep.mul (proj i) (proj j)) 
    let div i j = inj (Rep.div (proj i) (proj j))
    let rem i j = inj (Rep.rem (proj i) (proj j))
    let logand = Rep.logand
    let logor = Rep.logor
    let lognot i = inj (Rep.lognot (proj i))
    let logxor i j = inj (Rep.logxor (proj i) (proj j))
    let shift_left i j = Rep.shift_left i j
    let shift_right = Rep.shift_right
    let shift_right_logical = Rep.shift_right_logical
    let of_int i = inj (Rep.of_int i)
    let to_int i = Rep.to_int (proj i)
    let to_string i = Rep.to_string (proj i)
end

module Int32Rep =
           struct
            include Int32
            let bitwidth = 32
           end

module Int8Rep = SubRep(Int32Rep)(struct let bitwidth = 8 end)
module Int16Rep = SubRep(Int32Rep)(struct let bitwidth = 16 end)

module Word8 = Wasm.Int.Make(Int8Rep)
module Word16 = Wasm.Int.Make(Int16Rep)
module Word32 = Wasm.I32
module Word64 = Wasm.I64

module Natural = Wasm.Int.Make(Int32Rep)
module Integer = Wasm.Int.Make(Int32Rep)
module Float = Wasm.F64

let nat_width = 32
let int_width = 32

type nat = Natural.t
type unicode = int32

type width =
  | Width8
  | Width16
  | Width32
  | Width64

type prim =
  | NullT
  | BoolT
  | NatT
  | IntT
  | WordT of width
  | FloatT
  | CharT
  | TextT

type word =
  | Word8 of Word8.t
  | Word16 of Word16.t
  | Word32 of Word32.t
  | Word64 of Word64.t

type lit =
  | NullLit
  | BoolLit of bool
  | NatLit of Natural.t
  | IntLit of Integer.t
  | WordLit of word
  | FloatLit of Float.t
  | CharLit of unicode
  | TextLit of string
  | PreLit of string                             (* unresolved numeric literal *)

type unop =
  | PosOp                                        (* +x *)
  | NegOp                                        (* -x *)
  | NotOp                                        (* bitwise negation *)

type binop =
  | AddOp                                        (* x+y *)
  | SubOp                                        (* x-y *)
  | MulOp                                        (* x*y *)
  | DivOp                                        (* x/y *)
  | ModOp                                        (* x%y *)
  | AndOp                                        (* bitwise operators... *)
  | OrOp
  | XorOp
  | ShiftLOp
  | ShiftROp
  | RotLOp
  | RotROp
  | CatOp                                        (* concatenation *)

type relop =
  | EqOp                                        (* x=y *)
  | NeqOp                                       (* x!=y *)
  | LtOp                                        (* x<y *)
  | LeOp                                        (* x<=y *)
  | GtOp                                        (* x>y *)
  | GeOp                                        (* x>=y *)


(* Environments *)

module Env = Map.Make(String) 

let union env1 env2 = Env.union (fun k v1 v2 -> Some v2) env1 env2
let lookup env k =
  try Some (Env.find k env) with Not_found -> None (* TODO: use find_opt in 4.05 *)


(* Unique constructors, stamped *)

module Con =
struct
  type t = {name : string; stamp : int}
  let compare (c1 : t) (c2 : t) = compare c1.stamp c2.stamp
  let stamp = ref 0
  let fresh var =
    stamp := !stamp + 1;
    {name = var; stamp = !stamp}
  let to_string con = Printf.sprintf "%s/%i" con.name con.stamp
end

module ConEnv = Map.Make(Con)

let lookup_con map k = try Some (ConEnv.find k map) with Not_found -> None (* TODO: use find_opt in 4.05 *)


type con = Con.t
type mut = ConstMut | VarMut
type actor = Object | Actor

type typ =
  | VarT of con * typ list                     (* constructor *)
  | PrimT of prim                              (* primitive *)
  | ObjT of actor * typ_field list             (* object *)
  | ArrayT of mut * typ                        (* array *)
  | OptT of typ                                (* option *)
  | TupT of typ list                           (* tuple *)
  | FuncT of typ_bind list * typ * typ         (* function *)
  | AsyncT of typ                              (* future *)
  | LikeT of typ                               (* expansion *)
  | AnyT                                       (* top *)
(*
  | UnionT of type * typ                       (* union *)
  | AtomT of string                            (* atom *)
*)

and typ_bind = {var : con; bound : typ}
and typ_field = {var : string; typ : typ; mut : mut}

type kind =
  | DefK of typ_bind list * typ
  | ObjK of typ_bind list * actor * typ_field list
  | ParK of typ_bind list * typ


let unitT = TupT []
let boolT = PrimT BoolT
let intT = PrimT IntT


(* Poor man's pretty printing - replace with Format client *)

open Printf

let string_of_mut = function
  | VarMut -> " var "
  | ConstMut -> ""

let string_of_width = function
  | Width8 -> "8"
  | Width16 -> "16"
  | Width32 -> "32"
  | Width64 -> "64"

let string_of_prim = function
  | NullT -> "Null"
  | IntT -> "Int"
  | BoolT -> "Bool"
  | FloatT -> "Float"
  | NatT -> "Nat"
  | WordT w -> "Word" ^ string_of_width w
  | CharT -> "Char"
  | TextT -> "Text"

let rec string_of_atomic_typ = function
  | AnyT -> "Any"
  | PrimT p -> string_of_prim p
  | VarT (c, []) -> Con.to_string c
  | VarT (c, ts) ->
    sprintf "%s<%s>"
      (Con.to_string c) (String.concat "," (List.map string_of_typ ts))
  | TupT ts ->
    sprintf "(%s)" (String.concat "," (List.map string_of_typ ts))
  | ObjT (Object, fs) ->
    sprintf "{%s}" (String.concat ";" (List.map string_of_typ_field fs))
  | t ->
    sprintf "(%s)" (string_of_typ t)

and string_of_typ t =
  match t with
  | ArrayT (m, t) ->
    sprintf "%s%s[]" (string_of_mut m) (string_of_atomic_typ t)  
  | FuncT (tbs, t1, t2) ->
    sprintf "%s%s->%s"
      (string_of_typ_binds tbs) (string_of_atomic_typ t1) (string_of_typ t2)
  | OptT t ->
    sprintf "%s?"  (string_of_atomic_typ t)
  | AsyncT t -> 
    sprintf "async %s" (string_of_atomic_typ t)
  | LikeT t -> 
    sprintf "like %s" (string_of_atomic_typ t)
  | ObjT (Actor, fs) ->
    sprintf "actor%s" (string_of_atomic_typ (ObjT (Object, fs)))
  | t -> string_of_atomic_typ t

and string_of_typ_field {var; mut; typ} =
  sprintf "%s:%s %s" var (string_of_mut mut) (string_of_typ typ)

and string_of_typ_bind {var; bound} =
  (* TODO: print bound *)
  Con.to_string var

and string_of_typ_binds = function
  | [] -> ""
  | tbs -> "<" ^ (String.concat "," (List.map string_of_typ_bind tbs)) ^ ">"

let string_of_kind = function
  | DefK (tbs, t) ->
    sprintf "= %s%s" (string_of_typ_binds tbs) (string_of_typ t)
  | ObjK (tbs, actor, fs) -> 
    sprintf ":= %s%s" (string_of_typ_binds tbs) (string_of_typ (ObjT (actor, fs)))
  | ParK (tbs, t) -> 
    sprintf ":: %s%s" (string_of_typ_binds tbs) (string_of_typ t) 


(* First-order substitutions *)

type subst = typ ConEnv.t

let rec rename_binds sigma = function
  | [] -> sigma, []
  | {var = con; bound}::binds ->
    let con' = Con.fresh con.name in
    let sigma' = ConEnv.add con (VarT (con', [])) sigma in
    let (rho, binds') = rename_binds sigma' binds in
    rho, {var = con'; bound = subst rho bound}::binds'

and subst sigma = function
  | PrimT p -> PrimT p
  | VarT (c, ts) ->
    (match lookup_con sigma c with
    | Some t -> assert (List.length ts = 0); t
    | None -> VarT (c, List.map (subst sigma) ts)
    )
  | ArrayT (m, t) ->
    ArrayT (m, subst sigma t)
  | TupT ts ->
    TupT (List.map (subst sigma) ts)
  | FuncT(ts, t1, t2) ->
    let (rho, ts') = rename_binds sigma ts in
    FuncT (ts', subst rho t1, subst rho t2)
  | OptT t ->
    OptT (subst sigma t)
  | AsyncT t ->
    AsyncT (subst sigma t)
  | LikeT t ->
    LikeT (subst sigma t)
  | ObjT (a, fs) ->
    ObjT (a, subst_fields sigma fs)

and subst_fields sigma fs = 
  List.map (fun {var; mut; typ} -> {var; mut; typ = subst sigma typ}) fs


let rec substitute us ts = substitute' ConEnv.empty us ts
and substitute' sigma us ts =
  match us, ts with
  | [], [] -> sigma
  | u::us, {var; bound}::ts ->
    substitute' (ConEnv.add var u sigma) us ts
  | _ -> raise (Invalid_argument "substitute")
