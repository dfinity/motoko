open Value
open Numerics

module T = Mo_types.Type

(* Operators *)

type unop =
  | PosOp                                       (* +x *)
  | NegOp                                       (* -x *)
  | NotOp                                       (* bitwise negation *)

type binop =
  | AddOp                                       (* x+y *)
  | SubOp                                       (* x-y *)
  | MulOp                                       (* x*y *)
  | DivOp                                       (* x/y *)
  | ModOp                                       (* x%y *)
  | PowOp                                       (* x^y *)
  | AndOp                                       (* bitwise operators... *)
  | OrOp
  | XorOp
  | ShLOp
  | UShROp
  | SShROp
  | RotLOp
  | RotROp
  | WrappingAddOp                               (* wrapping operators... *)
  | WrappingSubOp
  | WrappingMulOp
  | WrappingPowOp
  | CatOp                                       (* concatenation *)

type relop =
  | EqOp                                        (* x=y *)
  | NeqOp                                       (* x!=y *)
  | LtOp                                        (* x<y *)
  | GtOp                                        (* x>y *)
  | LeOp                                        (* x<=y *)
  | GeOp                                        (* x>=y *)

let impossible _ = raise (Invalid_argument "operator called for None")


(* Unary operators *)

(* bit-wise unops *)
let bit_unop (fword8, fword16, fword32, fword64) = function
  | T.Word8 -> fun v -> Word8 (fword8 (as_word8 v))
  | T.Word16 -> fun v -> Word16 (fword16 (as_word16 v))
  | T.Word32 -> fun v -> Word32 (fword32 (as_word32 v))
  | T.Word64 -> fun v -> Word64 (fword64 (as_word64 v))
  | _ -> raise (Invalid_argument "unop")

(* types that support sign operations (+, -)  *)
let sign_unop fint (fint8, fint16, fint32, fint64, fword8, fword16, fword32, fword64) ffloat = function
  | T.Int -> fun v -> Int (fint (as_int v))
  | T.Int8 -> fun v -> Int8 (fint8 (as_int8 v))
  | T.Int16 -> fun v -> Int16 (fint16 (as_int16 v))
  | T.Int32 -> fun v -> Int32 (fint32 (as_int32 v))
  | T.Int64 -> fun v -> Int64 (fint64 (as_int64 v))
  | T.Word8 -> fun v -> Word8 (fword8 (as_word8 v))
  | T.Word16 -> fun v -> Word16 (fword16 (as_word16 v))
  | T.Word32 -> fun v -> Word32 (fword32 (as_word32 v))
  | T.Word64 -> fun v -> Word64 (fword64 (as_word64 v))
  | T.Float -> fun v -> Float (ffloat (as_float v))
  | _ -> raise (Invalid_argument "unop")

let unop op t =
  match t with
  | T.Prim p ->
    (match op with
    | PosOp -> let id v = v in sign_unop id (id, id, id, id, id, id, id, id) id p
    | NegOp ->
      sign_unop
        Int.neg
        (Int_8.neg, Int_16.neg, Int_32.neg, Int_64.neg,
         Word8.neg, Word16.neg, Word32.neg, Word64.neg)
        Float.neg
        p
    | NotOp -> bit_unop
      (Word8.not, Word16.not, Word32.not, Word64.not)
      p
    )
  | T.Non -> impossible
  | _ -> raise (Invalid_argument "unop")


(* Binary operators *)

let text_binop ftext = function
  | T.Text -> fun v1 v2 -> Text (ftext (as_text v1) (as_text v2))
  | _ -> raise (Invalid_argument "binop")

let word_binop (fword8, fword16, fword32, fword64) = function
  | T.Word8 -> fun v1 v2 -> Word8 (fword8 (as_word8 v1) (as_word8 v2))
  | T.Word16 -> fun v1 v2 -> Word16 (fword16 (as_word16 v1) (as_word16 v2))
  | T.Word32 -> fun v1 v2 -> Word32 (fword32 (as_word32 v1) (as_word32 v2))
  | T.Word64 -> fun v1 v2 -> Word64 (fword64 (as_word64 v1) (as_word64 v2))
  | _ -> raise (Invalid_argument "binop")

let fixed_binop (fnat8, fnat16, fnat32, fnat64, fint8, fint16, fint32, fint64) fwords = function
  | T.Nat8 -> fun v1 v2 -> Nat8 (fnat8 (as_nat8 v1) (as_nat8 v2))
  | T.Nat16 -> fun v1 v2 -> Nat16 (fnat16 (as_nat16 v1) (as_nat16 v2))
  | T.Nat32 -> fun v1 v2 -> Nat32 (fnat32 (as_nat32 v1) (as_nat32 v2))
  | T.Nat64 -> fun v1 v2 -> Nat64 (fnat64 (as_nat64 v1) (as_nat64 v2))
  | T.Int8 -> fun v1 v2 -> Int8 (fint8 (as_int8 v1) (as_int8 v2))
  | T.Int16 -> fun v1 v2 -> Int16 (fint16 (as_int16 v1) (as_int16 v2))
  | T.Int32 -> fun v1 v2 -> Int32 (fint32 (as_int32 v1) (as_int32 v2))
  | T.Int64 -> fun v1 v2 -> Int64 (fint64 (as_int64 v1) (as_int64 v2))
  | t -> word_binop fwords t

let num_binop fnat fint ffixed fwords ffloat = function
  | T.Nat -> fun v1 v2 -> Int (fnat (as_int v1) (as_int v2))
  | T.Int -> fun v1 v2 -> Int (fint (as_int v1) (as_int v2))
  | T.Float -> fun v1 v2 -> Float (ffloat (as_float v1) (as_float v2))
  | t -> fixed_binop ffixed fwords t

let binop op t =
  match t with
  | T.Prim p ->
    (match op with
    | AddOp -> num_binop Nat.add Int.add (Nat8.add, Nat16.add, Nat32.add, Nat64.add, Int_8.add, Int_16.add, Int_32.add, Int_64.add) (Word8.add, Word16.add, Word32.add, Word64.add) Float.add p
    | SubOp -> num_binop Nat.sub Int.sub (Nat8.sub, Nat16.sub, Nat32.sub, Nat64.sub, Int_8.sub, Int_16.sub, Int_32.sub, Int_64.sub) (Word8.sub, Word16.sub, Word32.sub, Word64.sub) Float.sub p
    | MulOp -> num_binop Nat.mul Int.mul (Nat8.mul, Nat16.mul, Nat32.mul, Nat64.mul, Int_8.mul, Int_16.mul, Int_32.mul, Int_64.mul) (Word8.mul, Word16.mul, Word32.mul, Word64.mul) Float.mul p
    | DivOp -> num_binop Nat.div Int.div (Nat8.div, Nat16.div, Nat32.div, Nat64.div, Int_8.div, Int_16.div, Int_32.div, Int_64.div) (Word8.div, Word16.div, Word32.div, Word64.div) Float.div p
    | ModOp -> num_binop Nat.rem Int.rem (Nat8.rem, Nat16.rem, Nat32.rem, Nat64.rem, Int_8.rem, Int_16.rem, Int_32.rem, Int_64.rem) (Word8.rem, Word16.rem, Word32.rem, Word64.rem) Float.rem p
    | PowOp -> num_binop Nat.pow Int.pow (Nat8.pow, Nat16.pow, Nat32.pow, Nat64.pow, Int_8.pow, Int_16.pow, Int_32.pow, Int_64.pow) (Word8.pow, Word16.pow, Word32.pow, Word64.pow) Float.pow p
    | AndOp -> word_binop (Word8.and_, Word16.and_, Word32.and_, Word64.and_) p
    | OrOp  -> word_binop (Word8.or_, Word16.or_, Word32.or_, Word64.or_) p
    | XorOp -> word_binop (Word8.xor, Word16.xor, Word32.xor, Word64.xor) p
    | ShLOp -> word_binop (Word8.shl, Word16.shl, Word32.shl, Word64.shl) p
    | UShROp -> word_binop (Word8.shr, Word16.shr, Word32.shr, Word64.shr) p
    | SShROp -> word_binop (Word8.shr_s, Word16.shr_s, Word32.shr_s, Word64.shr_s) p
    | RotLOp -> word_binop (Word8.rotl, Word16.rotl, Word32.rotl, Word64.rotl) p
    | RotROp -> word_binop (Word8.rotr, Word16.rotr, Word32.rotr, Word64.rotr) p
    | WrappingAddOp -> fixed_binop (Nat8.wrapping_add, Nat16.wrapping_add, Nat32.wrapping_add, Nat64.wrapping_add, Int_8.wrapping_add, Int_16.wrapping_add, Int_32.wrapping_add, Int_64.wrapping_add) (Word8.wrapping_add, Word16.wrapping_add, Word32.wrapping_add, Word64.wrapping_add) p
    | WrappingSubOp -> fixed_binop (Nat8.wrapping_sub, Nat16.wrapping_sub, Nat32.wrapping_sub, Nat64.wrapping_sub, Int_8.wrapping_sub, Int_16.wrapping_sub, Int_32.wrapping_sub, Int_64.wrapping_sub) (Word8.wrapping_sub, Word16.wrapping_sub, Word32.wrapping_sub, Word64.wrapping_sub) p
    | WrappingMulOp -> fixed_binop (Nat8.wrapping_mul, Nat16.wrapping_mul, Nat32.wrapping_mul, Nat64.wrapping_mul, Int_8.wrapping_mul, Int_16.wrapping_mul, Int_32.wrapping_mul, Int_64.wrapping_mul) (Word8.wrapping_mul, Word16.wrapping_mul, Word32.wrapping_mul, Word64.wrapping_mul) p
    | WrappingPowOp -> fixed_binop (Nat8.wrapping_pow, Nat16.wrapping_pow, Nat32.wrapping_pow, Nat64.wrapping_pow, Int_8.wrapping_pow, Int_16.wrapping_pow, Int_32.wrapping_pow, Int_64.wrapping_pow) (Word8.wrapping_pow, Word16.wrapping_pow, Word32.wrapping_pow, Word64.wrapping_pow) p
    | CatOp -> text_binop (^) p
    )
  | T.Non -> impossible
  | _ -> raise (Invalid_argument "binop")


(* Relational operators *)

let num_relop fnat fint (fnat8, fnat16, fnat32, fnat64, fint8, fint16, fint32, fint64, fword8, fword16, fword32, fword64) ffloat = function
  | T.Nat -> fun v1 v2 -> Bool (fnat (as_int v1) (as_int v2))
  | T.Nat8 -> fun v1 v2 -> Bool (fnat8 (as_nat8 v1) (as_nat8 v2))
  | T.Nat16 -> fun v1 v2 -> Bool (fnat16 (as_nat16 v1) (as_nat16 v2))
  | T.Nat32 -> fun v1 v2 -> Bool (fnat32 (as_nat32 v1) (as_nat32 v2))
  | T.Nat64 -> fun v1 v2 -> Bool (fnat64 (as_nat64 v1) (as_nat64 v2))
  | T.Int -> fun v1 v2 -> Bool (fint (as_int v1) (as_int v2))
  | T.Int8 -> fun v1 v2 -> Bool (fint8 (as_int8 v1) (as_int8 v2))
  | T.Int16 -> fun v1 v2 -> Bool (fint16 (as_int16 v1) (as_int16 v2))
  | T.Int32 -> fun v1 v2 -> Bool (fint32 (as_int32 v1) (as_int32 v2))
  | T.Int64 -> fun v1 v2 -> Bool (fint64 (as_int64 v1) (as_int64 v2))
  | T.Word8 -> fun v1 v2 -> Bool (fword8 (as_word8 v1) (as_word8 v2))
  | T.Word16 -> fun v1 v2 -> Bool (fword16 (as_word16 v1) (as_word16 v2))
  | T.Word32 -> fun v1 v2 -> Bool (fword32 (as_word32 v1) (as_word32 v2))
  | T.Word64 -> fun v1 v2 -> Bool (fword64 (as_word64 v1) (as_word64 v2))
  | T.Float -> fun v1 v2 -> Bool (ffloat (as_float v1) (as_float v2))
  | _ -> raise (Invalid_argument "relop")

let ord_relop fnat fint fwords ffloat fchar ftext fblob = function
  | T.Char -> fun v1 v2 -> Bool (fchar (as_char v1) (as_char v2))
  | T.Text -> fun v1 v2 -> Bool (ftext (as_text v1) (as_text v2))
  | T.Blob | T.Principal -> fun v1 v2 -> Bool (ftext (as_blob v1) (as_blob v2))
  | t -> num_relop fnat fint fwords ffloat t

let eq_relop fnat fint fwords ffloat fchar ftext fblob fnull fbool = function
  | T.Null -> fun v1 v2 -> Bool (fnull (as_null v1) (as_null v2))
  | T.Bool -> fun v1 v2 -> Bool (fbool (as_bool v1) (as_bool v2))
  | t -> ord_relop fnat fint fwords ffloat fchar ftext fblob t

let eq_prim =
  eq_relop Nat.eq  Int.eq (Nat8.eq, Nat16.eq, Nat32.eq, Nat64.eq, Int_8.eq, Int_16.eq, Int_32.eq, Int_64.eq, Word8.eq, Word16.eq, Word32.eq, Word64.eq) Float.eq (=) (=) (=) (=) (=)

(* Follows the structure of `shared` in mo_type/type.ml *)
let structural_equality t =
  let rec go t =
    match t with
    | T.Var _ | T.Pre | T.Non | T.Async _ | T.Mut _ -> assert false
    | T.Any | T.Typ _ -> fun v1 v2 -> Bool true
    | T.Prim p -> eq_prim p
    | T.Con (c, ts) -> (
        match Mo_types.Con.kind c with
        | T.Abs _ -> assert false
        | T.Def (_, t) -> go (T.open_ ts t) (* TBR this may fail to terminate *)
        )
    | T.Array t ->
        fun v1 v2 ->
          let eq_elem = go t in
          let v1 = as_array v1 in
          let v2 = as_array v2 in
          Bool (
            Array.length v1 == Array.length v2 &&
            Lib.Array.for_all2 (fun x y -> as_bool (eq_elem x y)) v1 v2
          )
    | T.Opt t -> (
        fun v1 v2 ->
          match (v1, v2) with
          | Null, Null -> Bool true
          | Null, Opt _ 
          | Opt _, Null -> Bool false
          | Opt v1, Opt v2 -> go t v1 v2
          | _, _ -> assert false )
    | T.Tup ts ->
        fun v1 v2 ->
          let v1 = as_tup v1 in
          let v2 = as_tup v2 in
          let rec go_inner ts v1 v2 =
            match (ts, v1, v2) with
            | [], [], [] -> true
            | t :: ts, v1 :: v1s, v2 :: v2s ->
                as_bool (go t v1 v2) && go_inner ts v1s v2s
            | _ -> assert false
          in
          Bool (go_inner ts v1 v2)
    | T.Obj (s, fs) -> (
        match s with
        | T.Actor ->
            fun v1 v2 ->
              (match (v1, v2) with
               | Blob s1, Blob s2 -> Bool (s1 = s2)
               | _, _ -> Bool (v1 == v2) (* HACK *))
        | T.Module | T.Memory -> assert false
        | T.Object ->
            fun v1 v2 ->
              let v1 = as_obj v1 in
              let v2 = as_obj v2 in
              Bool
                (List.for_all
                   (fun f ->
                     as_bool
                       (go f.T.typ (Env.find f.T.lab v1) (Env.find f.T.lab v2)))
                   fs) )
    | T.Variant fs ->
        fun v1 v2 ->
          let l1, v1 = as_variant v1 in
          let l2, v2 = as_variant v2 in
          if l1 <> l2 then Bool false
          else
            go (List.find (fun f -> f.T.lab = l1) fs).T.typ v1 v2
    | T.Func (s, c, tbs, ts1, ts2) ->
        assert (T.is_shared_sort s);
        fun v1 v2 -> Bool (v1 == v2)  (* HACK *)
  in
  go t

let relop op t =
  match t with
  | T.Prim p ->
    (match op with
    | EqOp -> eq_prim p
    | NeqOp -> eq_relop Nat.ne Int.ne (Nat8.ne, Nat16.ne, Nat32.ne, Nat64.ne, Int_8.ne, Int_16.ne, Int_32.ne, Int_64.ne, Word8.ne, Word16.ne, Word32.ne, Word64.ne) Float.ne (<>) (<>) (<>) (<>) (<>) p
    | LtOp -> eq_relop Nat.lt Int.lt (Nat8.lt, Nat16.lt, Nat32.lt, Nat64.lt, Int_8.lt, Int_16.lt, Int_32.lt, Int_64.lt, Word8.lt, Word16.lt, Word32.lt, Word64.lt) Float.lt (<) (<) (<) (<) (<) p
    | GtOp -> eq_relop Nat.gt Int.gt (Nat8.gt, Nat16.gt, Nat32.gt, Nat64.gt, Int_8.gt, Int_16.gt, Int_32.gt, Int_64.gt, Word8.gt, Word16.gt, Word32.gt, Word64.gt) Float.gt (>) (>) (>) (>) (>) p
    | LeOp -> eq_relop Nat.le Int.le (Nat8.le, Nat16.le, Nat32.le, Nat64.le, Int_8.le, Int_16.le, Int_32.le, Int_64.le, Word8.le, Word16.le, Word32.le, Word64.le) Float.le (<=) (<=) (<=) (<=) (<=) p
    | GeOp -> eq_relop Nat.ge Int.ge (Nat8.ge, Nat16.ge, Nat32.ge, Nat64.ge, Int_8.ge, Int_16.ge, Int_32.ge, Int_64.ge, Word8.ge, Word16.ge, Word32.ge, Word64.ge) Float.ge (>=) (>=) (>=) (>=) (>=) p
    )
  | T.Non -> impossible
  | t when op = EqOp && T.shared t ->
    structural_equality t
  | t when op = NeqOp && T.shared t ->
    fun v1 v2 -> Bool (not (as_bool (structural_equality t v1 v2)))
  | _ -> raise (Invalid_argument "relop")


let has f op t = try ignore (f op t); true with Invalid_argument _ -> false
let has_unop op t = has unop op t
let has_binop op t = has binop op t
let has_relop op t = has relop op t

let type_unop op t = if t = T.nat then T.int else t
let type_binop op t = t
let type_relop op t = t
