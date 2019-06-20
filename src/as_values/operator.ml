open Value

module T = As_types.Type

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
  | CatOp                                       (* concatenation *)

type relop =
  | EqOp                                        (* x=y *)
  | NeqOp                                       (* x!=y *)
  | LtOp                                        (* x<y *)
  | GtOp                                        (* x>y *)
  | LeOp                                        (* x<=y *)
  | GeOp                                        (* x>=y *)

let impossible _ = raise (Invalid_argument "impossible")


(* Unary operators *)

let word_unop (fword8, fword16, fword32, fword64) = function
  | T.Word8 -> fun v -> Word8 (fword8 (as_word8 v))
  | T.Word16 -> fun v -> Word16 (fword16 (as_word16 v))
  | T.Word32 -> fun v -> Word32 (fword32 (as_word32 v))
  | T.Word64 -> fun v -> Word64 (fword64 (as_word64 v))
  | _ -> raise (Invalid_argument "unop")

let num_unop fint (fint8, fint16, fint32, fint64) wordops ffloat = function
  | T.Int -> fun v -> Int (fint (as_int v))
  | T.Int8 -> fun v -> Int8 (fint8 (as_int8 v))
  | T.Int16 -> fun v -> Int16 (fint16 (as_int16 v))
  | T.Int32 -> fun v -> Int32 (fint32 (as_int32 v))
  | T.Int64 -> fun v -> Int64 (fint64 (as_int64 v))
  | T.Float -> fun v -> Float (ffloat (as_float v))
  | t -> word_unop wordops t

let unop t op =
  match t with
  | T.Prim p ->
    (match op with
    | PosOp -> let id v = v in num_unop id  (id, id, id, id) (id, id, id, id) id p
    | NegOp ->
      num_unop
        Int.neg
        (Int_8.neg, Int_16.neg, Int_32.neg, Int_64.neg)
        (Word8.neg, Word16.neg, Word32.neg, Word64.neg)
        Float.neg
        p
    | NotOp -> word_unop (Word8.not, Word16.not, Word32.not, Word64.not) p
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

let num_binop fnat fint (fint8, fint16, fint32, fint64) fwords ffloat = function
  | T.Nat -> fun v1 v2 -> Int (fnat (as_int v1) (as_int v2))
  | T.Int -> fun v1 v2 -> Int (fint (as_int v1) (as_int v2))
  | T.Int8 -> fun v1 v2 -> Int8 (fint8 (as_int8 v1) (as_int8 v2))
  | T.Int16 -> fun v1 v2 -> Int16 (fint16 (as_int16 v1) (as_int16 v2))
  | T.Int32 -> fun v1 v2 -> Int32 (fint32 (as_int32 v1) (as_int32 v2))
  | T.Int64 -> fun v1 v2 -> Int64 (fint64 (as_int64 v1) (as_int64 v2))
  | T.Float -> fun v1 v2 -> Float (ffloat (as_float v1) (as_float v2))
  | t -> word_binop fwords t

let binop t op =
  match t with
  | T.Prim p ->
    (match op with
    | AddOp -> num_binop Nat.add Int.add (Int_8.add, Int_16.add, Int_32.add, Int_64.add) (Word8.add, Word16.add, Word32.add, Word64.add) Float.add p
    | SubOp -> num_binop Nat.sub Int.sub (Int_8.sub, Int_16.sub, Int_32.sub, Int_64.sub) (Word8.sub, Word16.sub, Word32.sub, Word64.sub) Float.sub p
    | MulOp -> num_binop Nat.mul Int.mul (Int_8.mul, Int_16.mul, Int_32.mul, Int_64.mul) (Word8.mul, Word16.mul, Word32.mul, Word64.mul) Float.mul p
    | DivOp -> num_binop Nat.div Int.div (Int_8.div, Int_16.div, Int_32.div, Int_64.div) (Word8.div_u, Word16.div_u, Word32.div_u, Word64.div_u) Float.div p
    | ModOp -> num_binop Nat.rem Int.rem (Int_8.rem, Int_16.rem, Int_32.rem, Int_64.rem) (Word8.rem_u, Word16.rem_u, Word32.rem_u, Word64.rem_u) Float.div p (* TBR *)
    | PowOp -> num_binop Nat.pow Int.pow (Int_8.pow, Int_16.pow, Int_32.pow, Int_64.pow) (Word8.pow, Word16.pow, Word32.pow, Word64.pow) Float.pow p (* TBR *)
    | AndOp -> word_binop (Word8.and_, Word16.and_, Word32.and_, Word64.and_) p
    | OrOp  -> word_binop (Word8.or_, Word16.or_, Word32.or_, Word64.or_) p
    | XorOp -> word_binop (Word8.xor, Word16.xor, Word32.xor, Word64.xor) p
    | ShLOp -> word_binop (Word8.shl, Word16.shl, Word32.shl, Word64.shl) p
    | UShROp -> word_binop (Word8.shr_u, Word16.shr_u, Word32.shr_u, Word64.shr_u) p
    | SShROp -> word_binop (Word8.shr_s, Word16.shr_s, Word32.shr_s, Word64.shr_s) p
    | RotLOp -> word_binop (Word8.rotl, Word16.rotl, Word32.rotl, Word64.rotl) p
    | RotROp -> word_binop (Word8.rotr, Word16.rotr, Word32.rotr, Word64.rotr) p
    | CatOp -> text_binop (^) p
    )
  | T.Non -> impossible
  | _ -> raise (Invalid_argument "binop")


(* Relational operators *)

let word_relop (fword8, fword16, fword32, fword64) = function
  | T.Word8 -> fun v1 v2 -> Bool (fword8 (as_word8 v1) (as_word8 v2))
  | T.Word16 -> fun v1 v2 -> Bool (fword16 (as_word16 v1) (as_word16 v2))
  | T.Word32 -> fun v1 v2 -> Bool (fword32 (as_word32 v1) (as_word32 v2))
  | T.Word64 -> fun v1 v2 -> Bool (fword64 (as_word64 v1) (as_word64 v2))
  | _ -> raise (Invalid_argument "relop")

let num_relop fnat (fnat8, fnat16, fnat32, fnat64) fint (fint8, fint16, fint32, fint64) fwords ffloat = function
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
  | T.Float -> fun v1 v2 -> Bool (ffloat (as_float v1) (as_float v2))
  | t -> word_relop fwords t

let ord_relop fnat fnats fint fints fwords ffloat fchar ftext = function
  | T.Char -> fun v1 v2 -> Bool (fchar (as_char v1) (as_char v2))
  | T.Text -> fun v1 v2 -> Bool (ftext (as_text v1) (as_text v2))
  | t -> num_relop fnat fnats fint fints fwords ffloat t

let eq_relop fnat fnats fint fints fwords ffloat fchar ftext fnull fbool = function
  | T.Null -> fun v1 v2 -> Bool (fnull (as_null v1) (as_null v2))
  | T.Bool -> fun v1 v2 -> Bool (fbool (as_bool v1) (as_bool v2))
  | t -> ord_relop fnat fnats fint fints fwords ffloat fchar ftext t

let relop t op =
  match t with
  | T.Prim p -> 
    (match op with
    | EqOp -> eq_relop Nat.eq (Nat8.eq, Nat16.eq, Nat32.eq, Nat64.eq) Int.eq (Int_8.eq, Int_16.eq, Int_32.eq, Int_64.eq) (Word8.eq, Word16.eq, Word32.eq, Word64.eq) Float.eq (=) (=) (=) (=) p
    | NeqOp -> eq_relop Nat.ne (Nat8.ne, Nat16.ne, Nat32.ne, Nat64.ne) Int.ne (Int_8.ne, Int_16.ne, Int_32.ne, Int_64.ne) (Word8.ne, Word16.ne, Word32.ne, Word64.ne) Float.ne (<>) (<>) (<>) (<>) p
    | LtOp -> ord_relop Nat.lt (Nat8.lt, Nat16.lt, Nat32.lt, Nat64.lt) Int.lt (Int_8.lt, Int_16.lt, Int_32.lt, Int_64.lt) (Word8.lt_u, Word16.lt_u, Word32.lt_u, Word64.lt_u) Float.lt (<) (<) p
    | GtOp -> ord_relop Nat.gt (Nat8.gt, Nat16.gt, Nat32.gt, Nat64.gt) Int.gt (Int_8.gt, Int_16.gt, Int_32.gt, Int_64.gt) (Word8.gt_u, Word16.gt_u, Word32.gt_u, Word64.gt_u) Float.gt (>) (>) p
    | LeOp -> ord_relop Nat.le (Nat8.le, Nat16.le, Nat32.le, Nat64.le) Int.le (Int_8.le, Int_16.le, Int_32.le, Int_64.le) (Word8.le_u, Word16.le_u, Word32.le_u, Word64.le_u) Float.le (<=) (<=) p
    | GeOp -> ord_relop Nat.ge (Nat8.ge, Nat16.ge, Nat32.ge, Nat64.ge) Int.ge (Int_8.ge, Int_16.ge, Int_32.ge, Int_64.ge) (Word8.ge_u, Word16.ge_u, Word32.ge_u, Word64.ge_u) Float.ge (>=) (>=) p
    )
  | T.Non -> impossible
  | _ -> raise (Invalid_argument "relop")


let has f t op = try ignore (f t op); true with Invalid_argument _ -> false
let has_unop t op = has unop t op
let has_binop t op = has binop t op
let has_relop t op = has relop t op
