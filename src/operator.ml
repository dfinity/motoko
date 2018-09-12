open Syntax
open Value

module T = Type


(* Unary operators *)

let word_unop fword8 fword16 fword32 fword64 = function
  | T.Word8 -> fun v -> Word8 (fword8 (as_word8 v))
  | T.Word16 -> fun v -> Word16 (fword16 (as_word16 v))
  | T.Word32 -> fun v -> Word32 (fword32 (as_word32 v))
  | T.Word64 -> fun v -> Word64 (fword64 (as_word64 v))
  | _ -> raise (Invalid_argument "unop")

let num_unop fint fword8 fword16 fword32 fword64 ffloat = function
  | T.Int -> fun v -> Int (fint (as_int v))
  | T.Float -> fun v -> Float (ffloat (as_float v))
  | t -> word_unop fword8 fword16 fword32 fword64 t

let unop t op =
  match t with
  | T.Prim p ->
    (match op with
    | PosOp -> let id v = v in num_unop id id id id id id p
    | NegOp -> num_unop Int.neg Word8.neg Word16.neg Word32.neg Word64.neg Float.neg p
    | NotOp -> word_unop Word8.not Word16.not Word32.not Word64.not p
    )
  | _ -> raise (Invalid_argument "unop")


(* Binary operators *)

let text_binop ftext = function
  | T.Text -> fun v1 v2 -> Text (ftext (as_text v1) (as_text v2))
  | _ -> raise (Invalid_argument "binop")

let word_binop fword8 fword16 fword32 fword64 = function
  | T.Word8 -> fun v1 v2 -> Word8 (fword8 (as_word8 v1) (as_word8 v2))
  | T.Word16 -> fun v1 v2 -> Word16 (fword16 (as_word16 v1) (as_word16 v2))
  | T.Word32 -> fun v1 v2 -> Word32 (fword32 (as_word32 v1) (as_word32 v2))
  | T.Word64 -> fun v1 v2 -> Word64 (fword64 (as_word64 v1) (as_word64 v2))
  | _ -> raise (Invalid_argument "binop")

let num_binop fnat fint fword8 fword16 fword32 fword64 ffloat = function
  | T.Nat -> fun v1 v2 -> Nat (fnat (as_nat v1) (as_nat v2))
  | T.Int -> fun v1 v2 -> Int (fint (as_int v1) (as_int v2))
  | T.Float -> fun v1 v2 -> Float (ffloat (as_float v1) (as_float v2))
  | t -> word_binop fword8 fword16 fword32 fword64 t

let binop t op =
  match t with
  | T.Prim p ->
    (match op with
    | AddOp -> num_binop Nat.add Int.add Word8.add Word16.add Word32.add Word64.add Float.add p
    | SubOp -> num_binop Nat.sub Int.sub Word8.sub Word16.sub Word32.sub Word64.sub Float.sub p
    | MulOp -> num_binop Nat.mul Int.mul Word8.mul Word16.mul Word32.mul Word64.mul Float.mul p
    | DivOp -> num_binop Nat.div Int.div Word8.div_u Word16.div_u Word32.div_u Word64.div_u Float.div p
    | ModOp -> num_binop Nat.rem Int.rem Word8.rem_u Word16.rem_u Word32.rem_u Word64.rem_u Float.div p (* TBR *)
    | PowOp -> num_binop Nat.pow Int.pow Word8.pow Word16.pow Word32.pow Word64.pow Float.pow p (* TBR *)
    | AndOp -> word_binop Word8.and_ Word16.and_ Word32.and_ Word64.and_ p
    | OrOp  -> word_binop Word8.or_ Word16.or_ Word32.or_ Word64.or_ p
    | XorOp -> word_binop Word8.xor Word16.xor Word32.xor Word64.xor p
    | ShLOp -> word_binop Word8.shl Word16.shl Word32.shl Word64.shl p
    | ShROp -> word_binop Word8.shr_s Word16.shr_s Word32.shr_s Word64.shr_s p
    | RotLOp -> word_binop Word8.rotl Word16.rotl Word32.rotl Word64.rotl p
    | RotROp -> word_binop Word8.rotr Word16.rotr Word32.rotr Word64.rotr p
    | CatOp -> text_binop (^) p
    )
  | _ -> raise (Invalid_argument "binop")


(* Relational operators *)

let word_relop fword8 fword16 fword32 fword64 = function
  | T.Word8 -> fun v1 v2 -> Bool (fword8 (as_word8 v1) (as_word8 v2))
  | T.Word16 -> fun v1 v2 -> Bool (fword16 (as_word16 v1) (as_word16 v2))
  | T.Word32 -> fun v1 v2 -> Bool (fword32 (as_word32 v1) (as_word32 v2))
  | T.Word64 -> fun v1 v2 -> Bool (fword64 (as_word64 v1) (as_word64 v2))
  | _ -> raise (Invalid_argument "relop")

let num_relop fnat fint fword8 fword16 fword32 fword64 ffloat = function
  | T.Nat -> fun v1 v2 -> Bool (fnat (as_nat v1) (as_nat v2))
  | T.Int -> fun v1 v2 -> Bool (fint (as_int v1) (as_int v2))
  | T.Float -> fun v1 v2 -> Bool (ffloat (as_float v1) (as_float v2))
  | t -> word_relop fword8 fword16 fword32 fword64 t

let ord_relop fnat fint fword8 fword16 fword32 fword64 ffloat fchar ftext = function
  | T.Char -> fun v1 v2 -> Bool (fchar (as_char v1) (as_char v2))
  | T.Text -> fun v1 v2 -> Bool (ftext (as_text v1) (as_text v2))
  | t -> num_relop fnat fint fword8 fword16 fword32 fword64 ffloat t

let eq_relop fnat fint fword8 fword16 fword32 fword64 ffloat fchar ftext fnull fbool = function
  | T.Null -> fun v1 v2 -> Bool (fnull (as_null v1) (as_null v2))
  | T.Bool -> fun v1 v2 -> Bool (fbool (as_bool v1) (as_bool v2))
  | t -> ord_relop fnat fint fword8 fword16 fword32 fword64 ffloat fchar ftext t

let relop t op =
  match t with
  | T.Prim p -> 
    (match op with
    | EqOp -> eq_relop Nat.eq Int.eq Word8.eq Word16.eq Word32.eq Word64.eq Float.eq (=) (=) (=) (=) p
    | NeqOp -> eq_relop Nat.ne Int.ne Word8.ne Word16.ne Word32.ne Word64.ne Float.ne (<>) (<>) (<>) (<>) p
    | LtOp -> ord_relop Nat.lt Int.lt Word8.lt_u Word16.lt_u Word32.lt_u Word64.lt_u Float.lt (<) (<) p
    | GtOp -> ord_relop Nat.gt Int.gt Word8.gt_u Word16.gt_u Word32.gt_u Word64.gt_u Float.gt (>) (>) p
    | LeOp -> ord_relop Nat.le Int.le Word8.le_u Word16.le_u Word32.le_u Word64.le_u Float.le (<=) (<=) p
    | GeOp -> ord_relop Nat.ge Int.ge Word8.ge_u Word16.ge_u Word32.ge_u Word64.ge_u Float.ge (>=) (>=) p
    )
  | _ -> raise (Invalid_argument "relop")


let has f t op = try ignore (f t op); true with Invalid_argument _ -> false
let has_unop t op = has unop t op
let has_binop t op = has binop t op
let has_relop t op = has relop t op
