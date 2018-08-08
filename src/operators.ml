open Syntax
open Type
open Value

let find_unop t uop =
  match t with
  | PrimT p ->
  begin
    match uop with
    | PosOp ->
      (match p with
       | IntT -> fun v -> v
       | FloatT -> fun v -> v
       | _ -> raise Not_found)
    | NegOp ->
      (match p with
       | IntT -> fun v -> IntV (Int32Rep.neg (as_int v)) (*TBR overflow*)
       | FloatT -> fun v -> FloatV (Float.neg (as_float v))
       | _ -> raise Not_found)
    | NotOp ->
      (match p with
       | WordT Width8  -> fun v -> let w = as_word8 v in (Word8V (Word8.xor w w))
       | WordT Width16 -> fun v -> let w = as_word16 v in (Word16V (Word16.xor w w))
       | WordT Width32 -> fun v -> let w = as_word32 v in (Word32V (Word32.xor w w))
       | WordT Width64 -> fun v -> let w = as_word64 v in (Word64V (Word64.xor w w))
       | _ -> raise Not_found)
  end
  | _ -> raise Not_found

let find_binop t bop =
  match t with
  | PrimT p ->
  begin
    match bop with
    | CatOp ->
      fun v1 v2 -> TextV (as_text v1 ^ as_text v2) (*TBR will ^ work on unicode *)
    | AddOp ->
      (match p with
      | IntT -> fun v1 v2 -> IntV (Int.add (as_int v1) (as_int v2))   (*TBR overflow*)
      | NatT -> fun v1 v2 -> NatV (Nat.add (as_nat v1) (as_nat v2))  (*TBR overflow*)
      | FloatT -> fun v1 v2 -> FloatV (Float.add (as_float v1) (as_float v2))
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.add (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.add (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.add (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.add (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
    | SubOp ->
      (match p with
      | IntT -> fun v1 v2 -> IntV (Int.sub (as_int v1) (as_int v2)) (*TBR underflow*)
      | NatT -> fun v1 v2 -> NatV (Nat.sub (as_nat v1) (as_nat v2))  (*TBR underflow*)
      | FloatT -> fun v1 v2 -> FloatV (Float.sub (as_float v1) (as_float v2))
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.sub (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.sub (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.sub (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.sub (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
    | MulOp ->
      (match p with
      | IntT -> fun v1 v2 -> IntV (Int.mul (as_int v1) (as_int v2))  (*TBR overflow*)
      | NatT -> fun v1 v2 -> NatV (Nat.mul (as_nat v1) (as_nat v2))  (*TBR overflow*)
      | FloatT -> fun v1 v2 -> FloatV (Float.mul (as_float v1) (as_float v2))
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.mul (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.mul (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.mul (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.mul  (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
    | DivOp ->
      (match p with
      | IntT -> fun v1 v2 -> IntV (Int.div_s (as_int v1) (as_int v2))
      | NatT -> fun v1 v2 -> NatV (Nat.div_u (as_nat v1) (as_nat v2))
      | FloatT -> fun v1 v2 -> FloatV (Float.div (as_float v1) (as_float v2))
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.div_u (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.div_u (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.div_u (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.div_u  (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
    | ModOp ->
      (match p with
      | IntT -> fun v1 v2 -> IntV (Int.rem_s (as_int v1) (as_int v2))
      | NatT -> fun v1 v2 -> NatV (Nat.rem_u (as_nat v1) (as_nat v2))
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.rem_u (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.rem_u (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.rem_u (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.rem_u  (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
    | AndOp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.and_ (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.and_ (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.and_ (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.and_ (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
    | OrOp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.or_ (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.or_ (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.or_ (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.or_ (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
    | XorOp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.xor (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.xor (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.xor (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.xor (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
    | ShiftLOp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.shl (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.shl (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.shl (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.shl (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
    | ShiftROp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.shr_u (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.shr_u (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.shr_u (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.shr_u (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
    | RotLOp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.rotl (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.rotl (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.rotl (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.rotl (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
    | RotROp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 -> Word8V (Word8.rotr (as_word8 v1) (as_word8 v2))
      | WordT Width16 -> fun v1 v2 -> Word16V (Word16.rotr (as_word16 v1) (as_word16 v2))
      | WordT Width32 -> fun v1 v2 -> Word32V (Word32.rotr (as_word32 v1) (as_word32 v2))
      | WordT Width64 -> fun v1 v2 -> Word64V (Word64.rotr (as_word64 v1) (as_word64 v2))
      | _ -> raise Not_found)
  end
  | _ -> raise Not_found

let find_relop t rop =
  match t with
    | PrimT p -> 
    begin
      match rop with
      | EqOp -> 
       (match p with
        | IntT -> fun v1 v2 -> BoolV (Int.eq (as_int v1) (as_int v2))
        | NatT -> fun v1 v2 -> BoolV (Nat.eq (as_nat v1) (as_nat v2))
        | FloatT -> fun v1 v2 -> BoolV (Float.eq (as_float v1) (as_float v2))
        | WordT Width8 ->  fun v1 v2 -> BoolV (Word8.eq (as_word8 v1) (as_word8 v2))
        | WordT Width16 -> fun v1 v2 -> BoolV (Word16.eq (as_word16 v1) (as_word16 v2))
        | WordT Width32 -> fun v1 v2 -> BoolV (Word32.eq (as_word32 v1) (as_word32 v2))
        | WordT Width64 -> fun v1 v2 -> BoolV (Word64.eq (as_word64 v1) (as_word64 v2))
        | BoolT -> fun v1 v2 -> BoolV (as_bool v1 = as_bool v2)
        | CharT -> fun v1 v2 -> BoolV (as_char v1 = as_char v2)
        | TextT -> fun v1 v2 -> BoolV (as_text v1 = as_text v2)
        | _ -> raise Not_found)
      | NeqOp ->
       (match p with
        | IntT -> fun v1 v2 -> BoolV (Int.ne (as_int v1) (as_int v2))
        | NatT -> fun v1 v2 -> BoolV (Nat.ne (as_nat v1) (as_nat v2))
        | FloatT -> fun v1 v2 -> BoolV (Float.ne (as_float v1) (as_float v2))
        | WordT Width8 ->  fun v1 v2 -> BoolV (Word8.ne (as_word8 v1) (as_word8 v2))
        | WordT Width16 -> fun v1 v2 -> BoolV (Word16.ne (as_word16 v1) (as_word16 v2))
        | WordT Width32 -> fun v1 v2 -> BoolV (Word32.ne (as_word32 v1) (as_word32 v2))
        | WordT Width64 -> fun v1 v2 -> BoolV (Word64.ne (as_word64 v1) (as_word64 v2))
        | BoolT -> fun v1 v2 -> BoolV (as_bool v1 <> as_bool v2)
        | CharT -> fun v1 v2 -> BoolV (as_char v1 <> as_char v2)
        | TextT -> fun v1 v2 -> BoolV (as_text v1 <> as_text v2)
        | _ -> raise Not_found)
      | LtOp -> 
       (match p with
        | IntT -> fun v1 v2 -> BoolV (Int.lt_s (as_int v1) (as_int v2))
        | NatT -> fun v1 v2 ->  BoolV (Nat.lt_u (as_nat v1) (as_nat v2))
        | FloatT -> fun v1 v2 -> BoolV (Float.lt (as_float v1) (as_float v2))
        | WordT Width8 ->  fun v1 v2 -> BoolV (Word8.lt_u (as_word8 v1) (as_word8 v2))
        | WordT Width16 -> fun v1 v2 -> BoolV (Word16.lt_u (as_word16 v1) (as_word16 v2))
        | WordT Width32 -> fun v1 v2 -> BoolV (Word32.lt_u (as_word32 v1) (as_word32 v2))
        | WordT Width64 -> fun v1 v2 -> BoolV (Word64.lt_u (as_word64 v1) (as_word64 v2))
        | BoolT -> fun v1 v2 -> BoolV (as_bool v1 < as_bool v2)
        | CharT -> fun v1 v2 -> BoolV (as_char v1 < as_char v2)
        | TextT -> fun v1 v2 -> BoolV (as_text v1 < as_text v2)
        | _ -> raise Not_found)
      | LeOp -> 
       (match p with
        | IntT -> fun v1 v2 -> BoolV (Int.le_s (as_int v1) (as_int v2))
        | NatT -> fun v1 v2 -> BoolV (Nat.le_u (as_nat v1) (as_nat v2))
        | FloatT -> fun v1 v2 -> BoolV (Float.le (as_float v1) (as_float v2))
        | WordT Width8 ->  fun v1 v2 -> BoolV (Word8.le_u (as_word8 v1) (as_word8 v2))
        | WordT Width16 -> fun v1 v2 -> BoolV (Word16.le_u (as_word16 v1) (as_word16 v2))
        | WordT Width32 -> fun v1 v2 -> BoolV (Word32.le_u (as_word32 v1) (as_word32 v2))
        | WordT Width64 -> fun v1 v2 -> BoolV (Word64.le_u (as_word64 v1) (as_word64 v2))
        | BoolT -> fun v1 v2 -> BoolV (as_bool v1 <= as_bool v2)
        | CharT -> fun v1 v2 -> BoolV (as_char v1 <= as_char v2)
        | TextT -> fun v1 v2 -> BoolV (as_text v1 <= as_text v2)
        | _ -> raise Not_found)
      | GtOp -> 
       (match p with
        | IntT -> fun v1 v2 -> BoolV (Int.gt_s (as_int v1) (as_int v2))
        | NatT -> fun v1 v2 -> BoolV (Nat.gt_u (as_nat v1) (as_nat v2))
        | FloatT -> fun v1 v2 -> BoolV (Float.gt (as_float v1) (as_float v2))
        | WordT Width8 ->  fun v1 v2 -> BoolV (Word8.gt_u (as_word8 v1) (as_word8 v2))
        | WordT Width16 -> fun v1 v2 -> BoolV (Word16.gt_u (as_word16 v1) (as_word16 v2))
        | WordT Width32 -> fun v1 v2 -> BoolV (Word32.gt_u (as_word32 v1) (as_word32 v2))
        | WordT Width64 -> fun v1 v2 -> BoolV (Word64.gt_u (as_word64 v1) (as_word64 v2))
        | BoolT -> fun v1 v2 -> BoolV (as_bool v1 > as_bool v2)
        | CharT -> fun v1 v2 -> BoolV (as_char v1 > as_char v2)
        | TextT -> fun v1 v2 -> BoolV (as_text v1 > as_text v2)
        | _ -> raise Not_found)
      | GeOp -> 
       (match p with
        | IntT -> fun v1 v2 -> BoolV (Int.ge_s (as_int v1) (as_int v2))
        | NatT -> fun v1 v2 -> BoolV (Nat.ge_u (as_nat v1) (as_nat v2))
        | FloatT -> fun v1 v2 -> BoolV (Float.ge (as_float v1) (as_float v2))
        | WordT Width8 ->  fun v1 v2 -> BoolV (Word8.ge_u  (as_word8 v1) (as_word8 v2))
        | WordT Width16 -> fun v1 v2 -> BoolV (Word16.ge_u (as_word16 v1) (as_word16 v2))
        | WordT Width32 -> fun v1 v2 -> BoolV (Word32.ge_u (as_word32 v1) (as_word32 v2))
        | WordT Width64 -> fun v1 v2 -> BoolV (Word64.ge_u (as_word64 v1) (as_word64 v2))
        | BoolT -> fun v1 v2 -> BoolV (as_bool v1 >= as_bool v2)
        | CharT -> fun v1 v2 -> BoolV (as_char v1 >= as_char v2)
        | TextT -> fun v1 v2 -> BoolV (as_text v1 >= as_text v2)
        | _ -> raise Not_found)
    end
  | _ -> raise Not_found


let has_unop t uop = try find_unop t uop; true with Not_found -> false
let has_binop t bop = try find_binop t bop; true with Not_found -> false
let has_relop t rop = try find_relop t rop; true with Not_found -> false
