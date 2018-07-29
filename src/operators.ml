open Types
open Values

let rop (PrimT p) rop =
      match rop with
      | EqOp -> 
       (match p with
        | IntT -> fun v1 v2 -> boolV (Integer.eq (int_of_V v1) (int_of_V v2))
        | NatT -> fun v1 v2 ->  boolV (Natural.eq (nat_of_V v1) (nat_of_V v2))
        | FloatT -> fun v1 v2 -> boolV (Float.eq (float_of_V v1) (float_of_V v2))
        | WordT Width8 ->  fun v1 v2 -> boolV (Word8.eq (word8_of_V v1) (word8_of_V v2))
        | WordT Width16 -> fun v1 v2 -> boolV (Word16.eq (word16_of_V v1) (word16_of_V v2))
        | WordT Width32 -> fun v1 v2 -> boolV (Word32.eq (word32_of_V v1) (word32_of_V v2))
        | WordT Width64 -> fun v1 v2 -> boolV (Word64.eq (word64_of_V v1) (word64_of_V v2))
        | BoolT -> fun v1 v2 -> boolV (bool_of_V v1 = bool_of_V v2)
        | CharT -> fun v1 v2 -> boolV (char_of_V v1 = char_of_V v2)
        | TextT -> fun v1 v2 -> boolV (text_of_V v1 = text_of_V v2)
        | _ -> raise Not_found)
      | NeqOp ->
       (match p with
        | IntT -> fun v1 v2 -> boolV (Integer.ne (int_of_V v1) (int_of_V v2))
        | NatT -> fun v1 v2 ->  boolV (Natural.ne (nat_of_V v1) (nat_of_V v2))
        | FloatT -> fun v1 v2 -> boolV (Float.ne (float_of_V v1) (float_of_V v2))
        | WordT Width8 ->  fun v1 v2 -> boolV (Word8.ne (word8_of_V v1) (word8_of_V v2))
        | WordT Width16 -> fun v1 v2 -> boolV (Word16.ne (word16_of_V v1) (word16_of_V v2))
        | WordT Width32 -> fun v1 v2 -> boolV (Word32.ne (word32_of_V v1) (word32_of_V v2))
        | WordT Width64 -> fun v1 v2 -> boolV (Word64.ne (word64_of_V v1) (word64_of_V v2))
        | BoolT -> fun v1 v2 -> boolV (bool_of_V v1 <> bool_of_V v2)
        | CharT -> fun v1 v2 -> boolV (char_of_V v1 <> char_of_V v2)
        | TextT -> fun v1 v2 -> boolV (text_of_V v1 <> text_of_V v2)
        | _ -> raise Not_found)
      | LtOp -> 
       (match p with
        | IntT -> fun v1 v2 -> boolV (Integer.lt_s (int_of_V v1) (int_of_V v2))
        | NatT -> fun v1 v2 ->  boolV (Natural.lt_u (nat_of_V v1) (nat_of_V v2))
        | FloatT -> fun v1 v2 -> boolV (Float.lt (float_of_V v1) (float_of_V v2))
        | WordT Width8 ->  fun v1 v2 -> boolV (Word8.lt_u (word8_of_V v1) (word8_of_V v2))
        | WordT Width16 -> fun v1 v2 -> boolV (Word16.lt_u (word16_of_V v1) (word16_of_V v2))
        | WordT Width32 -> fun v1 v2 -> boolV (Word32.lt_u (word32_of_V v1) (word32_of_V v2))
        | WordT Width64 -> fun v1 v2 -> boolV (Word64.lt_u (word64_of_V v1) (word64_of_V v2))
        | BoolT -> fun v1 v2 -> boolV (bool_of_V v1 < bool_of_V v2)
        | CharT -> fun v1 v2 -> boolV (char_of_V v1 < char_of_V v2)
        | TextT -> fun v1 v2 -> boolV (text_of_V v1 < text_of_V v2)
        | _ -> raise Not_found)
      | LeOp -> 
       (match p with
        | IntT -> fun v1 v2 -> boolV (Integer.le_s (int_of_V v1) (int_of_V v2))
        | NatT -> fun v1 v2 ->  boolV (Natural.le_u (nat_of_V v1) (nat_of_V v2))
        | FloatT -> fun v1 v2 -> boolV (Float.le (float_of_V v1) (float_of_V v2))
        | WordT Width8 ->  fun v1 v2 -> boolV (Word8.le_u (word8_of_V v1) (word8_of_V v2))
        | WordT Width16 -> fun v1 v2 -> boolV (Word16.le_u (word16_of_V v1) (word16_of_V v2))
        | WordT Width32 -> fun v1 v2 -> boolV (Word32.le_u (word32_of_V v1) (word32_of_V v2))
        | WordT Width64 -> fun v1 v2 -> boolV (Word64.le_u (word64_of_V v1) (word64_of_V v2))
        | BoolT -> fun v1 v2 -> boolV (bool_of_V v1 <= bool_of_V v2)
        | CharT -> fun v1 v2 -> boolV (char_of_V v1 <= char_of_V v2)
        | TextT -> fun v1 v2 -> boolV (text_of_V v1 <= text_of_V v2)
        | _ -> raise Not_found)
      | GtOp -> 
       (match p with
        | IntT -> fun v1 v2 -> boolV (Integer.gt_s (int_of_V v1) (int_of_V v2))
        | NatT -> fun v1 v2 ->  boolV (Natural.gt_u (nat_of_V v1) (nat_of_V v2))
        | FloatT -> fun v1 v2 -> boolV (Float.gt (float_of_V v1) (float_of_V v2))
        | WordT Width8 ->  fun v1 v2 ->  boolV (Word8.gt_u (word8_of_V v1) (word8_of_V v2))
        | WordT Width16 -> fun v1 v2 -> boolV (Word16.gt_u (word16_of_V v1) (word16_of_V v2))
        | WordT Width32 -> fun v1 v2 -> boolV (Word32.gt_u (word32_of_V v1) (word32_of_V v2))
        | WordT Width64 -> fun v1 v2 -> boolV (Word64.gt_u (word64_of_V v1) (word64_of_V v2))
        | BoolT -> fun v1 v2 -> boolV (bool_of_V v1 > bool_of_V v2)
        | CharT -> fun v1 v2 -> boolV (char_of_V v1 > char_of_V v2)
        | TextT -> fun v1 v2 -> boolV (text_of_V v1 > text_of_V v2)
        | _ -> raise Not_found)
      | GeOp -> 
       (match p with
        | IntT -> fun v1 v2 -> boolV (Integer.ge_s (int_of_V v1) (int_of_V v2))
        | NatT -> fun v1 v2 ->  boolV (Natural.ge_u (nat_of_V v1) (nat_of_V v2))
        | FloatT -> fun v1 v2 -> boolV (Float.ge (float_of_V v1) (float_of_V v2))
        | WordT Width8 ->  fun v1 v2 -> boolV (Word8.ge_u  (word8_of_V v1) (word8_of_V v2))
        | WordT Width16 -> fun v1 v2 -> boolV (Word16.ge_u (word16_of_V v1) (word16_of_V v2))
        | WordT Width32 -> fun v1 v2 -> boolV (Word32.ge_u (word32_of_V v1) (word32_of_V v2))
        | WordT Width64 -> fun v1 v2 -> boolV (Word64.ge_u (word64_of_V v1) (word64_of_V v2))
        | BoolT -> fun v1 v2 -> boolV (bool_of_V v1 >= bool_of_V v2)
        | CharT -> fun v1 v2 -> boolV (char_of_V v1 >= char_of_V v2)
        | TextT -> fun v1 v2 -> boolV (text_of_V v1 >= text_of_V v2)
        | _ -> raise Not_found)

let uop (PrimT p) uop =
    match uop with
    | PosOp ->
      (match p with
       | IntT -> fun v -> v
       | FloatT -> fun v -> v
       | _ -> raise Not_found)
    | NegOp ->
      (match p with
       | IntT -> fun v -> intV (Int32Rep.neg (int_of_V v)) (*TBR overflow*)
       | FloatT -> fun v -> floatV (Float.neg (float_of_V v))
       | _ -> raise Not_found)
    | NotOp ->
      (match p with
       | WordT Width8  -> fun v -> let w = word8_of_V v in (word8V (Word8.xor w w))
       | WordT Width16 -> fun v -> let w = word16_of_V v in (word16V (Word16.xor w w))
       | WordT Width32 -> fun v -> let w = word32_of_V v in (word32V (Word32.xor w w))
       | WordT Width64 -> fun v -> let w = word64_of_V v in (word64V (Word64.xor w w))
       | _ -> raise Not_found)

let bop (PrimT p) bop =
    match bop with
    | CatOp ->
      fun v1 v2 -> textV ((text_of_V v1) ^ (text_of_V v2)) (*TBR will ^ work on unicode *)
    | AddOp ->
      (match p with
      | IntT -> fun v1 v2 -> intV (Integer.add (int_of_V v1) (int_of_V v2))   (*TBR overflow*)
      | NatT -> fun v1 v2 -> natV (Natural.add (nat_of_V v1) (nat_of_V v2))  (*TBR overflow*)
      | FloatT -> fun v1 v2 ->floatV (Float.add (float_of_V v1) (float_of_V v2))
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.add (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.add (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.add (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.add (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)
    | SubOp ->
      (match p with
      | IntT -> fun v1 v2 -> intV (Integer.sub (int_of_V v1) (int_of_V v2)) (*TBR underflow*)
      | NatT -> fun v1 v2 -> natV (Natural.sub (nat_of_V v1) (nat_of_V v2))  (*TBR underflow*)
      | FloatT -> fun v1 v2 -> floatV (Float.sub (float_of_V v1) (float_of_V v2))
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.sub (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.sub (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.sub (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.sub (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)
    | MulOp ->
      (match p with
      | IntT -> fun v1 v2 -> intV (Integer.mul (int_of_V v1) (int_of_V v2))  (*TBR overflow*)
      | NatT -> fun v1 v2 -> natV (Natural.mul (nat_of_V v1) (nat_of_V v2))  (*TBR overflow*)
      | FloatT -> fun v1 v2 -> floatV (Float.mul (float_of_V v1) (float_of_V v2))
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.mul (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.mul (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.mul (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.mul  (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)
    | DivOp ->
      (match p with
      | IntT -> fun v1 v2 -> intV (Integer.div_s (int_of_V v1) (int_of_V v2))
      | NatT -> fun v1 v2 -> natV (Natural.div_u (nat_of_V v1) (nat_of_V v2))
      | FloatT -> fun v1 v2 -> floatV (Float.div (float_of_V v1) (float_of_V v2))
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.div_u (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.div_u (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.div_u (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.div_u  (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)
    | ModOp ->
      (match p with
      | IntT -> fun v1 v2 -> intV (Integer.rem_s (int_of_V v1) (int_of_V v2))
      | NatT -> fun v1 v2 -> natV (Natural.rem_u (nat_of_V v1) (nat_of_V v2))
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.rem_u (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.rem_u (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.rem_u (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.rem_u  (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)
    | AndOp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.and_ (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.and_ (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.and_ (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.and_ (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)
    | OrOp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.or_ (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.or_ (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.or_ (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.or_ (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)
    | XorOp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.xor (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.xor (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.xor (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.xor (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)
    | ShiftLOp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.shl (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.shl (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.shl (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.shl (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)
    | ShiftROp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.shr_u (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.shr_u (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.shr_u (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.shr_u (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)
    | RotLOp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.rotl (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.rotl (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.rotl (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.rotl (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)
    | RotROp ->
      (match p with
      | WordT Width8 ->  fun v1 v2 ->  word8V (Word8.rotr (word8_of_V v1) (word8_of_V v2))
      | WordT Width16 -> fun v1 v2 -> word16V (Word16.rotr (word16_of_V v1) (word16_of_V v2))
      | WordT Width32 -> fun v1 v2 -> word32V (Word32.rotr (word32_of_V v1) (word32_of_V v2))
      | WordT Width64 -> fun v1 v2 -> word64V (Word64.rotr (word64_of_V v1) (word64_of_V v2))
      | _ -> raise Not_found)

