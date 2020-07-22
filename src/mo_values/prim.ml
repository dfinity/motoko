(* Primitives *)
open Mo_types

open Value

module Conv = struct
  open Big_int
  let int_of_word32_u w = Int32.to_int w land 0xffff_ffff

  let twoRaised62 = power_int_positive_int 2 62
  let twoRaised63 = power_int_positive_int 2 63
  let twoRaised64 = power_int_positive_int 2 64
  let word_twoRaised63 = Word64.(pow 2L 63L)

  let word64_of_big_int_u i =
    assert (sign_big_int i > -1);
    let wrapped = mod_big_int i twoRaised64 in
    match int64_of_big_int_opt wrapped with
    | Some n -> n
    | _ -> Word64.add (int64_of_big_int (sub_big_int wrapped twoRaised63)) word_twoRaised63

  let word64_of_big_int_s i =
    let wrapped = mod_big_int i twoRaised64 in
    match int64_of_big_int_opt wrapped with
    | Some n -> n
    | _ -> Word64.sub (int64_of_big_int (sub_big_int wrapped twoRaised63)) word_twoRaised63

  let big_int_of_word64_u w =
    let i = big_int_of_int64 w in
    if sign_big_int i > -1 then i
    else add_big_int i twoRaised64

  let wrapped_int_of_big_int i =
    match int_of_big_int_opt i with
    | Some n -> n
    | _ -> int_of_big_int (mod_big_int i twoRaised62)

  (* for q in {0, -1} return i + q * offs *)
  let to_signed i q offs = if Big_int.sign_big_int q = 0 then i else i - offs
  let to_signed_big_int i q offs = Big_int.(if sign_big_int q = 0 then i else sub_big_int i offs)
end (* Conv *)

let range_violation () = raise (Invalid_argument "numeric overflow")

let num_conv_prim t1 t2 =
  let module T = Type in
  match (t1, t2) with
  | T.Nat8, T.Word8 -> fun v ->
    let i = Nat8.to_int (as_nat8 v)
    in Word8 (Word8.of_int_u i)
  | T.Nat, T.Word8 -> fun v ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in Word8 (Word8.of_int_u i)
  | T.Nat, T.Nat8 -> fun v ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 8) in
    let i = Big_int.int_of_big_int r
    in Big_int.(if eq_big_int q zero_big_int then Nat8 (Nat8.of_int i) else range_violation ())
  | T.Int8, T.Word8 -> fun v ->
    let i = Int_8.to_int (as_int8 v)
    in Word8 (Word8.of_int_s i)
  | T.Int, T.Word8 -> fun v ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in Word8 (Word8.of_int_s i)
  | T.Int, T.Int8 -> fun v ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 7) in
    let i = Big_int.int_of_big_int r in
    Big_int.
    (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
    then Int8(Int_8.of_int (Conv.to_signed i q 0x80)) else range_violation ())
  | T.Nat16, T.Word16 -> fun v ->
    let i = Nat16.to_int (as_nat16 v)
    in Word16 (Word16.of_int_u i)
  | T.Nat, T.Word16 -> fun v ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in Word16 (Word16.of_int_u i)
  | T.Nat, T.Nat16 -> fun v ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 16) in
    let i = Big_int.int_of_big_int r
    in Big_int.(if eq_big_int q zero_big_int then Nat16 (Nat16.of_int i) else range_violation ())
  | T.Int16, T.Word16 -> fun v ->
    let i = Int_16.to_int (as_int16 v)
    in Word16 (Word16.of_int_s i)
  | T.Int, T.Word16 -> fun v ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in Word16 (Word16.of_int_s i)
  | T.Int, T.Int16 -> fun v ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 15) in
    let i = Big_int.int_of_big_int r in
    Big_int.
    (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
    then Int16(Int_16.of_int (Conv.to_signed i q 0x8000)) else range_violation ())
  | T.Nat32, T.Word32 -> fun v ->
    let i = Nat32.to_int (as_nat32 v)
    in Word32 (Word32.of_int_u i)
  | T.Nat, T.Word32 -> fun v ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in Word32 (Word32.of_int_u i)
  | T.Nat, T.Nat32 -> fun v ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 32) in
    let i = Big_int.int_of_big_int r
    in Big_int.(if eq_big_int q zero_big_int then Nat32 (Nat32.of_int i) else range_violation ())
  | T.Int32, T.Word32 -> fun v ->
    let i = Int_32.to_int (as_int32 v)
    in Word32 (Word32.of_int_s i)
  | T.Int, T.Word32 -> fun v ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in Word32 (Word32.of_int_s i)
  | T.Int, T.Int32 -> fun v ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 31) in
    let i = Big_int.int_of_big_int r in
    Big_int.
    (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
    then Int32 (Int_32.of_int (Conv.to_signed i q 0x80000000)) else range_violation ())

  | T.Nat64, T.Word64 -> fun v ->
    let q, r = Big_int.quomod_big_int (Nat64.to_big_int (as_nat64 v)) Conv.twoRaised63 in
    let i = Conv.(to_signed_big_int r q twoRaised63) in
    Word64 (Big_int.int64_of_big_int i)
  | T.Nat, T.Word64 -> fun v -> Word64 (Conv.word64_of_big_int_u (as_int v))
  | T.Nat, T.Nat64 -> fun v ->
    let q, r = Big_int.quomod_big_int (as_int v) Conv.twoRaised64 in
    Big_int.
    (if eq_big_int q zero_big_int
    then Nat64 (Nat64.of_big_int r)
    else range_violation ())
  | T.Int64, T.Word64 -> fun v -> Word64 (Big_int.int64_of_big_int (Int_64.to_big_int (as_int64 v)))
  | T.Int, T.Word64 -> fun v -> Word64 (Conv.word64_of_big_int_s (as_int v))
  | T.Int, T.Int64 -> fun v ->
    let q, r = Big_int.quomod_big_int (as_int v) Conv.twoRaised63 in
    Big_int.
    (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
    then Int64 (Int_64.of_big_int Conv.(to_signed_big_int r q twoRaised63))
    else range_violation ())

  | T.Word8, T.Nat -> fun v ->
    let i = Int32.to_int (Int32.shift_right_logical (Word8.to_bits (as_word8 v)) 24)
    in Int (Big_int.big_int_of_int i)
  | T.Word8, T.Nat8 -> fun v ->
    let i = Int32.to_int (Int32.shift_right_logical (Word8.to_bits (as_word8 v)) 24)
    in Nat8 (Nat8.of_int i)
  | T.Int8, T.Int -> fun v -> Int (Int.of_int (Int_8.to_int (as_int8 v)))
  | T.Nat8, T.Nat -> fun v -> Int (Nat.of_int (Nat8.to_int (as_nat8 v)))
  | T.Word8, T.Int -> fun v ->
    let i = Int32.to_int (Int32.shift_right (Word8.to_bits (as_word8 v)) 24)
    in Int (Big_int.big_int_of_int i)
  | T.Word8, T.Int8 -> fun v ->
    let i = Int32.to_int (Int32.shift_right (Word8.to_bits (as_word8 v)) 24)
    in Int8 (Int_8.of_int i)
  | T.Word16, T.Nat -> fun v ->
    let i = Int32.to_int (Int32.shift_right_logical (Word16.to_bits (as_word16 v)) 16)
    in Int (Big_int.big_int_of_int i)
  | T.Word16, T.Nat16 -> fun v ->
    let i = Int32.to_int (Int32.shift_right_logical (Word16.to_bits (as_word16 v)) 16)
    in Nat16 (Nat16.of_int i)
  | T.Int16, T.Int -> fun v -> Int (Int.of_int (Int_16.to_int (as_int16 v)))
  | T.Nat16, T.Nat -> fun v -> Int (Nat.of_int (Nat16.to_int (as_nat16 v)))
  | T.Word16, T.Int -> fun v ->
    let i = Int32.to_int (Int32.shift_right (Word16.to_bits (as_word16 v)) 16)
    in Int (Big_int.big_int_of_int i)
  | T.Word16, T.Int16 -> fun v ->
    let i = Int32.to_int (Int32.shift_right (Word16.to_bits (as_word16 v)) 16)
    in Int16 (Int_16.of_int i)
  | T.Int32, T.Int -> fun v -> Int (Int.of_int (Int_32.to_int (as_int32 v)))
  | T.Nat32, T.Nat -> fun v -> Int (Nat.of_int (Nat32.to_int (as_nat32 v)))
  | T.Word32, T.Nat -> fun v ->
    let i = Conv.int_of_word32_u (as_word32 v)
    in Int (Big_int.big_int_of_int i)
  | T.Word32, T.Int -> fun v -> Int (Big_int.big_int_of_int32 (as_word32 v))
  | T.Word32, T.Int32 -> fun v ->
    let i = Big_int.(int_of_big_int (big_int_of_int32 (as_word32 v))) in
    Int32 (Int_32.of_int i)
  | T.Word32, T.Nat32 -> fun v ->
    let i = Big_int.(int_of_big_int (big_int_of_int32 (as_word32 v))) in
    let i' = if i < 0 then i + 0x100000000 else i in
    Nat32 (Nat32.of_int i')

  | T.Int64, T.Int -> fun v -> Int (Int_64.to_big_int (as_int64 v))
  | T.Nat64, T.Nat -> fun v -> Int (Nat64.to_big_int (as_nat64 v))
  | T.Word64, T.Nat -> fun v ->
    let i = Conv.big_int_of_word64_u (as_word64 v)
    in Int i
  | T.Word64, T.Nat64 -> fun v ->
    let i = Conv.big_int_of_word64_u (as_word64 v)
    in Nat64 (Nat64.of_big_int i)
  | T.Word64, T.Int -> fun v -> Int (Big_int.big_int_of_int64 (as_word64 v))
  | T.Word64, T.Int64 -> fun v ->
    let i = Big_int.big_int_of_int64 (as_word64 v)
    in Int64 (Int_64.of_big_int i)

  | T.Char, T.Word32 -> fun v ->
    let i = as_char v
    in Word32 (Word32.of_int_u i)
  | T.Word32, T.Char -> fun v ->
    let i = Conv.int_of_word32_u (as_word32 v)
    in if i < 0xD800 || i >= 0xE000 && i < 0x110000 then Char i else raise (Invalid_argument "character value out of bounds")
  | T.Float, T.Int64 -> fun v -> Int64 (Int_64.of_big_int (Big_int.big_int_of_int64 (Wasm.I64_convert.trunc_f64_s (as_float v))))
  | T.Int64, T.Float -> fun v -> Float (Wasm.F64_convert.convert_i64_s (Big_int.int64_of_big_int (Int_64.to_big_int (as_int64 v))))
  | t1, t2 -> raise (Invalid_argument ("Value.num_conv_prim: " ^ T.string_of_typ (T.Prim t1) ^ T.string_of_typ (T.Prim t2) ))

let prim =
  let via_float f v = Float.(Float (of_float (f (to_float (as_float v))))) in
  let via_float2 f v w = Float.(Float (of_float (f (to_float (as_float v)) (to_float (as_float w))))) in
  let unpack_nat8 v = Nat8.to_int (as_nat8 v) in
  let float_formatter prec : int -> float -> string =
    let open Printf in
    function
    | 0 -> sprintf "%.*f" prec 
    | 1 -> sprintf "%.*e" prec
    | 2 -> sprintf "%.*g" prec
    | 3 -> sprintf "%.*h" prec
    | _ -> fun _ -> raise (Invalid_argument "float_formatter: unrecognised mode") in
  function
  | "abs" -> fun _ v k -> k (Int (Nat.abs (as_int v)))
  | "fabs" -> fun _ v k -> k (Float (Float.abs (as_float v)))
  | "fsqrt" -> fun _ v k -> k (Float (Float.sqrt (as_float v)))
  | "fceil" -> fun _ v k -> k (Float (Float.ceil (as_float v)))
  | "ffloor" -> fun _ v k -> k (Float (Float.floor (as_float v)))
  | "ftrunc" -> fun _ v k -> k (Float (Float.trunc (as_float v)))
  | "fnearest" -> fun _ v k -> k (Float (Float.nearest (as_float v)))
  | "fmin" -> fun _ v k ->
    (match Value.as_tup v with
     | [a; b] -> k (Float (Float.min (as_float a) (as_float b)))
     | _ -> assert false)
  | "fmax" -> fun _ v k ->
    (match Value.as_tup v with
     | [a; b] -> k (Float (Float.max (as_float a) (as_float b)))
     | _ -> assert false)
  | "fcopysign" -> fun _ v k ->
    (match Value.as_tup v with
     | [a; b] -> k (Float (Float.copysign (as_float a) (as_float b)))
     | _ -> assert false)
  | "Float->Text" -> fun _ v k -> k (Text (Float.to_string (as_float v)))
  | "fmtFloat->Text" -> fun _ v k ->
    (match Value.as_tup v with
     | [f; prec; mode] ->
       k (Text (float_formatter (unpack_nat8 prec) (unpack_nat8 mode) Float.(to_float (as_float f))))
     | _ -> assert false)
  | "fsin" -> fun _ v k -> k (via_float Stdlib.sin v)
  | "fcos" -> fun _ v k -> k (via_float Stdlib.cos v)
  | "ftan" -> fun _ v k -> k (via_float Stdlib.tan v)
  | "fasin" -> fun _ v k -> k (via_float Stdlib.asin v)
  | "facos" -> fun _ v k -> k (via_float Stdlib.acos v)
  | "fatan" -> fun _ v k -> k (via_float Stdlib.atan v)
  | "fatan2" -> fun _ v k ->
    (match Value.as_tup v with
     | [y; x] -> k (via_float2 Stdlib.atan2 y x)
     | _ -> assert false)
  | "fexp" -> fun _ v k -> k (via_float Stdlib.exp v)
  | "flog" -> fun _ v k -> k (via_float Stdlib.log v)

  | "popcnt8" | "popcnt16" | "popcnt32" | "popcnt64" ->
     fun _ v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. popcnt w)
        | Word16 w -> Word16 (Word16.popcnt w)
        | Word32 w -> Word32 (Word32.popcnt w)
        | Word64 w -> Word64 (Word64.popcnt w)
        | _ -> failwith "popcnt")

  | "clz8" | "clz16" | "clz32" | "clz64" ->
     fun _ v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. clz w)
        | Word16 w -> Word16 (Word16.clz w)
        | Word32 w -> Word32 (Word32.clz w)
        | Word64 w -> Word64 (Word64.clz w)
        | _ -> failwith "clz")

  | "ctz8" | "ctz16" | "ctz32" | "ctz64" ->
     fun _ v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. ctz w)
        | Word16 w -> Word16 (Word16.ctz w)
        | Word32 w -> Word32 (Word32.ctz w)
        | Word64 w -> Word64 (Word64.ctz w)
        | _ -> failwith "ctz")

  | "btst8" | "btst16" | "btst32" | "btst64" ->
     fun _ v k ->
     let w, a = as_pair v
     in k (match w with
           | Word8  y -> Word8  Word8. (and_ y (shl (of_int_u 1) (as_word8  a)))
           | Word16 y -> Word16 Word16.(and_ y (shl (of_int_u 1) (as_word16 a)))
           | Word32 y -> Word32 Word32.(and_ y (shl 1l (as_word32 a)))
           | Word64 y -> Word64 Word64.(and_ y (shl 1L (as_word64 a)))
           | _ -> failwith "btst")

  | "conv_Char_Text" -> fun _ v k -> let str = match as_char v with
                                          | c when c <= 0o177 -> String.make 1 (Char.chr c)
                                          | code -> Wasm.Utf8.encode [code]
                               in k (Text str)
  | "print" -> fun _ v k -> Printf.printf "%s\n%!" (as_text v); k unit
  | "rts_version" -> fun _ v k -> as_unit v; k (Text "0.1")
  | "rts_heap_size" -> fun _ v k -> as_unit v; k (Int (Int.of_int 0))
  | "rts_total_allocation" -> fun _ v k -> as_unit v; k (Int (Int.of_int 0))
  | "rts_outstanding_callbacks" -> fun _ v k -> as_unit v; k (Int (Int.of_int 0))
  | "time" -> fun _ v k -> as_unit v; k (Value.Nat64 (Value.Nat64.of_int 42))
  | "idlHash" -> fun _ v k -> let s = as_text v in k (Word32 (Lib.Uint32.to_int32 (Idllib.IdlHash.idl_hash s)))
  | "crc32Hash" -> fun _ v k -> let s = as_text v in
    k (Word32 Optint.(to_int32 (Checkseum.Crc32.digest_string s 0 (String.length s) zero)))
  | "array_len" -> fun _ v k ->
    k (Int (Int.of_int (Array.length (Value.as_array v))))
  | "blob_size" -> fun _ v k ->
    k (Int (Nat.of_int (String.length (Value.as_text v))))
  | "blob_iter" -> fun _ v k ->
    let s = String.to_seq (Value.as_text v) in
    let valuation b = Word8 (Word8.of_int_u (Char.code b)) in
    k (Iter (ref (Seq.map valuation s)))
  | "blob_iter_done" | "text_iter_done" -> fun _ v k ->
    let i = Value.as_iter v in
    k (Bool (!i () = Seq.Nil))
  | "blob_iter_next" | "text_iter_next" -> fun _ v k ->
    let i = Value.as_iter v in
    begin match !i () with
    | Seq.Nil -> assert false
    | Seq.Cons (v, vs) -> i := vs; k v
    end
  | "text_len" -> fun _ v k ->
    k (Int (Nat.of_int (List.length (Wasm.Utf8.decode (Value.as_text v)))))
  | "text_iter" -> fun _ v k ->
    let s = Wasm.Utf8.decode (Value.as_text v) in
    let i = Seq.map (fun c -> Char c) (List.to_seq s) in
    k (Iter (ref i))
  | "Array.init" -> fun _ v k ->
    (match Value.as_tup v with
    | [len; x] ->
      k (Array (Array.init (Int.to_int (as_int len)) (fun _ -> Mut (ref x))))
    | _ -> assert false
    )
  | "Array.tabulate" -> fun c v k ->
    (match Value.as_tup v with
    | [len; g] ->
      let len_nat = Int.to_int (as_int len) in
      let (_, g') = Value.as_func g in
      let rec go prefix k i =
        if i == len_nat
        then k (Array (Array.of_list (prefix [])))
        else g' c (Int (Int.of_int i)) (fun x -> go (fun tl -> prefix (x::tl)) k (i + 1))
      in go (fun xs -> xs) k 0
    | _ -> assert false
    )
  | "cast" -> fun _ v k -> k v
  | p when Lib.String.chop_prefix "num_conv" p <> None ->
    begin match String.split_on_char '_' p with
    | [_;_;s1;s2] ->
      let p1 = Type.prim s1 in
      let p2 = Type.prim s2 in
      fun env v k -> k (num_conv_prim p1 p2 v)
    | _ -> assert false
    end

  | "char_to_upper" ->
      fun _ v k ->
        begin match Uucp.Case.Map.to_upper (Uchar.of_int (as_char v)) with
        | `Uchars [c] -> k (Char (Uchar.to_int c))
        | `Uchars _ ->
            (* RTS implementation of to_upper returns the input for characters
               that map to multiple characters in uppercase versions, so to be
               in sync with that we do the same here *)
            k v
        | `Self -> k v
        end

  | "char_to_lower" ->
      fun _ v k ->
        begin match Uucp.Case.Map.to_lower (Uchar.of_int (as_char v)) with
        | `Uchars [c] -> k (Char (Uchar.to_int c))
        | `Uchars _ -> k v (* same as above, in char_to_upper *)
        | `Self -> k v
        end

  | "char_is_whitespace" ->
      fun _ v k -> k (Bool (Uucp.White.is_white_space (Uchar.of_int (as_char v))))

  | "char_is_lowercase" ->
      fun _ v k -> k (Bool (Uucp.Case.is_lower (Uchar.of_int (as_char v))))

  | "char_is_uppercase" ->
      fun _ v k -> k (Bool (Uucp.Case.is_upper (Uchar.of_int (as_char v))))

  | "char_is_alphabetic" ->
      fun _ v k -> k (Bool (Uucp.Alpha.is_alphabetic (Uchar.of_int (as_char v))))

  | s -> raise (Invalid_argument ("Value.prim: " ^ s))
