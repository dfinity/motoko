(* Primitives *)
open As_types

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
  | T.Nat8, T.Word8 -> fun v k ->
    let i = Nat8.to_int (as_nat8 v)
    in k (Word8 (Word8.of_int_u i))
  | T.Nat, T.Word8 -> fun v k ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in k (Word8 (Word8.of_int_u i))
  | T.Nat, T.Nat8 -> fun v k ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 8) in
    let i = Big_int.int_of_big_int r
    in Big_int.(if eq_big_int q zero_big_int then k (Nat8 (Nat8.of_int i)) else range_violation ())
  | T.Int8, T.Word8 -> fun v k ->
    let i = Int_8.to_int (as_int8 v)
    in k (Word8 (Word8.of_int_s i))
  | T.Int, T.Word8 -> fun v k ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in k (Word8 (Word8.of_int_s i))
  | T.Int, T.Int8 -> fun v k ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 7) in
    let i = Big_int.int_of_big_int r in
    Big_int.
    (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
    then k (Int8(Int_8.of_int (Conv.to_signed i q 0x80))) else range_violation ())
  | T.Nat16, T.Word16 -> fun v k ->
    let i = Nat16.to_int (as_nat16 v)
    in k (Word16 (Word16.of_int_u i))
  | T.Nat, T.Word16 -> fun v k ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in k (Word16 (Word16.of_int_u i))
  | T.Nat, T.Nat16 -> fun v k ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 16) in
    let i = Big_int.int_of_big_int r
    in Big_int.(if eq_big_int q zero_big_int then k (Nat16 (Nat16.of_int i)) else range_violation ())
  | T.Int16, T.Word16 -> fun v k ->
    let i = Int_16.to_int (as_int16 v)
    in k (Word16 (Word16.of_int_s i))
  | T.Int, T.Word16 -> fun v k ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in k (Word16 (Word16.of_int_s i))
  | T.Int, T.Int16 -> fun v k ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 15) in
    let i = Big_int.int_of_big_int r in
    Big_int.
    (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
    then k (Int16(Int_16.of_int (Conv.to_signed i q 0x8000))) else range_violation ())
  | T.Nat32, T.Word32 -> fun v k ->
    let i = Nat32.to_int (as_nat32 v)
    in k (Word32 (Word32.of_int_u i))
  | T.Nat, T.Word32 -> fun v k ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in k (Word32 (Word32.of_int_u i))
  | T.Nat, T.Nat32 -> fun v k ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 32) in
    let i = Big_int.int_of_big_int r
    in Big_int.(if eq_big_int q zero_big_int then k (Nat32 (Nat32.of_int i)) else range_violation ())
  | T.Int32, T.Word32 -> fun v k ->
    let i = Int_32.to_int (as_int32 v)
    in k (Word32 (Word32.of_int_s i))
  | T.Int, T.Word32 -> fun v k ->
    let i = Conv.wrapped_int_of_big_int (as_int v)
    in k (Word32 (Word32.of_int_s i))
  | T.Int, T.Int32 -> fun v k ->
    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 31) in
    let i = Big_int.int_of_big_int r in
    Big_int.
    (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
    then k (Int32 (Int_32.of_int (Conv.to_signed i q 0x80000000))) else range_violation ())

  | T.Nat64, T.Word64 -> fun v k ->
    let q, r = Big_int.quomod_big_int (Nat64.to_big_int (as_nat64 v)) Conv.twoRaised63 in
    let i = Conv.(to_signed_big_int r q twoRaised63) in
    k (Word64 (Big_int.int64_of_big_int i))
  | T.Nat, T.Word64 -> fun v k -> k (Word64 (Conv.word64_of_big_int_u (as_int v)))
  | T.Nat, T.Nat64 -> fun v k ->
    let q, r = Big_int.quomod_big_int (as_int v) Conv.twoRaised64 in
    Big_int.
    (if eq_big_int q zero_big_int
    then k (Nat64 (Nat64.of_big_int r))
    else range_violation ())
  | T.Int64, T.Word64 -> fun v k -> k (Word64 (Big_int.int64_of_big_int (Int_64.to_big_int (as_int64 v))))
  | T.Int, T.Word64 -> fun v k -> k (Word64 (Conv.word64_of_big_int_s (as_int v)))
  | T.Int, T.Int64 -> fun v k ->
    let q, r = Big_int.quomod_big_int (as_int v) Conv.twoRaised63 in
    Big_int.
    (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
    then k (Int64 (Int_64.of_big_int Conv.(to_signed_big_int r q twoRaised63)))
    else range_violation ())

  | T.Word8, T.Nat -> fun v k ->
    let i = Int32.to_int (Int32.shift_right_logical (Word8.to_bits (as_word8 v)) 24)
    in k (Int (Big_int.big_int_of_int i))
  | T.Word8, T.Nat8 -> fun v k ->
    let i = Int32.to_int (Int32.shift_right_logical (Word8.to_bits (as_word8 v)) 24)
    in k (Nat8 (Nat8.of_int i))
  | T.Int8, T.Int -> fun v k -> k (Int (Int.of_int (Int_8.to_int (as_int8 v))))
  | T.Nat8, T.Nat -> fun v k -> k (Int (Nat.of_int (Nat8.to_int (as_nat8 v))))
  | T.Word8, T.Int -> fun v k ->
    let i = Int32.to_int (Int32.shift_right (Word8.to_bits (as_word8 v)) 24)
    in k (Int (Big_int.big_int_of_int i))
  | T.Word8, T.Int8 -> fun v k ->
    let i = Int32.to_int (Int32.shift_right (Word8.to_bits (as_word8 v)) 24)
    in k (Int8 (Int_8.of_int i))
  | T.Word16, T.Nat -> fun v k ->
    let i = Int32.to_int (Int32.shift_right_logical (Word16.to_bits (as_word16 v)) 16)
    in k (Int (Big_int.big_int_of_int i))
  | T.Word16, T.Nat16 -> fun v k ->
    let i = Int32.to_int (Int32.shift_right_logical (Word16.to_bits (as_word16 v)) 16)
    in k (Nat16 (Nat16.of_int i))
  | T.Int16, T.Int -> fun v k -> k (Int (Int.of_int (Int_16.to_int (as_int16 v))))
  | T.Nat16, T.Nat -> fun v k -> k (Int (Nat.of_int (Nat16.to_int (as_nat16 v))))
  | T.Word16, T.Int -> fun v k ->
    let i = Int32.to_int (Int32.shift_right (Word16.to_bits (as_word16 v)) 16)
    in k (Int (Big_int.big_int_of_int i))
  | T.Word16, T.Int16 -> fun v k ->
    let i = Int32.to_int (Int32.shift_right (Word16.to_bits (as_word16 v)) 16)
    in k (Int16 (Int_16.of_int i))
  | T.Int32, T.Int -> fun v k -> k (Int (Int.of_int (Int_32.to_int (as_int32 v))))
  | T.Nat32, T.Nat -> fun v k -> k (Int (Nat.of_int (Nat32.to_int (as_nat32 v))))
  | T.Word32, T.Nat -> fun v k ->
    let i = Conv.int_of_word32_u (as_word32 v)
    in k (Int (Big_int.big_int_of_int i))
  | T.Word32, T.Int -> fun v k -> k (Int (Big_int.big_int_of_int32 (as_word32 v)))
  | T.Word32, T.Int32 -> fun v k ->
    let i = Big_int.(int_of_big_int (big_int_of_int32 (as_word32 v))) in
    k (Int32 (Int_32.of_int i))
  | T.Word32, T.Nat32 -> fun v k ->
    let i = Big_int.(int_of_big_int (big_int_of_int32 (as_word32 v))) in
    let i' = if i < 0 then i + 0x100000000 else i in
    k (Nat32 (Nat32.of_int i'))

  | T.Int64, T.Int -> fun v k -> k (Int (Int_64.to_big_int (as_int64 v)))
  | T.Nat64, T.Nat -> fun v k -> k (Int (Nat64.to_big_int (as_nat64 v)))
  | T.Word64, T.Nat -> fun v k ->
    let i = Conv.big_int_of_word64_u (as_word64 v)
    in k (Int i)
  | T.Word64, T.Nat64 -> fun v k ->
    let i = Conv.big_int_of_word64_u (as_word64 v)
    in k (Nat64 (Nat64.of_big_int i))
  | T.Word64, T.Int -> fun v k -> k (Int (Big_int.big_int_of_int64 (as_word64 v)))
  | T.Word64, T.Int64 -> fun v k ->
    let i = Big_int.big_int_of_int64 (as_word64 v)
    in k (Int64 (Int_64.of_big_int i))

  | T.Char, T.Word32 -> fun v k ->
    let i = as_char v
    in k (Word32 (Word32.of_int_u i))
  | T.Word32, T.Char -> fun v k ->
    let i = Conv.int_of_word32_u (as_word32 v)
    in if i < 0xD800 || i >= 0xE000 && i < 0x110000 then k (Char i) else raise (Invalid_argument "character value out of bounds")
  | t1, t2 -> raise (Invalid_argument ("Value.num_conv_prim: " ^ T.string_of_typ (T.Prim t1) ^ T.string_of_typ (T.Prim t2) ))

let prim = function
  | "abs" -> fun v k -> k (Int (Nat.abs (as_int v)))


  | "popcnt8" | "popcnt16" | "popcnt32" | "popcnt64" ->
     fun v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. popcnt w)
        | Word16 w -> Word16 (Word16.popcnt w)
        | Word32 w -> Word32 (Word32.popcnt w)
        | Word64 w -> Word64 (Word64.popcnt w)
        | _ -> failwith "popcnt")

  | "clz8" | "clz16" | "clz32" | "clz64" ->
     fun v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. clz w)
        | Word16 w -> Word16 (Word16.clz w)
        | Word32 w -> Word32 (Word32.clz w)
        | Word64 w -> Word64 (Word64.clz w)
        | _ -> failwith "clz")

  | "ctz8" | "ctz16" | "ctz32" | "ctz64" ->
     fun v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. ctz w)
        | Word16 w -> Word16 (Word16.ctz w)
        | Word32 w -> Word32 (Word32.ctz w)
        | Word64 w -> Word64 (Word64.ctz w)
        | _ -> failwith "ctz")

  | "btst8" | "btst16" | "btst32" | "btst64" ->
     fun v k ->
     let w, a = as_pair v
     in k (match w with
           | Word8  y -> Word8  Word8. (and_ y (shl (of_int_u 1) (as_word8  a)))
           | Word16 y -> Word16 Word16.(and_ y (shl (of_int_u 1) (as_word16 a)))
           | Word32 y -> Word32 Word32.(and_ y (shl 1l (as_word32 a)))
           | Word64 y -> Word64 Word64.(and_ y (shl 1L (as_word64 a)))
           | _ -> failwith "btst")

  | "conv_Char_Text" -> fun v k -> let str = match as_char v with
                                          | c when c <= 0o177 -> String.make 1 (Char.chr c)
                                          | code -> Wasm.Utf8.encode [code]
                               in k (Text str)
  | "print" -> fun v k -> Printf.printf "%s%!" (as_text v); k unit
  | "rts_version" -> fun v k -> as_unit v; k (Text "0.1")
  | "idlHash" -> fun v k -> let s = as_text v in k (Word32 (Lib.Uint32.to_int32 (Idllib.IdlHash.idl_hash s)))
  | "decodeUTF8" -> fun v k ->
                    let s = as_text v in
                    let open Int32 in
                    let take_and_mask bits offset =
                      logand (sub (shift_left 1l bits) 1l) (of_int (Char.code s.[offset])) in
                    let open List in
                    let classify_utf8_leader =
                      function
                      | ch when compare ch 0x80l < 0 -> map take_and_mask [7]
                      | ch when compare ch 0xe0l < 0 -> map take_and_mask [5; 6]
                      | ch when compare ch 0xf0l < 0 -> map take_and_mask [4; 6; 6]
                      | ch                           -> map take_and_mask [3; 6; 6; 6] in
                    let nobbles = mapi (fun i f -> f i) (classify_utf8_leader (of_int (Char.code s.[0]))) in
                    let code = fold_left (fun acc nobble -> logor (shift_left acc 6) nobble) 0l nobbles in
                    k (Tup [Word32 (of_int (length nobbles)); Char (to_int code)])

  | "array_len" -> fun v k ->
    k (Int (Int.of_int (Array.length (Value.as_array v))))
  | "text_len" -> fun v k ->
    k (Int (Nat.of_int (List.length (Wasm.Utf8.decode (Value.as_text v)))))
  | "text_chars" -> fun v k ->
    let i = ref 0 in
    let s = Wasm.Utf8.decode (Value.as_text v) in
    let next = local_func 0 1 @@ fun v k' ->
        if !i = List.length s then k' Null else
          let v = Opt (Char (List.nth s !i)) in incr i; k' v
    in k (Obj (Env.singleton "next" next))

  | "Array.init" -> fun v k ->
    (match Value.as_tup v with
    | [len; x] ->
      k (Array (Array.init (Int.to_int (as_int len)) (fun _ -> Mut (ref x))))
    | _ -> assert false
    )
  | "Array.tabulate" -> fun v k ->
    (match Value.as_tup v with
    | [len; g] ->
      let len_nat = Int.to_int (as_int len) in
      let (_, g') = Value.as_func g in
      let rec go prefix k i =
        if i == len_nat
        then k (Array (Array.of_list (prefix [])))
        else g' (Int (Int.of_int i)) (fun x -> go (fun tl -> prefix (x::tl)) k (i + 1))
      in go (fun xs -> xs) k 0
    | _ -> assert false
    )
  | "error" -> fun v k ->
     k (Value.Tup [Variant ("user", Tup []); v])
  | "errorCode" -> fun v k ->
    (match Value.as_tup v with
    | [code; _message] ->
      k code
    | _ -> assert false
    )
  | "errorMessage" -> fun v k ->
    (match Value.as_tup v with
    | [_code; message] ->
      k message
    | _ -> assert false
    )
  | p when Lib.String.chop_prefix "num_conv" p <> None ->
    begin match String.split_on_char '_' p with
    | [_;_;s1;s2] ->
      let p1 = Type.prim s1 in
      let p2 = Type.prim s2 in
      num_conv_prim p1 p2
    | _ -> assert false
    end
  | s -> raise (Invalid_argument ("Value.prim: " ^ s))
