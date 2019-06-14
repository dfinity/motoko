(* Primitives *)

open Value

module Conv = struct
  open Nativeint
  open Big_int
  let of_signed_Word32 w = to_int (logand 0xFFFFFFFFn (of_int32 w))

  let two = big_int_of_int 2
  let twoRaised62 = power_big_int_positive_int two 62
  let twoRaised63 = power_big_int_positive_int two 63
  let word_twoRaised63 = Word64.(pow 2L 63L)
  let twoRaised64 = power_big_int_positive_int two 64

  let word64_of_nat_big_int i =
    assert (sign_big_int i > -1);
    let wrapped = mod_big_int i twoRaised64 in
    match int64_of_big_int_opt wrapped with
    | Some n -> n
    | _ -> Word64.add (int64_of_big_int (sub_big_int wrapped twoRaised63)) word_twoRaised63

  let word64_of_big_int i =
    let wrapped = mod_big_int i twoRaised64 in
    match int64_of_big_int_opt wrapped with
    | Some n -> n
    | _ -> Word64.sub (int64_of_big_int (sub_big_int wrapped twoRaised63)) word_twoRaised63

  let big_int_of_unsigned_word64 w =
    let i = big_int_of_int64 w in
    if sign_big_int i > -1 then i
    else add_big_int i twoRaised64

  let wrapped_int_of_big_int i =
    match int_of_big_int_opt i with
    | Some n -> n
    | _ -> int_of_big_int (mod_big_int i twoRaised62)
end (* Conv *)


let prim = function
  | "abs" -> fun v k -> k (Int (Nat.abs (as_int v)))

  | "Nat->Word8" -> fun v k ->
                    let i = Conv.wrapped_int_of_big_int (as_int v)
                    in k (Word8 (Word8.of_int_u i))
  | "Int->Word8" -> fun v k ->
                    let i = Conv.wrapped_int_of_big_int (as_int v)
                    in k (Word8 (Word8.of_int_s i))
  | "Nat->Word16" -> fun v k ->
                     let i = Conv.wrapped_int_of_big_int (as_int v)
                     in k (Word16 (Word16.of_int_u i))
  | "Int->Word16" -> fun v k ->
                     let i = Conv.wrapped_int_of_big_int (as_int v)
                     in k (Word16 (Word16.of_int_s i))
  | "Nat->Word32" -> fun v k ->
                     let i = Conv.wrapped_int_of_big_int (as_int v)
                     in k (Word32 (Word32.of_int_u i))
  | "Int->Word32" -> fun v k ->
                     let i = Conv.wrapped_int_of_big_int (as_int v)
                     in k (Word32 (Word32.of_int_s i))

  | "Nat->Word64" -> fun v k -> k (Word64 (Conv.word64_of_nat_big_int (as_int v)))
  | "Int->Word64" -> fun v k -> k (Word64 (Conv.word64_of_big_int (as_int v)))

  | "Word8->Nat" -> fun v k ->
                    let i = Int32.to_int (Int32.shift_right_logical (Word8.to_bits (as_word8 v)) 24)
                    in k (Int (Big_int.big_int_of_int i))
  | "Word8->Int" -> fun v k ->
                    let i = Int32.to_int (Int32.shift_right (Word8.to_bits (as_word8 v)) 24)
                    in k (Int (Big_int.big_int_of_int i))
  | "Word16->Nat" -> fun v k ->
                     let i = Int32.to_int (Int32.shift_right_logical (Word16.to_bits (as_word16 v)) 16)
                     in k (Int (Big_int.big_int_of_int i))
  | "Word16->Int" -> fun v k ->
                     let i = Int32.to_int (Int32.shift_right (Word16.to_bits (as_word16 v)) 16)
                     in k (Int (Big_int.big_int_of_int i))
  | "Word32->Nat" -> fun v k ->
                     let i = Conv.of_signed_Word32 (as_word32 v)
                     in k (Int (Big_int.big_int_of_int i))
  | "Word32->Int" -> fun v k -> k (Int (Big_int.big_int_of_int32 (as_word32 v)))

  | "Word64->Nat" -> fun v k ->
                     let i = Conv.big_int_of_unsigned_word64 (as_word64 v)
                     in k (Int i)
  | "Word64->Int" -> fun v k -> k (Int (Big_int.big_int_of_int64 (as_word64 v)))

  | "Char->Word32" -> fun v k ->
                      let i = as_char v
                      in k (Word32 (Word32.of_int_u i))
  | "Word32->Char" -> fun v k ->
                      let i = Conv.of_signed_Word32 (as_word32 v)
                      in k (Char i)

  | "popcnt8" | "popcnt16" | "popcnt" | "popcnt64" ->
     fun v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. popcnt w)
        | Word16 w -> Word16 (Word16.popcnt w)
        | Word32 w -> Word32 (Word32.popcnt w)
        | Word64 w -> Word64 (Word64.popcnt w)
        | _ -> failwith "popcnt")

  | "clz8" | "clz16" | "clz" | "clz64" ->
     fun v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. clz w)
        | Word16 w -> Word16 (Word16.clz w)
        | Word32 w -> Word32 (Word32.clz w)
        | Word64 w -> Word64 (Word64.clz w)
        | _ -> failwith "clz")

  | "ctz8" | "ctz16" | "ctz" | "ctz64" ->
     fun v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. ctz w)
        | Word16 w -> Word16 (Word16.ctz w)
        | Word32 w -> Word32 (Word32.ctz w)
        | Word64 w -> Word64 (Word64.ctz w)
        | _ -> failwith "ctz")

  | "btst8" | "btst16" | "btst" | "btst64" ->
     fun v k ->
     let w, a = as_pair v
     in k (match w with
           | Word8  y -> Word8  Word8. (and_ y (shl (of_int_u 1) (as_word8  a)))
           | Word16 y -> Word16 Word16.(and_ y (shl (of_int_u 1) (as_word16 a)))
           | Word32 y -> Word32 Word32.(and_ y (shl 1l (as_word32 a)))
           | Word64 y -> Word64 Word64.(and_ y (shl 1L (as_word64 a)))
           | _ -> failwith "btst")

  | "Char->Text" -> fun v k -> let str = match as_char v with
                                          | c when c <= 0o177 -> String.make 1 (Char.chr c)
                                          | code -> Wasm.Utf8.encode [code]
                               in k (Text str)
  | "print" -> fun v k -> Printf.printf "%s%!" (as_text v); k unit
  | "rts_version" -> fun v k -> as_unit v; k (Text "0.1")
  | "idlHash" -> fun v k -> let s = as_text v in k (Word32 (IdlHash.idl_hash s))
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
  | "@serialize" -> fun v k -> k (Serialized v)
  | "@deserialize" -> fun v k -> k (as_serialized v)
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
  | s -> raise (Invalid_argument ("Value.prim: " ^ s))
