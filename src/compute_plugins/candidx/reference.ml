open Mo_values

(* Use the compiler's existing Num runtime so the interpreter and JS compiler
   need no new numeric primitives. This encoder implements the cx profile. *)
module Z = struct
  let zero = Big_int.zero_big_int
  let one = Big_int.unit_big_int
  let minus_one = Big_int.minus_big_int one
  let of_int = Big_int.big_int_of_int
  let to_int = Big_int.int_of_big_int
  let equal = Big_int.eq_big_int
  let shift_right = Big_int.shift_right_big_int
  let logand a mask =
    let a = if Big_int.sign_big_int a < 0 then
      Big_int.mod_big_int a
        (Big_int.power_int_positive_int 2 (Big_int.num_bits_big_int mask))
      else a
    in
    Big_int.and_big_int a mask
end

let max_bytes = 32 * 1024 * 1024
let max_items = 2_000_000
let max_types = 1024
let max_depth = 256
exception Invalid of string
let fail message = raise (Invalid message)

type value =
  | Scalar of int * bytes
  | Some_value of value
  | Vector of value list
  | Blob of bytes
  | Record of (int64 * value) list
  | Variant of int64 * value
type shape = { kind : int; fields : int64 list; children : shape list }
let scalar kind = { kind; fields = []; children = [] }
let unary kind child = { kind; fields = []; children = [child] }
let null = Scalar (-1, Bytes.empty)
let is_null = function Scalar (-1, _) -> true | _ -> false

let valid_utf8 bytes =
  let n = Bytes.length bytes in
  let at i = Char.code (Bytes.get bytes i) in
  let rec scan i =
    if i = n then true
    else
      let b = at i in
      if b < 128 then scan (i + 1)
      else
        let width, low, high =
          if b >= 0xc2 && b <= 0xdf then 2, 0x80, 0xbf
          else if b = 0xe0 then 3, 0xa0, 0xbf
          else if b = 0xed then 3, 0x80, 0x9f
          else if b >= 0xe1 && b <= 0xef then 3, 0x80, 0xbf
          else if b = 0xf0 then 4, 0x90, 0xbf
          else if b = 0xf4 then 4, 0x80, 0x8f
          else if b >= 0xf1 && b <= 0xf3 then 4, 0x80, 0xbf
          else 0, 0, 0
        in
        width > 0 && width <= n - i && at (i + 1) >= low &&
        at (i + 1) <= high &&
        (width < 3 || (at (i + 2) >= 0x80 && at (i + 2) <= 0xbf)) &&
        (width < 4 || (at (i + 3) >= 0x80 && at (i + 3) <= 0xbf)) &&
        scan (i + width)
  in
  scan 0
let leb n signed =
  let out = Buffer.create 8 in
  let rec loop n =
    let b = Z.to_int (Z.logand n (Z.of_int 127)) in
    let next = Z.shift_right n 7 in
    let done_ = if signed then
      (Z.equal next Z.zero && b < 64) ||
      (Z.equal next Z.minus_one && b >= 64)
      else Z.equal next Z.zero
    in
    Buffer.add_char out (Char.chr (b lor if done_ then 0 else 128));
    if not done_ then loop next
  in
  loop n;
  Bytes.of_string (Buffer.contents out)

let only = function [x] -> x | _ -> fail "invalid internal unary shape"
let rec join a b depth =
  if depth > max_depth then fail "type nesting limit";
  if a = b || b.kind = -17 then a
  else if a.kind = -17 then b
  else if a.kind = -1 then if b.kind = -18 then b else unary (-18) b
  else if b.kind = -1 then if a.kind = -18 then a else unary (-18) a
  else if a.kind = -18 || b.kind = -18 then
    unary (-18) (join (if a.kind = -18 then only a.children else a)
      (if b.kind = -18 then only b.children else b) (depth + 1))
  else if a.kind <> b.kind || not (List.mem a.kind [-19; -20; -21]) then
    fail "incompatible value shapes"
  else if a.kind = -19 then
    unary (-19) (join (only a.children) (only b.children) (depth + 1))
  else
    let aa = List.combine a.fields a.children in
    let bb = List.combine b.fields b.children in
    let fields = List.sort_uniq Int64.compare (a.fields @ b.fields) in
    let children = List.map (fun k ->
      match List.assoc_opt k aa, List.assoc_opt k bb with
      | None, Some b when a.kind = -21 -> b
      | Some child, None when a.kind = -21 -> child
      | x, y -> join (Option.value x ~default:(scalar (-1)))
        (Option.value y ~default:(scalar (-1))) (depth + 1)) fields
    in
    { kind = a.kind; fields; children }
let rec shape value depth =
  if depth > max_depth then fail "type nesting limit";
  match value with
  | Scalar (kind, _) -> scalar kind
  | Some_value v -> unary (-18) (shape v (depth + 1))
  | Blob _ -> unary (-19) (scalar (-5))
  | Vector vs -> unary (-19) (List.fold_left (fun a v ->
    join a (shape v (depth + 1)) 0) (scalar (-17)) vs)
  | Record fields ->
    { kind = -20; fields = List.map fst fields;
      children = List.map (fun (_, v) -> shape v (depth + 1)) fields }
  | Variant (k, v) ->
    { kind = -21; fields = [k]; children = [shape v (depth + 1)] }

let encode values =
  let types = List.map (fun v -> shape v 0) values in
  let indices = Hashtbl.create 32 in
  let table = ref [] in
  let rec intern t =
    if not (List.mem t.kind [-18; -19; -20; -21]) then t.kind
    else match Hashtbl.find_opt indices t with
    | Some i -> i
    | None ->
      let i = Hashtbl.length indices in
      if i >= max_types then fail "output type-table limit";
      Hashtbl.add indices t i;
      table := t :: !table;
      List.iter (fun c -> ignore (intern c)) t.children;
      i
  in
  let roots = List.map intern types in
  let out = Buffer.create 128 in
  let add b =
    if Bytes.length b > max_bytes - Buffer.length out then
      fail "output byte limit";
    Buffer.add_bytes out b
  in
  let num n signed = add (leb (Z.of_int n) signed) in
  let field_id n = add (leb (Big_int.big_int_of_int64 n) false) in
  add (Bytes.of_string "DIDL"); num (Hashtbl.length indices) false;
  List.iter (fun t ->
    num t.kind true;
    if t.kind = -18 || t.kind = -19 then num (intern (only t.children)) true
    else begin
      num (List.length t.fields) false;
      List.iter2 (fun k c -> field_id k; num (intern c) true)
        t.fields t.children
    end) (List.rev !table);
  num (List.length roots) false;
  List.iter (fun i -> num i true) roots;
  let rec emit v t depth =
    if depth > max_depth then fail "emitted value nesting limit";
    if t.kind = -18 then
      if is_null v then add (Bytes.of_string "\000")
      else begin
        add (Bytes.of_string "\001");
        emit (match v with Some_value c -> c | _ -> v)
          (only t.children) (depth + 1)
      end
    else match v with
    | Scalar (op, b) when op = t.kind -> add b
    | Blob bytes when t.kind = -19 && (only t.children).kind = -5 ->
      num (Bytes.length bytes) false; add bytes
    | Vector vs when t.kind = -19 ->
      num (List.length vs) false;
      List.iter (fun c -> emit c (only t.children) (depth + 1)) vs
    | Record fields when t.kind = -20 ->
      List.iter2 (fun k c -> emit
        (Option.value (List.assoc_opt k fields) ~default:null) c (depth + 1))
        t.fields t.children
    | Variant (k, v) when t.kind = -21 ->
      let rec find i fields children = match fields, children with
        | k' :: _, c :: _ when k = k' -> i, c
        | _ :: ks, _ :: cs -> find (i + 1) ks cs
        | _ -> fail "missing derived variant"
      in
      let i, c = find 0 t.fields t.children in
      num i false; emit v c (depth + 1)
    | _ -> fail "cannot emit at derived type"
  in
  List.iter2 (fun v t -> emit v t 0) values types;
  Bytes.of_string (Buffer.contents out)

(* The source and IR interpreters retain Motoko types alongside their original
   values. Walk those values into a normalized finite tree, then derive and emit
   the canonical Candid message once. *)
let encode_values types values =
  let module T = Mo_types.Type in
  let module V = Value in
  let module N = Numerics in
  try
    let steps = ref 0 in
    let payload_bytes = ref 0 in
    let step count depth =
      if count > max_items - !steps || depth > max_depth then
        fail "value traversal limit";
      steps := !steps + count
    in
    let bytes b =
      let n = Bytes.length b in
      if n > max_bytes - !payload_bytes then fail "output byte limit";
      payload_bytes := !payload_bytes + n;
      b
    in
    let primitive kind b = Scalar (kind, bytes b) in
    let integer kind signed n =
      if not signed && Big_int.sign_big_int n < 0 then
        fail "negative natural number";
      if Big_int.num_bits_big_int n > 4096 * 7 then
        fail "integer length limit";
      let b = leb n signed in
      if Bytes.length b > 4096 then fail "integer length limit";
      primitive kind b
    in
    let fixed kind width n =
      primitive kind (Bytes.init width (fun i ->
        Char.chr (Z.to_int (Z.logand (Z.shift_right n (8 * i))
          (Z.of_int 255)))))
    in
    let label lab =
      let bits = Idllib.Escape.unescape_hash lab in
      (* Labels are unsigned Candid IDs, unlike Motoko's heap field hashes. *)
      Int64.logand (Int64.of_int32 (Lib.Uint32.to_int32 bits)) 0xffff_ffffL
    in
    let sorted_fields fields =
      let fields = List.sort (fun a b -> Int64.compare (label a.T.lab)
        (label b.T.lab)) fields
      in
      let rec distinct = function
        | a :: (b :: _ as rest) ->
          if label a.T.lab = label b.T.lab then fail "duplicate field IDs";
          distinct rest
        | _ -> ()
      in
      distinct fields;
      fields
    in
    let seen = ref T.S.empty in
    let rec validate = function
      | [] -> ()
      | typ :: pending ->
        let typ = T.normalize typ in
        if T.S.mem typ !seen then validate pending
        else begin
          seen := T.S.add typ !seen;
          let children = match typ with
            | T.Obj (T.Actor, _, _) | T.Func _ -> []
            | T.Obj (_, fields, _) | T.Variant fields ->
              List.map (fun f -> f.T.typ) (sorted_fields fields)
            | T.Tup types -> types
            | T.Array child | T.Opt child -> [child]
            | _ -> []
          in
          validate (children @ pending)
        end
    in
    let rec normalize typ value depth =
      step 1 depth;
      match T.normalize typ, value with
      | T.Any, _ -> primitive (-16) Bytes.empty
      | T.Prim T.Null, V.Null | T.Tup [], V.Tup [] -> null
      | T.Prim T.Bool, V.Bool b ->
        primitive (-2) (Bytes.make 1 (if b then '\001' else '\000'))
      | T.Prim T.Nat, V.Int n -> integer (-3) false (N.Nat.to_big_int n)
      | T.Prim T.Int, V.Int n -> integer (-4) true (N.Int.to_big_int n)
      | T.Prim T.Nat8, V.Nat8 n -> fixed (-5) 1 (N.Nat8.to_big_int n)
      | T.Prim T.Nat16, V.Nat16 n -> fixed (-6) 2 (N.Nat16.to_big_int n)
      | T.Prim T.Nat32, V.Nat32 n -> fixed (-7) 4 (N.Nat32.to_big_int n)
      | T.Prim T.Nat64, V.Nat64 n -> fixed (-8) 8 (N.Nat64.to_big_int n)
      | T.Prim T.Int8, V.Int8 n -> fixed (-9) 1 (N.Int_8.to_big_int n)
      | T.Prim T.Int16, V.Int16 n -> fixed (-10) 2 (N.Int_16.to_big_int n)
      | T.Prim T.Int32, V.Int32 n -> fixed (-11) 4 (N.Int_32.to_big_int n)
      | T.Prim T.Int64, V.Int64 n -> fixed (-12) 8 (N.Int_64.to_big_int n)
      | T.Prim T.Char, V.Char c -> fixed (-7) 4 (Z.of_int c)
      | T.Prim T.Float32, V.Float32 f ->
        let bits = Wasm.F32.to_bits f in
        let bits =
          if Int32.logand bits 0x7f80_0000l = 0x7f80_0000l &&
             Int32.logand bits 0x007f_ffffl <> 0l
          then 0x7fc0_0000l else bits
        in
        fixed (-13) 4 (Big_int.big_int_of_int32 bits)
      | T.Prim T.Float, V.Float f ->
        let bits = Wasm.F64.to_bits f in
        let bits =
          if Int64.logand bits 0x7ff0_0000_0000_0000L =
             0x7ff0_0000_0000_0000L &&
             Int64.logand bits 0x000f_ffff_ffff_ffffL <> 0L
          then 0x7ff8_0000_0000_0000L else bits
        in
        fixed (-14) 8 (Big_int.big_int_of_int64 bits)
      | T.Prim T.Text, V.Text s ->
        if String.length s > max_items then fail "collection length limit";
        if String.length s > max_bytes - !payload_bytes then
          fail "output byte limit";
        let b = Bytes.of_string s in
        if not (valid_utf8 b) then fail "invalid UTF-8";
        primitive (-15) (Bytes.cat (leb (Z.of_int (Bytes.length b)) false) b)
      | T.Prim T.Principal, V.Blob s ->
        let n = String.length s in
        if n > 29 then fail "principal length exceeds 29";
        primitive (-24) (Bytes.cat
          (Bytes.init 2 (fun i -> Char.chr (if i = 0 then 1 else n)))
          (Bytes.of_string s))
      | T.Prim T.Blob, V.Blob s -> binary s depth
      | T.Array child, V.Blob s when T.normalize child = T.Prim T.Nat8 ->
        binary s depth
      | T.Opt _, V.Null -> null
      | T.Opt child, V.Opt v ->
        Some_value (normalize child v (depth + 1))
      | T.Array child, V.Array vs ->
        let n = Array.length vs in
        if n > max_items - !steps then fail "collection length limit";
        if T.normalize child = T.Prim T.Nat8 then begin
          step n (if n = 0 then depth else depth + 1);
          let b = Bytes.init n (fun i -> match vs.(i) with
            | V.Nat8 n -> Char.chr (N.Nat8.to_int n)
            | _ -> fail "value does not match nat8 element type")
          in
          Blob (bytes b)
        end else
          Vector (Array.to_list (Array.map
            (fun v -> normalize child v (depth + 1)) vs))
      | T.Tup ts, V.Tup vs ->
        if List.length ts <> List.length vs then
          fail "value does not match tuple type";
        Record (List.filter_map (fun (i, t, v) ->
          let child = normalize t v (depth + 1) in
          if T.is_opt (T.normalize t) && is_null child then None
          else Some (Int64.of_int i, child))
          (List.mapi (fun i (t, v) -> i, t, v) (List.combine ts vs)))
      | T.Obj (T.Actor, _, _), _ | T.Func _, _ ->
        fail "function/service values outside data profile"
      | T.Obj (_, fields, _), V.Obj values ->
        if List.length fields > max_items - !steps then
          fail "collection length limit";
        let fields = sorted_fields fields in
        Record (List.filter_map (fun f ->
          match V.Env.find_opt f.T.lab values with
          | None -> fail "value does not match record type"
          | Some v ->
            let child = normalize f.T.typ v (depth + 1) in
            if T.is_opt (T.normalize f.T.typ) && is_null child then None
            else Some (label f.T.lab, child)) fields)
      | T.Variant fields, V.Variant (tag, v) ->
        (match T.find_val_field_opt tag fields with
        | None -> fail "value does not match variant type"
        | Some field ->
          Variant (label tag, normalize field.T.typ v (depth + 1)))
      | T.Non, _ -> fail "empty has no values"
      | _ -> fail "value does not match supported type"
    and binary s depth =
      let n = String.length s in
      step n (if n = 0 then depth else depth + 1);
      if n > max_bytes - !payload_bytes then fail "output byte limit";
      Blob (bytes (Bytes.of_string s))
    in
    if List.length types <> List.length values then
      fail "argument type/value count mismatch";
    validate types;
    Ok (encode (List.map2 (fun t v -> normalize t v 0) types values))
  with Invalid message -> Error message
