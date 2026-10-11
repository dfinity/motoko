(* SHA-256 reference semantics for both Motoko interpreters. Keep this portable:
   the canister's implementation is in plugins/runtime/sha256/src/lib.rs. *)

let constants = [|
  0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l;
  0x3956c25bl; 0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l;
  0xd807aa98l; 0x12835b01l; 0x243185bel; 0x550c7dc3l;
  0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l; 0xc19bf174l;
  0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl;
  0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal;
  0x983e5152l; 0xa831c66dl; 0xb00327c8l; 0xbf597fc7l;
  0xc6e00bf3l; 0xd5a79147l; 0x06ca6351l; 0x14292967l;
  0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl; 0x53380d13l;
  0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
  0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l;
  0xd192e819l; 0xd6990624l; 0xf40e3585l; 0x106aa070l;
  0x19a4c116l; 0x1e376c08l; 0x2748774cl; 0x34b0bcb5l;
  0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl; 0x682e6ff3l;
  0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l;
  0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l;
|]

let ( +! ) = Int32.add
let ( ^! ) = Int32.logxor
let ( &! ) = Int32.logand
let shr = Int32.shift_right_logical
let ror x n = Int32.logor (shr x n) (Int32.shift_left x (32 - n))
let sigma0 x = ror x 7 ^! ror x 18 ^! shr x 3
let sigma1 x = ror x 17 ^! ror x 19 ^! shr x 10

let digest input =
  let state = [|
    0x6a09e667l; 0xbb67ae85l; 0x3c6ef372l; 0xa54ff53al;
    0x510e527fl; 0x9b05688cl; 0x1f83d9abl; 0x5be0cd19l;
  |] in
  let words = Array.make 64 0l in
  let compress bytes offset =
    for i = 0 to 15 do
      words.(i) <- String.get_int32_be bytes (offset + i * 4)
    done;
    for i = 16 to 63 do
      words.(i) <- sigma1 words.(i - 2) +! words.(i - 7) +!
        sigma0 words.(i - 15) +! words.(i - 16)
    done;
    let a = ref state.(0) and b = ref state.(1) in
    let c = ref state.(2) and d = ref state.(3) in
    let e = ref state.(4) and f = ref state.(5) in
    let g = ref state.(6) and h = ref state.(7) in
    for i = 0 to 63 do
      let big1 = ror !e 6 ^! ror !e 11 ^! ror !e 25 in
      let choose = (!e &! !f) ^! (Int32.lognot !e &! !g) in
      let t1 = !h +! big1 +! choose +! constants.(i) +! words.(i) in
      let big0 = ror !a 2 ^! ror !a 13 ^! ror !a 22 in
      let majority = (!a &! !b) ^! (!a &! !c) ^! (!b &! !c) in
      let t2 = big0 +! majority in
      h := !g; g := !f; f := !e; e := !d +! t1;
      d := !c; c := !b; b := !a; a := t1 +! t2
    done;
    List.iteri (fun i x -> state.(i) <- state.(i) +! !x)
      [a; b; c; d; e; f; g; h]
  in
  let length = String.length input in
  let complete = length / 64 in
  for i = 0 to complete - 1 do compress input (i * 64) done;
  let remaining = length mod 64 in
  let padded = if remaining < 56 then 64 else 128 in
  let tail = Bytes.make padded '\x00' in
  Bytes.blit_string input (complete * 64) tail 0 remaining;
  Bytes.set tail remaining '\x80';
  Bytes.set_int64_be tail (padded - 8)
    (Int64.shift_left (Int64.of_int length) 3);
  let tail = Bytes.unsafe_to_string tail in
  compress tail 0;
  if padded = 128 then compress tail 64;
  let result = Bytes.create 32 in
  Array.iteri (fun i word -> Bytes.set_int32_be result (i * 4) word) state;
  Bytes.unsafe_to_string result
