open CustomModule
open Wasm

(* Lots of code copied from Wasm.Binary.Decode *)

(* Decoding stream *)

type stream =
{
  name : string;
  bytes : string;
  pos : int ref;
}

exception EOS

let stream name bs = {name; bytes = bs; pos = ref 0}

let len s = String.length s.bytes
let pos s = !(s.pos)
let eos s = (pos s = len s)

let check n s = if pos s + n > len s then raise EOS
let skip n s = if n < 0 then raise EOS else check n s; s.pos := !(s.pos) + n

let read s = Char.code (s.bytes.[!(s.pos)])
let peek s = if eos s then None else Some (read s)
let get s = check 1 s; let b = read s in skip 1 s; b
let get_string n s = let i = pos s in skip n s; String.sub s.bytes i n


(* Errors *)

module Code = Error.Make ()
exception Code = Code.Error

let position (s : stream) pos = Source.({file = s.name; line = -1; column = pos})
let region s left right =
  Source.({left = position s left; right = position s right})

let error s pos msg = raise (Code (region s pos pos, msg))
let require b s pos msg = if not b then error s pos msg

let guard f s =
  try f s with EOS -> error s (len s) "unexpected end of section or function"

let get = guard get
let get_string n = guard (get_string n)

(* Generic values *)

let u8 s =
  get s

let u16 s =
  let lo = u8 s in
  let hi = u8 s in
  hi lsl 8 + lo

let u32 s =
  let lo = Int32.of_int (u16 s) in
  let hi = Int32.of_int (u16 s) in
  Int32.(add lo (shift_left hi 16))


let rec vuN n s =
  require (n > 0) s (pos s) "integer representation too long";
  let b = u8 s in
  require (n >= 7 || b land 0x7f < 1 lsl n) s (pos s - 1) "integer too large";
  let x = Int64.of_int (b land 0x7f) in
  if b land 0x80 = 0 then x else Int64.(logor x (shift_left (vuN (n - 7) s) 7))

let vu32 s = Int64.to_int32 (vuN 32 s)

let len32 s =
  let pos = pos s in
  let n = vu32 s in
  if I32.le_u n (Int32.of_int (len s)) then Int32.to_int n else
    error s pos "length out of bounds"

let string s = let n = len32 s in get_string n s
let rec list f n s = if n = 0 then [] else let x = f s in x :: list f (n - 1) s
let vec f s = let n = len32 s in list f n s

let name s =
  let pos = pos s in
  try Utf8.decode (string s) with Utf8.Utf8 ->
    error s pos "invalid UTF-8 encoding"

let sized f s =
  let size = len32 s in
  let start = pos s in
  let x = f size s in
  require (pos s = start + size) s start "section size mismatch";
  x

let sized_skip f s =
  let size = len32 s in
  let start = pos s in
  let x = f s in
  s.pos := start + size;
  x

let id s =
  let bo = peek s in
  Lib.Option.map
    (function
    | 0 -> `CustomSection
    | 1 -> `TypeSection
    | 2 -> `ImportSection
    | 3 -> `FuncSection
    | 4 -> `TableSection
    | 5 -> `MemorySection
    | 6 -> `GlobalSection
    | 7 -> `ExportSection
    | 8 -> `StartSection
    | 9 -> `ElemSection
    | 10 -> `CodeSection
    | 11 -> `DataSection
    | _ -> error s (pos s) "invalid section id"
    ) bo

let var s = vu32 s

(* Dylink section *)

let dylink _size s =
  require (name s = Wasm.Utf8.decode "dylink") s (pos s)
     "Expecting dylib to start with custom dylink section";
  let memory_size = vu32 s in
  let memory_alignment = vu32 s in
  let table_size = vu32 s in
  let table_alignment = vu32 s in
  let needed_dynlibs = vec string s in
  { memory_size; memory_alignment; table_size; table_alignment; needed_dynlibs }

let dylink_section s =
  require (id s = Some `CustomSection) s (pos s)
     "Expecting dylib to start with custom dylink section";
  ignore (u8 s);
  sized dylink s

(* Name section *)

let repeat_until p_end s x0 f =
  let rec go x =
    require (pos s <= p_end) s (pos s) "repeat_until overshot";
    if pos s = p_end then x else go (f x s)
  in go x0

let assoc_list f s = vec (fun s ->
    let i = var s in
    let x = f s in
    (i, x)
  ) s

let name_map = assoc_list string
let indirect_name_map = assoc_list name_map

let name_section_subsection (ns : name_section) s =
  match u8 s with
  | 0 -> (* module name *)
    let mod_name = sized (fun _ -> string) s in
    { ns with module_ = Some mod_name }
  | 1 -> (* function names *)
    let func_names = sized (fun _ -> name_map) s in
    { ns with function_names = ns.function_names @ func_names }
  | 2 -> (* local names *)
    let loc_names = sized (fun _ -> indirect_name_map) s in
    { ns with locals_names = ns.locals_names @ loc_names }
  | i -> error s (pos s) "unknown name section subsection id"

let name_section_content size s =
  let p_end = pos s + size in
  require (name s = Wasm.Utf8.decode "name") s (pos s)
     "Expecting name section to start with \"name\"";
  repeat_until p_end s empty_name_section name_section_subsection


let name_section s =
  match id s with
    | Some `CustomSection -> ignore (u8 s); sized name_section_content s
    | Some _ -> error s (pos s) "Expecting custom section"
    | None -> empty_name_section

(* Other section *)

let skip_other_section s =
  let p = pos s in
  let skip = match id s with
    | Some `CustomSection ->
      ignore (u8 s);
      sized_skip (fun s ->
        name s <> Wasm.Utf8.decode "name"
      ) s
    | Some _ ->
      ignore (u8 s);
      sized_skip (fun _ -> true) s
    | None -> false
  in
  if not skip then s.pos := p;
  skip

(* Modules *)

let rec iterate f s = if f s then iterate f s

let dylib_module s =
  let magic = u32 s in
  require (magic = 0x6d736100l) s 0 "magic header not detected";
  let version = u32 s in
  require (version = Encode.version) s 4 "unknown binary version";
  let dynlink = dylink_section s in
  iterate skip_other_section s;
  let name = name_section s in
  iterate skip_other_section s;
  require (pos s = len s) s (len s) "junk after last section";
  (Some dynlink, name)


let decode name bs =
  let module_ = (Wasm.Decode.decode name bs).Source.it in
  let (dylink, name) = dylib_module (stream name bs) in
  { module_; dylink; name; types = []; persist = [] }
