(* Specialized direct encoder extracted from the CandidX compiler patch.
   All Motoko layout access goes through the compiler plugin backend. *)
open Codegen
open Codegen.Compile_common
open Codegen.Compile_common.W64_Pointers
open Wasm_exts.Ast
open Wasm_exts.Types
open Mo_types

module Make (B : Plugin_codegen.Backend) = struct
  open B
  module TM = Map.Make (Type.Ord)
  (* Canonical encoding derives a value-dependent type table. Heap access and
     payload writing remain generated for the source types, as in serialize.
     The plugin runtime helpers only intern/join shapes and write the resulting header.
     No callback or GC increment occurs while the scratch context is live. *)
  let canonical_call env name = call env "candidx" ("candid_canonical_" ^ name)

  let canonical_primitive t =
    Option.map (fun n -> Int64.of_int32 (Int32.neg n)) (candid_primitive t)

  (* Large source variants need logarithmic dispatch. Load the Motoko tag
     through the forwarding barrier once, and distinguish its heap hash from
     the Candid field ID used by the selected branch. *)
  let select_variant env value fs result branch =
    let if_ = match result with
      | None -> E.if0
      | Some typ -> E.if1 typ in
    (* Keep the cold error path out of every decision-tree leaf. Besides code
       size, this matters for the IC's per-function complexity limit. *)
    let otherwise =
      Func.share_code Func.Always env "canonical_unexpected_variant" [] []
        (fun env _ -> E.trap_with env "CandidX error: unexpected variant") ^^
      G.i Unreachable in
    if List.length fs <= 4 then
      List.fold_right (fun (id, f) next ->
        value ^^ Variant.test_is env f.Type.lab ^^
        if_ (branch id f) next) fs otherwise
    else
      let set_tag, get_tag = new_local env "canonical_variant_tag" in
      let cases = Array.of_list (List.sort (fun (a, _, _) (b, _, _) ->
        Int64.compare a b) (List.map (fun (id, f) ->
          Variant.hash_variant_label env f.Type.lab, id, f) fs)) in
      let rec tree first last =
        if first = last then otherwise
        else if last - first <= 3 then
          let hash, id, f = cases.(first) in
          get_tag ^^ compile_eq_const hash ^^
          if_ (branch id f) (tree (first + 1) last)
        else
          let middle = first + (last - first) / 2 in
          let hash, _, _ = cases.(middle) in
          get_tag ^^ compile_unboxed_const hash ^^
          compile_comparison I64Op.LtU ^^
          if_ (tree first middle) (tree middle last)
      in
      value ^^ Variant.get_variant_tag env ^^ set_tag ^^
      tree 0 (Array.length cases)

  (* Only joins that can accumulate record fields or variant cases need a
     balanced reducer. Scalar/option/vector chains are cheaper as a fold. *)
  let joins_fields typ =
    let open Type in
    let seen = ref TM.empty in
    let rec visit typ =
      let typ = normalize typ in
      if TM.mem typ !seen then false else begin
        seen := TM.add typ () !seen;
        let field typ = is_opt (normalize typ) || visit typ in
        match typ with
        | Variant _ -> true
        | Tup ts -> List.exists field ts
        | Obj ((Object | Memory), fs, _) ->
          List.exists (fun f -> field f.typ) fs
        | Opt typ | Array typ -> visit typ
        | _ -> false
      end
    in
    visit typ

  let rec canonical_shape env t =
    let open Type in
    let t = normalize t in
    Func.share_code3 Func.Always env (Strm.name_for "canonical_shape" [t])
      (("x", I64Type), ("context", I64Type), ("depth", I64Type)) [I64Type]
      (fun env get_x get_context get_depth ->
        let child value typ =
          match canonical_primitive (normalize typ) with
          | Some op when op <> -17L ->
            get_context ^^ get_depth ^^ compile_add_const 1L ^^
            compile_unboxed_one ^^ canonical_call env "visit" ^^
            compile_unboxed_const op
          | _ -> value ^^ get_context ^^ get_depth ^^ compile_add_const 1L ^^
            canonical_shape env typ
        in
        let unary op shape =
          get_context ^^ compile_unboxed_const op ^^ shape ^^
          canonical_call env "unary"
        in
        let visit depth count =
          get_context ^^ depth ^^ count ^^ canonical_call env "visit"
        in
        let fields op fs =
          get_context ^^ compile_unboxed_const op ^^
          canonical_call env "begin" ^^
          G.concat_map (fun (id, typ, value) ->
            let retain =
              get_context ^^ compile_unboxed_const id ^^ child value typ ^^
              canonical_call env "field"
            in
            match normalize typ with
            | Opt _ when op = -20L ->
              value ^^ Opt.is_some env ^^
              E.if0 retain
                (visit (get_depth ^^ compile_add_const 1L) compile_unboxed_one)
            | _ -> retain
          ) fs ^^
          get_context ^^ canonical_call env "end"
        in
        visit get_depth compile_unboxed_one ^^
        match canonical_primitive t with
        | Some (-17L) -> E.trap_with env "CandidX error: empty has no values"
        | Some op -> compile_unboxed_const op
        | None -> begin match t with
          | Prim Blob ->
            let set_length, get_length = new_local env "binary_length" in
            get_x ^^ Blob.len env ^^ set_length ^^
            get_length ^^ compile_eq_const 0L ^^
            E.if0 G.nop
              (visit (get_depth ^^ compile_add_const 1L) get_length) ^^
            unary (-19L) (compile_unboxed_const (-5L))
          | Opt typ ->
            get_x ^^ Opt.is_some env ^^
            E.if1 I64Type
              (unary (-18L) (child (get_x ^^ Opt.project env typ) typ))
              (compile_unboxed_const (-1L))
          | Tup ts ->
            fields (-20L) (List.mapi (fun i typ ->
              Int64.of_int i, typ,
              get_x ^^ Tuple.load_n env (Int64.of_int i)) ts)
          | Obj ((Object | Memory), fs, _) ->
            fields (-20L) (List.map (fun (id, f) ->
              Wasm.I64_convert.extend_i32_u (Lib.Uint32.to_int32 id), f.typ,
              get_x ^^ Object.load_idx_raw env f.lab
            ) (sort_by_hash fs))
          | Variant fs ->
            let shared = List.length fs > 4 in
            select_variant env get_x (sort_by_hash fs) (Some I64Type)
              (fun id f ->
                let id = Wasm.I64_convert.extend_i32_u (Lib.Uint32.to_int32 id) in
                if shared then
                  get_x ^^ get_context ^^ get_depth ^^ compile_unboxed_const id ^^
                  canonical_case_shape env f.typ
                else get_context ^^ compile_unboxed_const id ^^
                  child (get_x ^^ Variant.project env) f.typ ^^
                  canonical_call env "variant")
          | Array typ ->
            let set_length, get_length = new_local env "vector_length" in
            get_x ^^ Arr.len env ^^ set_length ^^
            get_length ^^ compile_unboxed_const 2_000_000L ^^
            compile_comparison I64Op.GtU ^^
            E.then_trap_with env "CandidX error: collection length limit" ^^
            begin match canonical_primitive (normalize typ), normalize typ with
            | Some op, _ when op <> -17L ->
              get_length ^^ compile_eq_const 0L ^^
              E.if0 G.nop
                (visit (get_depth ^^ compile_add_const 1L) get_length) ^^
              unary (-19L) (if op = -5L then compile_unboxed_const op else
                get_length ^^ compile_eq_const 0L ^^
                E.if1 I64Type (compile_unboxed_const (-17L))
                  (compile_unboxed_const op))
            | _, Variant fs when List.for_all (fun f ->
                match canonical_primitive (normalize f.typ) with
                | Some op -> op <> -17L
                | None -> false) fs ->
              (* Each case has one fixed primitive payload shape. Collect
                 selected fields and intern the union once, without retaining
                 singleton shapes or intermediate unions. Compound payloads
                 still use the general balanced reduction below. *)
              unary (-19L) (
                get_length ^^ compile_eq_const 0L ^^
                E.if1 I64Type (compile_unboxed_const (-17L))
                  (get_length ^^ compile_eq_const 1L ^^
                   E.if1 I64Type
                     (child (get_x ^^ compile_unboxed_zero ^^
                       Arr.unsafe_idx env ^^ load_ptr) typ)
                     (visit (get_depth ^^ compile_add_const 1L) get_length ^^
                      visit (get_depth ^^ compile_add_const 2L) get_length ^^
                      get_context ^^ canonical_call env "variants_begin" ^^
                      Arr.for_each env get_x get_length (fun value ->
                        select_variant env value (sort_by_hash fs) None
                          (fun id f ->
                            match canonical_primitive (normalize f.typ) with
                            | Some op ->
                              get_context ^^ compile_unboxed_const
                                (Wasm.I64_convert.extend_i32_u
                                  (Lib.Uint32.to_int32 id)) ^^
                              compile_unboxed_const op ^^
                              canonical_call env "variants_push"
                            | None -> E.trap_with env
                                "CandidX error: unsupported source type")
                      ) ^^
                      get_context ^^ canonical_call env "variants_end")))
            | _, _ when not (joins_fields typ) ->
              let set_joined, get_joined = new_local env "joined_type" in
              compile_unboxed_const (-17L) ^^ set_joined ^^
              Arr.for_each env get_x get_length (fun value ->
                get_context ^^ get_joined ^^
                child value typ ^^
                canonical_call env "join" ^^ set_joined
              ) ^^
              unary (-19L) get_joined
            | _ ->
              unary (-19L) (
                get_length ^^ compile_eq_const 0L ^^
                E.if1 I64Type (compile_unboxed_const (-17L))
                  (get_length ^^ compile_eq_const 1L ^^
                   E.if1 I64Type
                     (child (get_x ^^ compile_unboxed_zero ^^
                       Arr.unsafe_idx env ^^ load_ptr) typ)
                     (get_context ^^ canonical_call env "join_begin" ^^
                      Arr.for_each env get_x get_length (fun value ->
                        get_context ^^
                        child value typ ^^
                        canonical_call env "join_push"
                      ) ^^
                      get_context ^^ canonical_call env "join_end")))
            end
          | Func _ | Obj (Actor, _, _) ->
            E.trap_with env
              "CandidX error: function and service values are excluded"
          | _ -> E.trap_with env "CandidX error: unsupported source type"
        end)

  (* Share child visits for equal payload types and intern the selected case
     directly, without opening an intermediate field-builder frame. *)
  and canonical_case_shape env typ =
    Func.share_code Func.Always env (Strm.name_for "canonical_case_shape" [typ])
      ["x", I64Type; "context", I64Type; "depth", I64Type; "case_id", I64Type]
      [I64Type] (fun env getters ->
        let get_x = List.nth getters 0 in
        let get_context = List.nth getters 1 in
        let get_depth = List.nth getters 2 in
        let get_id = List.nth getters 3 in
        let child = match canonical_primitive (Type.normalize typ) with
          | Some op when op <> -17L ->
            get_context ^^ get_depth ^^ compile_add_const 1L ^^
            compile_unboxed_one ^^ canonical_call env "visit" ^^
            compile_unboxed_const op
          | _ ->
            get_x ^^ Variant.project env ^^ get_context ^^
            get_depth ^^ compile_add_const 1L ^^ canonical_shape env typ
        in
        get_context ^^ get_id ^^ child ^^ canonical_call env "variant")

  let canonical_fixed_size t =
    let open Type in
    let memo = ref TM.empty in
    let rec size depth t =
      if depth > 256 then None else
      let t = normalize t in
      match TM.find_opt t !memo with
      | Some result -> result
      | None ->
      memo := TM.add t None !memo;
      let fields ts = List.fold_left (fun total typ ->
        match total with
        | None -> None
        | Some total -> match size (depth + 1) typ with
          | Some n when Int64.add total n <= 33_554_432L ->
            Some (Int64.add total n)
          | _ -> None) (Some 0L) ts in
      let result = match t with
      | Prim Null | Any -> Some 0L
      | Prim (Bool | Nat8 | Int8) -> Some 1L
      | Prim (Nat16 | Int16) -> Some 2L
      | Prim (Nat32 | Int32 | Char | Float32) -> Some 4L
      | Prim (Nat64 | Int64 | Float) -> Some 8L
      | Tup ts -> fields ts
      | Obj ((Object | Memory), fs, _) -> fields (List.map (fun f -> f.typ) fs)
      | _ -> None in
      memo := TM.add t result !memo;
      result
    in
    size 0 t

  (* Emit=false measures the exact payload under its derived wire type. This
     separate measurement handles vector joins without keeping a payload tree
     or allocating a capacity buffer that must later be copied or resized. *)
  let rec canonical_payload ?(static = false) ?(force_share = false) env emit t =
    let open Type in
    let t = normalize t in
    let name = Strm.name_for
      ((if static then "canonical_static_" else "canonical_") ^
       if emit then "write" else "size") [t] in
    let sharing =
      if not force_share && canonical_primitive t <> None then Func.Never
      else Func.Always in
    let parameters = if static then ["x", I64Type; "position", I64Type]
      else ["x", I64Type; "context", I64Type; "wire_type", I64Type;
        "position", I64Type] in
    Func.share_code sharing env name parameters
      [I64Type] (fun env getters ->
        let get_x = List.nth getters 0 in
        let get_context = if static then G.nop else List.nth getters 1 in
        let get_wire = if static then G.nop else List.nth getters 2 in
        let get_position = List.nth getters (if static then 1 else 3) in
        let set_position = G.setter_for get_position in
        let advance n =
          get_position ^^ n ^^ G.i (Binary (Wasm_exts.Values.I64 I64Op.Add)) ^^
          set_position
        in
        let bytes n = advance (compile_unboxed_const n) in
        let word value = if emit then
          Strm.write_word_leb env get_position value else
          let set_word, get_word = new_local env "word" in
          value ^^ set_word ^^ advance (I32Leb.compile_leb128_size get_word)
        in
        let marker n = if emit then
          Strm.write_byte env get_position (compile_unboxed_const n)
          else bytes 1L
        in
        let child ?(share = false) value typ wire =
          match emit, canonical_fixed_size typ with
          | false, Some n -> bytes n
          | _ ->
            let wire = match canonical_primitive (normalize typ) with
              | Some op -> compile_unboxed_const op
              | None -> wire in
            value ^^
            (if static then G.nop else get_context ^^ wire) ^^
            get_position ^^
            canonical_payload ~static ~force_share:share env emit typ ^^
            set_position
        in
        let unary_child =
          if static then compile_unboxed_const (-5L) else
            get_context ^^ get_wire ^^ canonical_call env "child" in
        let fields fs = G.concat_map (fun (id, typ, value) ->
          if static then child value typ G.nop else
          match canonical_primitive (normalize typ) with
          | Some op when op <> -17L ->
            child value typ (compile_unboxed_const op)
          | _ ->
            let set_field, get_field = new_local env "field_type" in
            get_context ^^ get_wire ^^ compile_unboxed_const id ^^
            canonical_call env "field_type" ^^ set_field ^^
            get_field ^^ compile_eq_const Int64.min_int ^^
            E.if0 G.nop (child value typ get_field)
        ) fs in
        let fixed n code = if emit then code else bytes n in
        let raw_bytes size write =
          let set_length, get_length = new_local env "payload_length" in
          size ^^ set_length ^^
          get_length ^^ compile_unboxed_const 2_000_000L ^^
          compile_comparison I64Op.GtU ^^
          E.then_trap_with env "CandidX error: collection length limit" ^^
          (if emit then write else word get_length ^^ advance get_length)
        in
        let body = match t with
        | Prim Nat | Prim Int ->
          if emit then (if t = Prim Nat then Strm.write_bignum_leb else
            Strm.write_bignum_sleb) env get_position get_x
          else
            let set_length, get_length = new_local env "integer_length" in
            get_x ^^ BitTagged.if_tagged_scalar env [I64Type]
              (Bool.lit true)
              (get_x ^^ BigNumLibtommath.fits_unsigned_bits env 28_672) ^^
            E.else_trap_with env "CandidX error: integer length limit" ^^
            get_x ^^ buffer_size env t ^^ G.i Drop ^^ set_length ^^
            get_length ^^ compile_unboxed_const 4096L ^^
            compile_comparison I64Op.GtU ^^
            E.then_trap_with env "CandidX error: integer length limit" ^^
            advance get_length
        | Prim Float32 ->
          fixed 4L (
            let set_float, get_float, _ = new_local_ env F32Type "float" in
            Strm.reserve env get_position 4L ^^
            get_x ^^ unbox_float32 env ^^
            set_float ^^
            get_float ^^ get_float ^^ compile_comparison_f32 F32Op.Ne ^^
            E.if1 I32Type (compile_const_32 0x7fc00000l)
              (get_float ^^
                G.i (Convert (Wasm_exts.Values.I32 I32Op.ReinterpretFloat))) ^^
            G.i (Store {ty = I32Type; align = 0; offset = 0L; sz = None}))
        | Prim Float ->
          fixed 8L (
            let set_float, get_float, _ = new_local_ env F64Type "float" in
            Strm.reserve env get_position 8L ^^
            get_x ^^ Float.unbox env ^^ set_float ^^
            get_float ^^ get_float ^^ compile_comparison_f64 F64Op.Ne ^^
            E.if1 I64Type (compile_unboxed_const 0x7ff8000000000000L)
              (get_float ^^
                G.i (Convert (Wasm_exts.Values.I64 I64Op.ReinterpretFloat))) ^^
            G.i (Store {ty = I64Type; align = 0; offset = 0L; sz = None}))
        | Prim (Int64 | Nat64 as pty) ->
          fixed 8L (Strm.reserve env get_position 8L ^^
            get_x ^^ BoxedWord64.unbox env pty ^^
            G.i (Store {ty = I64Type; align = 0; offset = 0L; sz = None}))
        | Prim (Int32 | Nat32 as pty) ->
          fixed 4L (Strm.write_word_32 env get_position
            (get_x ^^ TaggedSmallWord.lsb_adjust pty))
        | Prim Char ->
          fixed 4L (Strm.write_word_32 env get_position
            (get_x ^^ TaggedSmallWord.lsb_adjust_codepoint env))
        | Prim (Int16 | Nat16 as pty) ->
          fixed 2L (Strm.reserve env get_position 2L ^^
            get_x ^^ TaggedSmallWord.lsb_adjust pty ^^
            G.i (Convert (Wasm_exts.Values.I32 I32Op.WrapI64)) ^^
            G.i (Store {ty = I32Type; align = 0; offset = 0L;
              sz = Some Pack16}))
        | Prim (Int8 | Nat8 as pty) ->
          fixed 1L (Strm.write_byte env get_position
            (get_x ^^ TaggedSmallWord.lsb_adjust pty))
        | Prim Bool -> fixed 1L (Strm.write_byte env get_position get_x)
        | Prim Null | Any | Tup [] -> G.nop
        | Prim Blob ->
          raw_bytes (get_x ^^ Blob.len env)
            (Strm.write_blob env get_position get_x)
        | Prim Text ->
          raw_bytes (get_x ^^ Text.size env)
            (Strm.write_text env get_position get_x)
        | Prim Principal ->
          marker 1L ^^ raw_bytes (get_x ^^ Blob.len env)
            (Strm.write_blob env get_position get_x)
        | Opt typ ->
          get_x ^^ Opt.is_some env ^^
          E.if0
            (marker 1L ^^ child (get_x ^^ Opt.project env typ) typ unary_child)
            (get_wire ^^ compile_eq_const (-1L) ^^
             E.if0 G.nop (marker 0L))
        | Tup ts -> fields (List.mapi (fun i typ ->
            Int64.of_int i, typ, get_x ^^ Tuple.load_n env (Int64.of_int i)) ts)
        | Obj ((Object | Memory), fs, _) -> fields (List.map (fun (id, f) ->
            Wasm.I64_convert.extend_i32_u (Lib.Uint32.to_int32 id), f.typ,
            get_x ^^ Object.load_idx_raw env f.lab
          ) (sort_by_hash fs))
        | Variant fs ->
          let shared = List.length fs > 4 in
          select_variant env get_x (sort_by_hash fs) None (fun id f ->
            let id = compile_unboxed_const
              (Wasm.I64_convert.extend_i32_u (Lib.Uint32.to_int32 id)) in
            if shared then
              get_x ^^ get_context ^^ get_wire ^^ id ^^ get_position ^^
              canonical_case_payload env emit f.typ ^^ set_position
            else
              word (get_context ^^ get_wire ^^ id ^^
                canonical_call env "case_index") ^^
              child (get_x ^^ Variant.project env) f.typ
                (get_context ^^ get_wire ^^ id ^^
                  canonical_call env "field_type"))
        | Array typ ->
          let set_length, get_length = new_local env "vector_length" in
          let set_child, get_child = new_local env "element_type" in
          get_x ^^ Arr.len env ^^ set_length ^^ word get_length ^^
          begin match emit, canonical_fixed_size typ with
          | false, Some width -> advance (get_length ^^ compile_mul_const width)
          | true, Some 0L -> G.nop
          | _ -> unary_child ^^ set_child ^^
            Arr.for_each env get_x get_length (fun value ->
              child value typ get_child)
          end
        | _ -> E.trap_with env "CandidX error: unsupported source value"
        in
        body ^^
        (if emit then G.nop else
          get_position ^^ compile_unboxed_const 33_554_432L ^^
          compile_comparison I64Op.GtU ^^
          E.then_trap_with env "CandidX error: output byte limit") ^^
        get_position)

  (* Share the work after case selection as well as the scalar payload. Wide
     variants otherwise duplicate LEB loops and thousands of locals, exceeding
     the IC's per-function complexity limit even with logarithmic dispatch. *)
  and canonical_case_payload env emit typ =
    let name = Strm.name_for
      (if emit then "canonical_case_write" else "canonical_case_size")
      [typ] in
    Func.share_code Func.Always env name
      ["x", I64Type; "context", I64Type; "wire_type", I64Type;
       "case_id", I64Type; "position", I64Type] [I64Type]
      (fun env getters ->
        let get_x = List.nth getters 0 in
        let get_context = List.nth getters 1 in
        let get_wire = List.nth getters 2 in
        let get_id = List.nth getters 3 in
        let get_position = List.nth getters 4 in
        let set_index, get_index = new_local env "case_index" in
        let prefix =
          get_context ^^ get_wire ^^ get_id ^^
          canonical_call env "case_index" ^^ set_index ^^
          if emit then Strm.write_word_leb env get_position get_index else
            get_position ^^ I32Leb.compile_leb128_size get_index ^^
            G.i (Binary (Wasm_exts.Values.I64 I64Op.Add)) ^^
            G.setter_for get_position
        in
        prefix ^^
        match emit, canonical_fixed_size typ with
        | false, Some width -> get_position ^^ compile_add_const width
        | true, Some 0L -> get_position
        | _ ->
          let wire = match canonical_primitive (Type.normalize typ) with
            | Some op -> compile_unboxed_const op
            | None -> get_context ^^ get_wire ^^ get_id ^^
              canonical_call env "field_type" in
          get_x ^^ Variant.project env ^^ get_context ^^ wire ^^
          get_position ^^ canonical_payload ~force_share:true env emit typ)

  (* Candid label collisions must also be rejected in unused source branches.
     This compile-time walk emits a normal runtime trap for an invalid schema;
     it does not change the shared-type rules or ordinary to_candid operator. *)
  let canonical_schema_valid ts =
    let open Type in
    let seen = ref TM.empty in
    let rec visit t =
      let t = normalize t in
      if TM.mem t !seen then true else begin
        seen := TM.add t () !seen;
        match t with
        | Obj ((Object | Memory), fs, _) | Variant fs ->
          let sorted = sort_by_hash fs in
          let rec unique = function
            | (a, _) :: ((b, _) :: _ as rest) ->
              Lib.Uint32.compare a b < 0 && unique rest
            | _ -> true
          in
          unique sorted && List.for_all (fun f -> visit f.typ) fs
        | Tup ts -> List.for_all visit ts
        | Array t | Opt t -> visit t
        | _ -> true
      end
    in
    List.for_all visit ts

  type static_shape =
    | Static_scalar of int64
    | Static_vector of static_shape
    | Static_record of (int64 * static_shape) list

  (* Apart from the empty/nonempty choice for one outer vector, these wire
     shapes are independent of their values. Deduplicate by Candid shape,
     not Motoko type (for example Char and Nat32 have the same wire type). *)
  let static_header ?vector_empty ts =
    let open Type in
    let exception Dynamic_shape in
    let visits = ref 0 in
    let rec shape depth typ =
      incr visits;
      if depth > 256 || !visits > 2_000_000 then raise Dynamic_shape;
      let typ = normalize typ in
      match canonical_primitive typ with
      | Some op when op <> -17L && op >= -24L -> Static_scalar op
      | _ -> match typ with
        | Prim Blob -> Static_vector (Static_scalar (-5L))
        | Array t when normalize t = Prim Nat8 -> Static_vector (Static_scalar (-5L))
        | Array t when depth = 0 && vector_empty <> None ->
          let fixed_visits = !visits in
          let child = if vector_empty = Some true then Static_scalar (-17L)
            else shape (depth + 1) t in
          (* Element visits are charged against the runtime length, rather
             than counting the element type once as if it were a value. *)
          visits := fixed_visits;
          Static_vector child
        | Tup ts -> Static_record (List.mapi (fun i t ->
            Int64.of_int i, shape (depth + 1) t) ts)
        | Obj ((Object | Memory), fs, _) ->
          Static_record (List.map (fun (id, f) ->
            Wasm.I64_convert.extend_i32_u (Lib.Uint32.to_int32 id),
            shape (depth + 1) f.typ) (sort_by_hash fs))
        | _ -> raise Dynamic_shape
    in
    try
      let shapes = List.map (shape 0) ts in
      let ids = Hashtbl.create 16 in
      let table = ref [] in
      let rec number = function
        | Static_scalar op -> op
        | shape -> match Hashtbl.find_opt ids shape with
          | Some id -> id
          | None ->
            let id = Hashtbl.length ids in
            if id >= 1024 then raise Dynamic_shape;
            let id = Int64.of_int id in
            Hashtbl.add ids shape id;
            table := shape :: !table;
            (match shape with
            | Static_record fs ->
              List.iter (fun (_, s) -> ignore (number s)) fs
            | Static_vector child -> ignore (number child)
            | Static_scalar _ -> ());
            id
      in
      let roots = List.map number shapes in
      let out = Buffer.create 32 in
      let rec leb signed n =
        let byte = Int64.to_int (Int64.logand n 127L) in
        let next = Int64.shift_right n 7 in
        let last = if signed then
          (next = 0L && byte < 64) || (next = -1L && byte >= 64)
          else next = 0L in
        Buffer.add_char out (Char.chr (byte lor if last then 0 else 128));
        if not last then leb signed next
      in
      let count n = leb false (Int64.of_int n) in
      Buffer.add_string out "DIDL";
      count (Hashtbl.length ids);
      List.iter (function
        | Static_vector child -> leb true (-19L); leb true (number child)
        | Static_record fs ->
          leb true (-20L);
          count (List.length fs);
          List.iter (fun (id, s) ->
            leb false id; leb true (number s)) fs
        | Static_scalar _ -> ()
      ) (List.rev !table);
      count (List.length roots);
      List.iter (leb true) roots;
      Some (Buffer.contents out, !visits)
    with Dynamic_shape -> None

  (* Called only after static_header has established a finite fixed shape. *)
  let rec static_nodes typ =
    let open Type in
    1 + match normalize typ with
    | Tup ts -> List.fold_left (fun count t -> count + static_nodes t) 0 ts
    | Obj ((Object | Memory), fs, _) ->
      List.fold_left (fun count f -> count + static_nodes f.typ) 0 fs
    | _ -> 0

  let rec has_static_binary typ =
    let open Type in
    match normalize typ with
    | Prim Blob | Array _ -> true
    | Tup ts -> List.exists has_static_binary ts
    | Obj ((Object | Memory), fs, _) ->
      List.exists (fun f -> has_static_binary f.typ) fs
    | _ -> false

  (* Fixed visits were counted at compile time. Charge vector elements in one
     batch, then inspect only the binary lengths that depend on their values.
     Nonempty binaries at the maximum depth still need the value-depth check. *)
  let rec static_binary_visits env remaining value typ depth =
    let open Type in
    match normalize typ with
    | Prim Blob | Array _ ->
      let set_length, get_length = new_local env "binary_visits" in
      value ^^ (match normalize typ with
        | Prim Blob -> Blob.len env
        | _ -> Arr.len env) ^^ set_length ^^
      (if depth = 256 then
        get_length ^^ compile_eq_const 0L ^^
        E.else_trap_with env "CandidX error: value nesting limit"
       else G.nop) ^^
      (match normalize typ with
      | Array _ ->
        get_length ^^ compile_unboxed_const 2_000_000L ^^
        compile_comparison I64Op.GtU ^^
        E.then_trap_with env "CandidX error: collection length limit"
      | _ -> G.nop) ^^
      let element_nodes = match normalize typ with
        | Array t -> static_nodes t
        | _ -> 1 in
      let visits = get_length ^^ compile_mul_const (Int64.of_int element_nodes) in
      visits ^^ remaining ^^ compile_comparison I64Op.GtU ^^
      E.then_trap_with env "CandidX error: value traversal limit" ^^
      remaining ^^ visits ^^
      G.i (Binary (Wasm_exts.Values.I64 I64Op.Sub)) ^^
      G.setter_for remaining ^^
      (match normalize typ with
      | Array t when has_static_binary t ->
        Arr.for_each env value get_length (fun element ->
          static_binary_visits env remaining element t (depth + 1))
      | _ -> G.nop)
    | Tup ts -> G.concat_mapi (fun i t ->
        static_binary_visits env remaining
          (value ^^ Tuple.load_n env (Int64.of_int i)) t (depth + 1)) ts
    | Obj ((Object | Memory), fs, _) -> G.concat_map (fun (_, f) ->
        static_binary_visits env remaining
          (value ^^ Object.load_idx_raw env f.lab) f.typ (depth + 1)
      ) (sort_by_hash fs)
    | _ -> G.nop

  let write_static_header get_position header =
    let rec write offset =
      let remaining = String.length header - offset in
      if remaining = 0 then G.nop else
      let width = if remaining >= 8 then 8 else
        if remaining >= 4 then 4 else if remaining >= 2 then 2 else 1 in
      let word = ref 0L in
      for i = 0 to width - 1 do
        word := Int64.logor !word (Int64.shift_left
          (Int64.of_int (Char.code header.[offset + i])) (8 * i))
      done;
      let ty, sz, constant = match width with
        | 8 -> I64Type, None, compile_unboxed_const !word
        | 4 -> I32Type, None, compile_const_32 (Int64.to_int32 !word)
        | 2 -> I32Type, Some Pack16, compile_const_32 (Int64.to_int32 !word)
        | _ -> I32Type, Some Pack8, compile_const_32 (Int64.to_int32 !word)
      in
      get_position ^^ constant ^^
      G.i (Store {ty; align = 0; offset = Int64.of_int offset; sz}) ^^
      write (offset + width)
    in
    write 0 ^^
    get_position ^^ compile_add_const (Int64.of_int (String.length header)) ^^
    G.setter_for get_position

  let serialize_static ?(suffix = "") env ts header visits =
    Func.share_code1 Func.Always env
      (Strm.name_for ("serialize_canonical" ^ suffix) ts)
      ("x", I64Type) [I64Type] (fun env get_x ->
        let set_remaining, get_remaining = new_local env "remaining_visits" in
        let set_size, get_size = new_local env "canonical_size" in
        let set_blob, get_blob = new_local env "canonical_blob" in
        let set_position, get_position = new_local env "canonical_position" in
        let argument i = if List.length ts = 1 then get_x else
          get_x ^^ Tuple.load_n env (Int64.of_int i) in
        compile_unboxed_const (Int64.of_int (2_000_000 - visits)) ^^
        set_remaining ^^
        G.concat_mapi (fun i typ ->
          static_binary_visits env get_remaining (argument i) typ 0) ts ^^
        compile_unboxed_const (Int64.of_int (String.length header)) ^^
        set_size ^^
        G.concat_mapi (fun i typ ->
          argument i ^^ get_size ^^ canonical_payload ~static:true env false
            typ ^^ set_size) ts ^^
        alloc_blob env get_size ^^ set_blob ^^
        get_blob ^^ Blob.payload_ptr_unskewed env ^^ set_position ^^
        write_static_header get_position header ^^
        G.concat_mapi (fun i typ ->
          argument i ^^ get_position ^^ canonical_payload ~static:true env true
            typ ^^ set_position) ts ^^
        get_position ^^ get_blob ^^ Blob.payload_ptr_unskewed env ^^
        get_size ^^ G.i (Binary (Wasm_exts.Values.I64 I64Op.Add)) ^^
        compile_comparison I64Op.Eq ^^
        E.else_trap_with env "CandidX error: output buffer not filled" ^^
        get_blob)

  let serialize_canonical env ts : G.t =
    Func.share_code1 Func.Always env
      (Strm.name_for "serialize_canonical" ts)
      ("x", I64Type) [I64Type] (fun env get_x ->
        let set_context, get_context = new_local env "canonical_context" in
        let set_header_size, get_header_size =
          new_local env "canonical_header_size" in
        let set_size, get_size = new_local env "canonical_payload_size" in
        let set_blob, get_blob = new_local env "canonical_blob" in
        let set_position, get_position = new_local env "canonical_position" in
        let argument i = if List.length ts = 1 then get_x else
          get_x ^^ Tuple.load_n env (Int64.of_int i)
        in
        let wire i = get_context ^^ compile_unboxed_const (Int64.of_int i) ^^
          canonical_call env "root_type"
        in
        canonical_call env "new" ^^ set_context ^^
        G.concat_mapi (fun i typ ->
          get_context ^^ argument i ^^ get_context ^^ compile_unboxed_zero ^^
          canonical_shape env typ ^^ canonical_call env "root"
        ) ts ^^
        get_context ^^ canonical_call env "types_size" ^^ set_header_size ^^
        compile_unboxed_zero ^^ set_size ^^
        G.concat_mapi (fun i typ ->
          argument i ^^ get_context ^^ wire i ^^ get_size ^^
          canonical_payload env false typ ^^ set_size
        ) ts ^^
        get_size ^^ get_header_size ^^
        G.i (Binary (Wasm_exts.Values.I64 I64Op.Add)) ^^
        set_size ^^
        get_size ^^ compile_unboxed_const 33_554_432L ^^
        compile_comparison I64Op.GtU ^^
        E.then_trap_with env "CandidX error: output byte limit" ^^
        alloc_blob env get_size ^^ set_blob ^^
        get_context ^^ get_blob ^^ Blob.payload_ptr_unskewed env ^^
        canonical_call env "write_types" ^^
        get_blob ^^ Blob.payload_ptr_unskewed env ^^
        G.i (Binary (Wasm_exts.Values.I64 I64Op.Add)) ^^ set_position ^^
        G.concat_mapi (fun i typ ->
          argument i ^^ get_context ^^ wire i ^^ get_position ^^
          canonical_payload env true typ ^^ set_position
        ) ts ^^
        get_position ^^ get_blob ^^ Blob.payload_ptr_unskewed env ^^
        get_size ^^ G.i (Binary (Wasm_exts.Values.I64 I64Op.Add)) ^^
        compile_comparison I64Op.Eq ^^
        E.else_trap_with env "CandidX error: output buffer not filled" ^^
        get_context ^^ canonical_call env "done" ^^
        get_blob)

  let compile env ts =
    if not (canonical_schema_valid ts) then
      E.trap_with env
        "CandidX error: source type field ids must strictly increase"
    else match static_header ts with
    | Some (header, visits) -> serialize_static env ts header visits
    | None -> match ts with
      | [typ] when (match Type.normalize typ with
          | Type.Array _ -> true | _ -> false) ->
        (match static_header ~vector_empty:true ts,
               static_header ~vector_empty:false ts with
        | Some (empty, visits), Some (nonempty, _) ->
          let set_x, get_x = new_local env "canonical_vector" in
          set_x ^^ get_x ^^ Arr.len env ^^ compile_eq_const 0L ^^
          E.if1 I64Type
            (get_x ^^ serialize_static ~suffix:"_empty" env ts empty visits)
            (get_x ^^ serialize_static env ts nonempty visits)
        | _ -> serialize_canonical env ts)
      | _ -> serialize_canonical env ts
end
