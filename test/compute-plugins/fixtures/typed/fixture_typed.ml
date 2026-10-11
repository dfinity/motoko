module P = Codegen.Plugin_codegen
module G = Codegen.InstrList
module T = Mo_types.Type

(* A small typed intrinsic: one byte describing each argument's static type.
   The deliberate function rejection also tests interpreter/backend errors. *)
let shape types =
  if List.exists (fun typ -> T.is_func (T.promote typ)) types then
    Error "fixtureShape does not support function values"
  else
    Ok (String.init (List.length types) (fun i ->
      match T.promote (List.nth types i) with
      | T.Prim T.Nat -> 'n'
      | T.Prim T.Text -> 't'
      | T.Prim T.Bool -> 'b'
      | T.Prim T.Blob -> 'B'
      | T.Tup _ -> 'T'
      | T.Array _ -> 'a'
      | T.Opt _ -> 'o'
      | T.Obj _ -> 'r'
      | T.Variant _ -> 'v'
      | _ -> '?'))

module Generator = struct
  module Make (B : P.Backend) = struct
    let compile env types =
      match shape types with
      | Error message -> B.E.trap_with env message
      | Ok bytes ->
        let set_output, get_output = B.new_local env "fixture_output" in
        let byte i = B.compile_unboxed_const
          (Int64.of_int (Char.code bytes.[i])) in
        G.concat [
          G.i Wasm_exts.Ast.Drop;
          B.alloc_blob env
            (B.compile_unboxed_const (Int64.of_int (String.length bytes)));
          set_output;
          G.concat (List.init (String.length bytes) (fun i -> G.concat [
            byte i;
            get_output; B.Blob.payload_ptr_unskewed env;
            B.compile_add_const (Int64.of_int i);
            B.call env "fixture_typed" "transform";
          ]));
          get_output;
        ]
  end
end

let binding = Mo_values.Compute_plugin.{
  id = "fixture_shape";
  name = "fixtureShape";
  arguments = Shared;
  evaluate = (fun types _values -> shape types);
}

let () = P.register P.{
  abi = 1;
  name = "fixture_typed";
  runtime = "runtime.wasm";
  (* Deliberately shares the echo runtime's symbol with a different type. *)
  imports = ["transform", Wasm_exts.Types.[I64Type; I64Type], []];
  bindings = [binding, (module Generator)];
}
