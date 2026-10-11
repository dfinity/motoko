module P = Codegen.Plugin_codegen
module G = Codegen.InstrList

module Generator = struct
  module Make (B : P.Backend) = struct
    let compile env _types =
      let set_input, get_input = B.new_local env "sha256_input" in
      let set_result, get_result = B.new_local env "sha256_result" in
      let size = G.i (Wasm_exts.Ast.Const Wasm.Source.{
        it = Wasm_exts.Values.I64 32L; at = no_region}) in
      G.concat [
        set_input;
        B.alloc_blob env size;
        set_result;
        get_input; B.Blob.payload_ptr_unskewed env;
        get_input; B.Blob.len env;
        get_result; B.Blob.payload_ptr_unskewed env;
        B.call env "sha256" "sha256_digest";
        get_result;
      ]
  end
end

let binding = Mo_values.Compute_plugin.{
  id = "sha256_digest";
  name = "sha256";
  arguments = Blob;
  evaluate = (fun _types -> function
    | [Mo_values.Value.Blob bytes] -> Ok (Sha256_reference.digest bytes)
    | _ -> Error "sha256 expects one Blob argument");
}

let () =
  P.register P.{
    abi = 1;
    name = "sha256";
    runtime = "runtime.wasm";
    imports = ["sha256_digest",
      Wasm_exts.Types.[I64Type; I64Type; I64Type], []];
    bindings = [binding, (module Generator)];
  }
