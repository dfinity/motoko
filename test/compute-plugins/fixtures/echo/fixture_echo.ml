module P = Codegen.Plugin_codegen
module G = Codegen.InstrList

module Generator = struct
  module Make (B : P.Backend) = struct
    let compile env _types =
      let set_input, get_input = B.new_local env "fixture_input" in
      let set_output, get_output = B.new_local env "fixture_output" in
      G.concat [
        set_input;
        B.alloc_blob env (G.concat [get_input; B.Blob.len env]);
        set_output;
        get_input; B.Blob.payload_ptr_unskewed env;
        get_input; B.Blob.len env;
        get_output; B.Blob.payload_ptr_unskewed env;
        B.call env "fixture_echo" "transform";
        get_output;
      ]
  end
end

let binding = Mo_values.Compute_plugin.{
  id = "fixture_echo";
  name = "fixtureEcho";
  arguments = Blob;
  evaluate = (fun _types -> function
    | [Mo_values.Value.Blob bytes] -> Ok bytes
    | _ -> Error "fixtureEcho expects one Blob argument");
}

let () = P.register P.{
  abi = 1;
  name = "fixture_echo";
  runtime = "runtime.wasm";
  imports = ["transform", Wasm_exts.Types.[I64Type; I64Type; I64Type], []];
  bindings = [binding, (module Generator)];
}
