open Wasm_exts
open Source
open Mo_config

module Js = Js_of_ocaml.Js

let position_of_pos pos =
  object%js
    (* The LSP spec requires zero-based positions *)
    val line = if pos.line > 0 then pos.line - 1 else 0
    val character = pos.column
  end

let range_of_region at =
  object%js
    val start = position_of_pos at.left
    val _end = position_of_pos at.right
  end

let diagnostics_of_msg (msg : Diag.message) =
  Diag.(object%js
    val source = Js.string "motoko"
    val severity = match msg.sev with Diag.Error -> 1 | (Diag.Warning | Diag.Info)  -> 2
    val range = range_of_region msg.at
    val message = Js.string msg.text
  end)

let diagnostics_of_msgs (msgs : Diag.message list) =
  Array.of_list (List.map diagnostics_of_msg msgs)

let js_check source =
  let msgs = match
    Pipeline.check_string (Js.to_string source) Filename.current_dir_name with
    | Error msgs -> msgs
    | Ok (_,  msgs) -> msgs in
  object%js
    val diagnostics = Js.array (diagnostics_of_msgs msgs)
    val code = Js.null
  end

let js_run source =
  let results = Pipeline.run_string (Js.to_string source) in
  let result = String.concat "\n" results in
  Js.string result
  
let js_compile_with mode_string do_link source convert =
  let mode =
    match Js.to_string mode_string with
    | "wasm" -> Flags.WasmMode
    | "dfinity" -> Flags.ICMode
    | "icref" -> Flags.RefMode
    | _ -> raise (Invalid_argument "js_compile_with: Unexpected mode")
  in
  match Pipeline.compile_string mode (Js.to_bool do_link) (Js.to_string source) Filename.current_dir_name with
  | Ok (module_, msgs) ->
    let (code, map) = convert module_ in
    object%js
      val diagnostics = Js.array (diagnostics_of_msgs msgs)
      val code = Js.some code
      val map = Js.some map
    end
  | Error msgs ->
    object%js
      val diagnostics = Js.array (diagnostics_of_msgs msgs)
      val code = Js.null
      val map = Js.null
    end

let js_compile_wasm mode do_link s =
  js_compile_with mode do_link s
    (fun m ->
      let (map, wasm) = CustomModuleEncode.encode m in
      Js_of_ocaml.Typed_array.Bigstring.to_arrayBuffer (Bigstring.of_string wasm), Js.string map
    )

let () =
  Js.export "Motoko"
    (object%js
      method check s = js_check s
      method compileWasm mode do_link s = js_compile_wasm mode do_link s
      method run s = js_run s
    end);
