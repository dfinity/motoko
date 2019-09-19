open Wasm_exts
open Source

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
    val source = Js.string "actorscript"
    val severity = match msg.sev with Diag.Error -> 1 | Diag.Warning -> 2
    val range = range_of_region msg.at
    val message = Js.string msg.text
  end)

let diagnostics_of_msgs (msgs : Diag.message list) =
  Array.of_list (List.map diagnostics_of_msg msgs)

let js_check source =
  let msgs = match
    Pipeline.check_string (Js.to_string source) "js-input" with
    | Error msgs -> msgs
    | Ok (_,  msgs) -> msgs in
  object%js
    val diagnostics = Js.array (diagnostics_of_msgs msgs)
    val code = Js.null
  end

let js_compile_with mode_string source convert =
  let mode =
    match Js.to_string mode_string with
    | "dfinity" -> Pipeline.AncientMode
    | _ -> Pipeline.WasmMode
  in
  match Pipeline.compile_string mode (Js.to_string source) "js-input" with
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

let js_compile_wasm mode s =
  js_compile_with mode s
    (fun m -> let (map, wasm) = CustomModuleEncode.encode m in Js.bytestring wasm, Js.string map)

let () =
  Js.export "ActorScript"
    (object%js
      method check s = js_check s
      method compileWasm mode s = js_compile_wasm mode s
    end);
