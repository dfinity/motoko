open Source

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

let diagnostics_of_error (msg : Diag.message) =
  Diag.(object%js
    val source = Js.string "actorscript"
    val severity = match msg.sev with Diag.Error -> 1 | Diag.Warning -> 2
    val range = range_of_region msg.at
    val message = Js.string msg.text
  end)


let js_check source =
  let msgs = match
    Pipeline.check_string Pipeline.initial_stat_env (Js.to_string source) "js-input" with
    | Error msgs -> msgs
    | Ok (_,  msgs) -> msgs in
  object%js
    val diagnostics = Js.array (Array.of_list (List.map diagnostics_of_error msgs))
    val code = Js.null
  end

let js_compile_with mode_string source_map source convert =
  Flags.source_map := source_map;
  let mode =
    match Js.to_string mode_string with
    | "dfinity" -> Pipeline.DfinityMode
    | _ -> Pipeline.WasmMode
  in
  match Pipeline.compile_string mode (Js.to_string source) "js-input" with
  | Ok module_ ->
    let (code, map) = convert module_ in
    object%js
      val diagnostics = Js.array [||]
      val code = Js.some code
      val map = Js.some map
    end
  | Error es ->
    object%js
      val diagnostics =
        Js.array (Array.of_list (List.map diagnostics_of_error es))
      val code = Js.null
      val map = Js.null
    end

let js_compile_wasm mode source_map s =
  js_compile_with mode source_map s
    (fun m -> let (map, wasm) = Wasm_copy.CustomModule.encode m in Js.bytestring wasm, Js.string map)

let () =
  Js.export "ActorScript"
    (object%js
      method check s = js_check s
      method compileWasm mode source_map s = js_compile_wasm mode source_map s
    end);
