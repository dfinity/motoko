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

let diagnostics_of_error (at, category, msg) =
  object%js
    val range = range_of_region at
    val severity = 1 (* 1 means error in the LSP spec *)
    val source = Js.string "actorscript"
    val message = Js.string msg
  end


let js_check source =
  match
    Pipeline.check_string Pipeline.initial_stat_env (Js.to_string source)
      "js-input"
  with
  | Ok _ ->
    object%js
      val diagnostics = Js.array [||]
      val code = Js.null
    end
  | Error es ->
    object%js
      val diagnostics =
        Js.array (Array.of_list (List.map diagnostics_of_error es))
      val code = Js.null
    end

let js_compile_with mode_string convert source =
  let mode =
    match Js.to_string mode_string with
    | "dfinity" -> Pipeline.DfinityMode
    | _ -> Pipeline.WasmMode
  in
  match Pipeline.compile_string mode (Js.to_string source) "js-input" with
  | Ok module_ ->
    object%js
      val diagnostics = Js.array [||]
      val code = Js.some (convert module_)
    end
  | Error es ->
    object%js
      val diagnostics =
        Js.array (Array.of_list (List.map diagnostics_of_error es))
      val code = Js.null
    end

let js_compile_wat mode =
  js_compile_with mode
    (fun m -> Js.string (Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ m)))

let js_compile_wasm mode =
  js_compile_with mode (fun m -> Js.bytestring (Wasm.Encode.encode m))

let () =
  Js.export "ActorScript"
    (object%js
      method check s = js_check s
      method compileWat mode s = js_compile_wat mode s
      method compileWasm mode s = js_compile_wasm mode s
    end);
