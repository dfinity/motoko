open Source

let position_of_pos pos = object%js
  (* The LSP spec requires zero-based positions *)
  val line = if pos.line > 0 then pos.line - 1 else 0
  val character = pos.column
  end

let range_of_region at = object%js
  val start = position_of_pos at.left
  val _end = position_of_pos at.right
  end

let diagnostics_of_error (at, category, msg) = object%js
  val range = range_of_region at
  val source = Js.string "actorscript"
  val message = Js.string msg
  end


let js_compile_with dfinity_mode convert source =
  let mode = if dfinity_mode then Pipeline.DfinityMode else Pipeline.WasmMode in
  match Pipeline.compile_string mode (Js.to_string source) "js-input" with
  | Ok module_ -> object%js
      val diagnostics = Js.array (Array.of_list [ ])
      val code = Js.some (convert module_)
    end
  | Error e -> object%js
      val diagnostics = Js.array (Array.of_list [ diagnostics_of_error e ])
      val code = Js.null
    end

let js_compile_wat dfinity_mode =
  js_compile_with dfinity_mode (fun m -> Js.string (Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ m)))
let js_compile_wasm dfinity_mode =
  js_compile_with dfinity_mode (fun m -> Js.bytestring (Wasm.Encode.encode m))

let () =
  Js.export "ActorScript"
    (object%js
      method compileWat dfinity_mode s = js_compile_wat dfinity_mode s
      method compileWasm dfinity_mode s = js_compile_wasm dfinity_mode s
    end);
