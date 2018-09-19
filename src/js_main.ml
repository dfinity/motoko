(* TBR: propagate error messages? *)
let js_compile_with dfinity_mode convert source =
  match Pipeline.compile_string dfinity_mode (Js.to_string source) "js-input" with
  | Some module_ -> Js.some (convert module_)
  | None -> Js.null

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
