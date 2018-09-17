let senv, _ = Pipeline.init ()

(* TBR: propagate error messages? *)
let js_compile_with convert source =
  match Pipeline.compile_string (Js.to_string source) senv "js-input" with
  | Some (module_, _) -> Js.some (convert module_)
  | None -> Js.null

let js_compile_wat =
  js_compile_with (fun m -> Js.string (Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ m)))
let js_compile_wasm =
  js_compile_with (fun m -> Js.bytestring (Wasm.Encode.encode m))

let () =
  Js.export "ActorScript"
    (object%js
      method compileWat s = js_compile_wat s
      method compileWasm s = js_compile_wasm s
    end);
