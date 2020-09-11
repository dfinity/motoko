open Wasm_exts
open Source
open Mo_config

module Js = Js_of_ocaml.Js
module Dom_html = Js_of_ocaml.Dom_html          

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
  
let js_compile_with mode_string source convert =
  let mode =
    match Js.to_string mode_string with
    | "wasi" -> Flags.WASIMode
    | "dfinity" -> Flags.ICMode
    | _ -> raise (Invalid_argument "js_compile_with: Unexpected mode")
  in
  match Pipeline.compile_string mode (Js.to_string source) Filename.current_dir_name with
  | Ok (module_, msgs) ->
    let code = convert module_ in
    object%js
      val diagnostics = Js.array (diagnostics_of_msgs msgs)
      val code = Js.some code
    end
  | Error msgs ->
    object%js
      val diagnostics = Js.array (diagnostics_of_msgs msgs)
      val code = Js.null
    end

let js_compile_wasm mode s =
  js_compile_with mode s
    (fun m ->
      let (_, wasm) = CustomModuleEncode.encode m in
      let len = String.length wasm in
      Js_of_ocaml.Typed_array.Bigstring.to_arrayBuffer (Bigstringaf.of_string ~off:0 ~len:len wasm)
    )

let redirect_channel channel id =
  Js_of_ocaml.Sys_js.set_channel_flusher channel
    (fun s ->
      match Dom_html.getElementById_coerce id Dom_html.CoerceTo.textarea with
      | None -> Js_of_ocaml.Firebug.console##log s;
      | Some output -> output##.value := Js.string (Js.to_string (output##.value) ^ s);
    )

let () =
  redirect_channel stdout "output";
  redirect_channel stderr "output";  
  Js.export "Motoko"
    (object%js
      method check s = js_check s
      method compileWasm mode s = js_compile_wasm mode s
      method run s = js_run s
    end);
