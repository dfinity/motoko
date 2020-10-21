open Wasm_exts
open Source
open Mo_config

module Js = Js_of_ocaml.Js
module Sys_js = Js_of_ocaml.Sys_js          

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
    val source = Js.string msg.at.left.file
    val severity = match msg.sev with Diag.Error -> 1 | (Diag.Warning | Diag.Info)  -> 2
    val range = range_of_region msg.at
    val message = Js.string msg.text
  end)

let diagnostics_of_msgs (msgs : Diag.message list) =
  Array.of_list (List.map diagnostics_of_msg msgs)

let js_check source =
  let msgs = match
    Pipeline.check_files [Js.to_string source] with
    | Error msgs -> msgs
    | Ok (_,  msgs) -> msgs in
  object%js
    val diagnostics = Js.array (diagnostics_of_msgs msgs)
    val code = Js.null
  end

let js_run source =
  let _ = Flags.compiled := false in
  let results = Pipeline.run_string (Js.to_string source) in
  let result = String.concat "\n" results in
  let _ = Flags.compiled := true in
  Js.string result

let js_candid source =
  let prog = Diag.run (Pipeline.generate_idl [Js.to_string source]) in
  let code = Idllib.Arrange_idl.string_of_prog prog in
  Js.string code

let js_compile_with mode_string source convert =
  let mode =
    match Js.to_string mode_string with
    | "wasi" -> Flags.WASIMode
    | "dfinity" -> Flags.ICMode
    | _ -> raise (Invalid_argument "js_compile_with: Unexpected mode")
  in
  match Pipeline.compile_files mode true [Js.to_string source] with
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
      let constructor = Js.Unsafe.global##._Uint8Array in
      constructor##from
        (object%js val length = String.length wasm end)
        (Js.wrap_callback (fun _v k -> Char.code wasm.[k]))
    )

let js_save_file filename content =
  let filename = Js.to_string filename in
  let content = Js.to_string content in
  try Sys_js.create_file ~name:filename ~content:content
  with _ -> Sys_js.update_file ~name:filename ~content:content

let stdout_buffer = Buffer.create(100)
let stderr_buffer = Buffer.create(100)

let wrap_output f =
  let result = f() in
  let stdout_result = Buffer.contents stdout_buffer in
  let stderr_result = Buffer.contents stderr_buffer in
  Buffer.clear stdout_buffer;
  Buffer.clear stderr_buffer;
  object%js
    val stdout = Js.bytestring stdout_result
    val stderr = Js.bytestring stderr_result
    val result = result
  end

let () =
  Sys_js.set_channel_flusher stdout (Buffer.add_string stdout_buffer);
  Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buffer);
  Flags.check_ir := false;
  Flags.debug_info := false;
  Flags.compiled := true;
  Flags.actor_idl_path := Some "idl/";
  Js.export "Motoko"
    (object%js
      method saveFile name content = js_save_file name content
      method loadFile name = Sys_js.read_file ~name:(Js.to_string name)
      method addPackage package dir =
        let libs = Flags.package_urls in
        libs := Flags.M.add (Js.to_string package) (Js.to_string dir) !libs
      method setActorAliases entries =
        let entries = Array.map (fun kv ->
                          let kv = Js.to_array kv in
                          Js.to_string (Array.get kv 0), Js.to_string (Array.get kv 1)) (Js.to_array entries) in
        let aliases = Flags.actor_aliases in
        aliases := Flags.M.of_seq (Array.to_seq entries)
      method check s = wrap_output (fun _ -> js_check s)
      method candid s = wrap_output (fun _ -> js_candid s)
      method compileWasm mode s = wrap_output (fun _ -> js_compile_wasm mode s)
      method run s = wrap_output (fun _ -> js_run s)
    end);
