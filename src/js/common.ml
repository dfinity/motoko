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

let js_result (result : 'a Diag.result) (wrap_code: 'a -> 'b) =
  match result with
  | Ok (code, msgs) ->
     object%js
       val diagnostics = Js.array (diagnostics_of_msgs msgs)
       val code = wrap_code code
     end
  | Error msgs ->
     object%js
       val diagnostics = Js.array (diagnostics_of_msgs msgs)
       val code = Js.null
     end

let js_check source =
  js_result (Pipeline.check_files [Js.to_string source]) (fun _ -> Js.null)

let js_run list source =
  let list = Array.to_list (Js.to_array list) in
  let list = List.map Js.to_string list in
  ignore (Pipeline.run_stdin_from_file list (Js.to_string source))

let js_candid source =
  js_result (Pipeline.generate_idl [Js.to_string source])
    (fun prog ->
      let code = Idllib.Arrange_idl.string_of_prog prog in
      Js.some (Js.string code)
    )

let js_compile_wasm mode source =
  let mode =
    match Js.to_string mode with
    | "wasi" -> Flags.WASIMode
    | "ic" -> Flags.ICMode
    | _ -> raise (Invalid_argument "js_compile_with: Unexpected mode")
  in
  js_result (Pipeline.compile_files mode true [Js.to_string source])
    (fun m ->
      let (_, wasm) = CustomModuleEncode.encode m in
      let constructor = Js.Unsafe.global##._Uint8Array in
      let code = constructor##from
        (object%js val length = String.length wasm end)
        (Js.wrap_callback (fun _v k -> Char.code wasm.[k])) in
      Js.some code)

let js_save_file filename content =
  let filename = Js.to_string filename in
  let content = Js.to_string content in
  try Sys_js.create_file ~name:filename ~content:content
  with _ -> Sys_js.update_file ~name:filename ~content:content  

let js_remove_file filename = Sys.remove (Js.to_string filename)
let js_rename_file oldpath newpath = Sys.rename (Js.to_string oldpath) (Js.to_string newpath)
let js_read_dir path = Sys.readdir (Js.to_string path)

let stdout_buffer = Buffer.create(100)
let stderr_buffer = Buffer.create(100)

let wrap_output f =
  let result = f () in
  let stdout_result = Buffer.contents stdout_buffer in
  let stderr_result = Buffer.contents stderr_buffer in
  Buffer.clear stdout_buffer;
  Buffer.clear stderr_buffer;
  object%js
    val stdout = Js.bytestring stdout_result
    val stderr = Js.bytestring stderr_result
    val result = result
  end

let add_package package dir =
  let libs = Flags.package_urls in
  libs := Flags.M.add (Js.to_string package) (Js.to_string dir) !libs
let clear_package () = Flags.package_urls := Flags.M.empty
let set_actor_aliases entries =
  let entries = Array.map (fun kv ->
                    let kv = Js.to_array kv in
                    Js.to_string (Array.get kv 0), Js.to_string (Array.get kv 1)) (Js.to_array entries) in
  let aliases = Flags.actor_aliases in
  aliases := Flags.M.of_seq (Array.to_seq entries)  

