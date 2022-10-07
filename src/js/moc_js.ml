open Mo_config
open Common

module Js = Js_of_ocaml.Js
module Sys_js = Js_of_ocaml.Sys_js

let () =
  Sys_js.set_channel_flusher stdout (Buffer.add_string stdout_buffer);
  Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buffer);
  Flags.check_ir := false;
  Flags.debug_info := false;
  Flags.actor_idl_path := Some "idl/";
  Js.export "Motoko"
    (object%js
      val version = js_version
      method saveFile name content = js_save_file name content
      method removeFile name = js_remove_file name
      method renameFile oldpath newpath = js_rename_file oldpath newpath
      method readFile name = js_read_file name
      method readDir path = js_read_dir path
      method addPackage package dir = add_package package dir
      method clearPackage () = clear_package ()
      method setActorAliases entries = set_actor_aliases entries
      method setPublicMetadata entries = set_public_metadata entries
      method gcFlags option = gc_flags option
      method run list s = Flags.compiled := false; wrap_output (fun _ -> js_run list s)
      method check s = Flags.compiled := false; js_check s
      method candid s = Flags.compiled := true; js_candid s
      method stableCompatible pre post = js_stable_compatible pre post
      method compileWasm mode s = Flags.compiled := true; js_compile_wasm mode s
      method parseCandid s = js_parse_candid s
      method parseMotoko s = js_parse_motoko s
      method parseMotokoTyped paths = js_parse_motoko_typed paths
     end);
