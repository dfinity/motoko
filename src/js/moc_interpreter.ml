open Mo_config
open Common

module Js = Js_of_ocaml.Js
module Sys_js = Js_of_ocaml.Sys_js

let () =
  Sys_js.set_channel_flusher stdout (Buffer.add_string stdout_buffer);
  Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buffer);
  Flags.check_ir := false;
  Flags.debug_info := false;
  Flags.compiled := false;
  Flags.actor_idl_path := Some "idl/";
  Js.export "Motoko"
    (object%js
      val version = js_version
      method saveFile name content = js_save_file name content
      method addPackage package dir = add_package package dir
      method clearPackage () = clear_package ()
      method setActorAliases entries = set_actor_aliases entries
      method run list s = wrap_output (fun _ -> js_run list s)
     end);
