open Ocamlbuild_plugin
let () =
  dispatch Bisect_ppx_plugin.dispatch;
  dispatch Ocamlbuild_atdgen.dispatcher
