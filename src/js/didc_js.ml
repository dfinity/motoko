open Idllib

module Js = Js_of_ocaml.Js

let js_of_did source : string =
  match Pipeline.(compile_js_string (Js.to_string source)) with
  | Stdlib.Error msgs -> "TODO"
  | Stdlib.Ok (b,_) -> Buffer.contents b

let () =
  Js.export "Didc"
    (object%js
      method jsOfDid s = Js.string (js_of_did s)
    end);
