open Ocamlbuild_plugin

let ocamlfind_query pkg =
  let cmd = Printf.sprintf "ocamlfind query %s" (Filename.quote pkg) in
  Ocamlbuild_pack.My_unix.run_and_open cmd (fun ic ->
      Printf.printf "Getting ocaml directory from command %s\n" cmd;
      input_line ic
    )

let () = dispatch begin function
                    | After_rules ->
                        ocaml_lib ~extern:true ~dir:(ocamlfind_query "js_of_ocaml-compiler") "js_of_ocaml_compiler";
                    | _ -> ()
           end
