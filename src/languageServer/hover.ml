open As_types
open Declaration_index
module Lsp = Lsp.Lsp_t

let hover_detail = function
  | ValueDecl value ->
     value.name ^ " : " ^ Type.string_of_typ value.typ
  | TypeDecl ty ->
     let _, params, _ = Type.strings_of_kind (Con.kind ty.typ) in
     Printf.sprintf "public type %s%s" ty.name params

let markup_content (msg : string) : Lsp.markup_content =
  Lsp.{ markup_content_kind = "plaintext";
        markup_content_value = msg }

let hover_handler index position file_contents project_root file_path =
  let hover_result =
    Lib.Option.bind
      (Source_file.identifier_at_pos
         project_root
         file_path
         file_contents
         position)
      (function
        | Source_file.Alias (_, path) ->
           Some Lsp.{ hover_result_contents = markup_content path }
        | Source_file.Resolved resolved ->
           Index.find_opt resolved.Source_file.path index
           |> Lib.Fun.flip Lib.Option.bind (fun decls ->
                List.find_opt
                  (fun d -> name_of_ide_decl d = resolved.Source_file.ident)
                  decls)
           |> Lib.Option.map (fun ide_decl ->
                  Lsp.{ hover_result_contents =
                          markup_content (hover_detail ide_decl) })
        | Source_file.Ident _ ->
           (* At some point we'll want to look this Ident up in the
              local context*)
           None
        | Source_file.Unresolved _ ->
           None)
    in
  let result = `TextDocumentHoverResponse hover_result in
  result
