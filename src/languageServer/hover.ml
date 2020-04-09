open Mo_types
open Declaration_index
module Lsp = Lsp.Lsp_t
module DI = Declaration_index

let hover_detail = function
  | ValueDecl value -> value.name ^ " : " ^ Pretty.string_of_typ value.typ
  | TypeDecl ty ->
      let _, params, _ = Type.strings_of_kind (Con.kind ty.typ) in
      Printf.sprintf "public type %s%s" ty.name params

let markup_content (msg : string) : Lsp.markup_content =
  Lsp.{ markup_content_kind = "plaintext"; markup_content_value = msg }

let hover_handler (index : DI.t) (position : Lsp.position)
    (file_contents : string) (project_root : string) (file_path : string) =
  let current_uri_opt = Lib.FilePath.relative_to project_root file_path in
  let toplevel_decls =
    current_uri_opt
    |> Fun.flip Option.bind (fun uri -> lookup_module project_root uri index)
    |> Option.fold ~none:[] ~some:snd
  in
  let mk_hover_result ide_decl =
    Lsp.{ hover_result_contents = markup_content (hover_detail ide_decl) }
  in
  let hover_result =
    Option.bind
      (Source_file.identifier_at_pos project_root file_path file_contents
         position) (function
      | Source_file.Alias (_, path) ->
          Some Lsp.{ hover_result_contents = markup_content path }
      | Source_file.Resolved resolved ->
          lookup_module project_root resolved.Source_file.path index
          |> Fun.flip Option.bind (fun (_, decls) ->
                 List.find_opt
                   (fun d -> name_of_ide_decl d = resolved.Source_file.ident)
                   decls)
          |> Option.map mk_hover_result
      | Source_file.Ident ident ->
          toplevel_decls
          |> List.find_opt (fun d -> name_of_ide_decl d = ident)
          |> Option.map mk_hover_result
      | Source_file.Unresolved _ -> None)
  in
  let result = `TextDocumentHoverResponse hover_result in
  result
