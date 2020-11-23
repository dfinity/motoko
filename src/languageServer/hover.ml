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
  let open Lib.Option.Syntax in
  let toplevel_decls =
    Option.fold ~none:[] ~some:snd
      (let* uri = Lib.FilePath.relative_to project_root file_path in
       lookup_module project_root uri index)
  in
  let mk_hover_result ide_decl =
    Lsp.{ hover_result_contents = markup_content (hover_detail ide_decl) }
  in
  let hover_result =
    let* ident_target =
      Source_file.identifier_at_pos project_root file_path file_contents
        position
    in
    match ident_target with
    | Source_file.Alias (_, path) ->
        Some Lsp.{ hover_result_contents = markup_content path }
    | Source_file.Resolved resolved ->
        let* _, decls =
          lookup_module project_root resolved.Source_file.path index
        in
        let+ decl =
          List.find_opt
            (fun d -> name_of_ide_decl d = resolved.Source_file.ident)
            decls
        in
        mk_hover_result decl
    | Source_file.Ident ident ->
        let+ decl =
          List.find_opt (fun d -> name_of_ide_decl d = ident) toplevel_decls
        in
        mk_hover_result decl
    | Source_file.Unresolved _ -> None
  in
  let result = `TextDocumentHoverResponse hover_result in
  result
