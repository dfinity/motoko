open Mo_types
open Declaration_index
module Lsp = Lsp.Lsp_t
module DI = Declaration_index

let hover_detail = function
  | ValueDecl value ->
      let doc_comment =
        match value.doc_comment with None -> "" | Some c -> "\n\n---\n\n" ^ c
      in
      Printf.sprintf "```motoko\n%s : %s\n```%s" value.name
        (Pretty.string_of_typ value.typ)
        doc_comment
  | TypeDecl ty ->
      let doc_comment =
        match ty.doc_comment with None -> "" | Some c -> "\n\n---\n\n" ^ c
      in
      let _, params, _ = Type.strings_of_kind (Cons.kind ty.typ) in
      Printf.sprintf "```motoko\ntype %s%s\n```%s" ty.name params doc_comment

let markup_content (msg : string) : Lsp.markup_content =
  Lsp.{ markup_content_kind = "markdown"; markup_content_value = msg }

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
    | Source_file.Symbol (_, path) ->
        (* TODO: add symbol-specific information *)
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
