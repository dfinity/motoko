module Lsp = Lsp.Lsp_t
module DI = Declaration_index

let position_of_pos (pos : Source.pos) : Lsp.position =
  Lsp.
    {
      (* The LSP spec requires zero-based positions *)
      position_line = (if pos.Source.line > 0 then pos.Source.line - 1 else 0);
      position_character = pos.Source.column;
    }

let range_of_region (at : Source.region) : Lsp.range =
  Lsp.
    {
      range_start = position_of_pos at.Source.left;
      range_end_ = position_of_pos at.Source.right;
    }

let find_named :
    string -> string * DI.ide_decl list -> (string * Source.region) option =
 fun name (path, decls) ->
  List.find_map
    (function
      | DI.ValueDecl value ->
          if String.equal value.DI.name name then value.DI.definition else None
      | DI.TypeDecl typ ->
          if String.equal typ.DI.name name then typ.DI.definition else None)
    decls
  |> Option.map (fun x -> (path, x))

let definition_handler index position file_contents project_root file_path =
  let open Source_file in
  let location =
    let open Lib.Option.Syntax in
    let* ident =
      Source_file.identifier_at_pos project_root file_path file_contents
        position
    in
    let* path, region =
      match ident with
      | Alias _ -> None
      | Unresolved _ -> None
      | Resolved resolved ->
          let* module_ = DI.lookup_module project_root resolved.path index in
          find_named resolved.ident module_
      | Ident ident ->
          let* uri = Lib.FilePath.relative_to project_root file_path in
          let* module_ = DI.lookup_module project_root uri index in
          find_named ident module_
    in
    Some
      Lsp.
        {
          location_uri =
            (if Source_file.is_non_file_path path then
             Option.get (Source_file.uri_for_package path)
            else Vfs.uri_from_file path);
          location_range = range_of_region region;
        }
  in
  `TextDocumentDefinitionResponse (Option.to_list location)
