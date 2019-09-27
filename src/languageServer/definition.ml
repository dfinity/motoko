module Lsp = Lsp.Lsp_t
open Declaration_index

let position_of_pos (pos : Source.pos) : Lsp.position = Lsp.
  (* The LSP spec requires zero-based positions *)
  { position_line = if pos.Source.line > 0 then pos.Source.line - 1 else 0;
    position_character = pos.Source.column;
  }

let range_of_region (at : Source.region) : Lsp.range = Lsp.
  { range_start = position_of_pos at.Source.left;
    range_end_ = position_of_pos at.Source.right;
  }

let definition_handler
      index
      position
      file_contents
      project_root
      file_path =
  let result =
    Lib.Option.bind
      (Source_file.identifier_at_pos
         project_root
         file_path
         file_contents
         position)
      (function
       | Source_file.Resolved resolved ->
          Index.find_opt resolved.Source_file.path index
          |> Lib.Fun.flip Lib.Option.bind (fun decls ->
                 decls
                 |> Lib.List.first_opt (function
                     | ValueDecl value ->
                        if String.equal value.name resolved.Source_file.ident
                        then
                          value.definition
                          |> Lib.Option.map (fun def ->
                               (resolved.Source_file.path, def))
                        else None
                     | TypeDecl typ ->
                        if String.equal typ.name resolved.Source_file.ident
                        then
                          typ.definition
                          |> Lib.Option.map (fun def ->
                               (resolved.Source_file.path, def))
                        else None))
       | Source_file.Alias _ -> None
       | Source_file.Unresolved _ -> None
       | Source_file.Ident _ -> None) in
  let location =
    Lib.Option.map (fun (path, region) ->
        Lsp.
        { location_uri = path;
          location_range = range_of_region region
        }) result in
  `TextDocumentDefinitionResponse location
