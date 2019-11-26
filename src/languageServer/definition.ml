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

let find_named (name : string) : ide_decl list -> Source.region option =
  Lib.List.first_opt (function
    | ValueDecl value ->
        if String.equal value.name name
        then value.definition
        else None
    | TypeDecl typ ->
        if String.equal typ.name name
        then typ.definition
        else None)

let opt_bind f = function
  | Some x -> f x
  | None -> None

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
       | Source_file.Alias _ -> None
       | Source_file.Unresolved _ -> None
       | Source_file.Resolved resolved ->
          lookup_module resolved.Source_file.path index
          |> opt_bind (find_named resolved.Source_file.ident)
          |> Lib.Option.map (fun loc -> (resolved.Source_file.path, loc))
       | Source_file.Ident ident ->
          Pipeline__.File_path.relative_to project_root file_path
          |> opt_bind (fun uri ->
              lookup_module uri index
              |> opt_bind (find_named ident)
              |> Lib.Option.map (fun loc -> (uri, loc))
      )) in
  let location =
    Lib.Option.map (fun (path, region) ->
        Lsp.
        { location_uri = Vfs.uri_from_file path;
          location_range = range_of_region region
        }) result in
  `TextDocumentDefinitionResponse location
