module Lsp = Lsp.Lsp_t
module DI = Declaration_index

let position_of_pos (pos : Source.pos) : Lsp.position = Lsp.
  (* The LSP spec requires zero-based positions *)
  { position_line = if pos.Source.line > 0 then pos.Source.line - 1 else 0;
    position_character = pos.Source.column;
  }

let range_of_region (at : Source.region) : Lsp.range = Lsp.
  { range_start = position_of_pos at.Source.left;
    range_end_ = position_of_pos at.Source.right;
  }

let find_named (name : string) : DI.ide_decl list -> Source.region option =
  Lib.List.first_opt (function
    | DI.ValueDecl value ->
        if String.equal value.DI.name name
        then value.DI.definition
        else None
    | DI.TypeDecl typ ->
        if String.equal typ.DI.name name
        then typ.DI.definition
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
    let open Source_file in
    Lib.Option.bind
      (Source_file.identifier_at_pos
         project_root
         file_path
         file_contents
         position)
      (function
       | Alias _ -> None
       | Unresolved _ -> None
       | Resolved resolved ->
          DI.lookup_module resolved.path index
          |> opt_bind (find_named resolved.ident)
          |> Lib.Option.map (fun loc -> (resolved.path, loc))
       | Ident ident ->
          Pipeline__.File_path.relative_to project_root file_path
          |> opt_bind (fun uri ->
              DI.lookup_module uri index
              |> opt_bind (find_named ident)
              |> Lib.Option.map (fun loc -> (uri, loc))
      )) in
  let location =
    Lib.Option.map (fun (path, region) ->
        Lsp.
        { location_uri =
            if Source_file.is_package_path path
            then Lib.Option.value (Source_file.uri_for_package path)
            else Vfs.uri_from_file path;
          location_range = range_of_region region
        }) result in
  `TextDocumentDefinitionResponse location
