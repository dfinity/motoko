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
    (* Find the relevant path for the declaration *)
    let* path =
      match ident with
      | Alias (_, path) -> Some path
      | Unresolved _ -> None
      | Resolved { path; _ } -> Some path
      | Ident _ -> Lib.FilePath.relative_to project_root file_path
    in
    let* module_ = DI.lookup_module project_root path index in
    (* Update `path` from the relevant module *)
    let path, _ = module_ in
    (* Try to find the start/end cursor range in the relevant module *)
    let* decl_range =
      match
        let* decl_ident =
          match ident with
          | Alias _ -> None
          | Unresolved { ident; _ } -> Some ident (* Currently unreachable *)
          | Resolved { ident; _ } -> Some ident
          | Ident ident -> Some ident
        in
        (* Note: ignoring `path` output value from `find_named` *)
        let* _, region = find_named decl_ident module_ in
        Some (range_of_region region)
      with
      | Some range -> Some range
      | None -> (
          match ident with
          | Ident _ -> None (* Unresolved identifiers in the same file *)
          | _ ->
              (* By default, point to the top of the relevant file *)
              (* TODO: use the module declaration start/end positions when possible *)
              let file_start =
                Lsp.{ position_line = 0; position_character = 0 }
              in
              Some Lsp.{ range_start = file_start; range_end_ = file_start })
    in
    Some
      Lsp.
        {
          location_uri =
            (if Source_file.is_non_file_path path then
             Option.get (Source_file.uri_for_package path)
            else Vfs.uri_from_file path);
          location_range = decl_range;
        }
  in
  `TextDocumentDefinitionResponse (Option.to_list location)
