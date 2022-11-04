open Mo_types
open Mo_config
open Mo_def
open Source
open Syntax

type value_decl = {
  name : string;
  typ : Type.typ;
  definition : region option;
  doc_comment : string option;
}

type type_decl = {
  name : string;
  typ : Type.con;
  definition : region option;
  doc_comment : string option;
}

type ide_decl = ValueDecl of value_decl | TypeDecl of type_decl

let string_of_option f = function None -> "None" | Some x -> "Some " ^ f x

let string_of_list f xs =
  List.map f xs |> String.concat "; " |> fun x -> "[ " ^ x ^ " ]"

(** For debugging purposes *)
let string_of_ide_decl = function
  | ValueDecl value ->
      "ValueDecl{ name = "
      ^ String.escaped value.name
      ^ ", typ = "
      ^ Type.string_of_typ value.typ
      ^ Lib.Option.get
          (Option.map
             (fun pos -> ", definition = " ^ string_of_region pos)
             value.definition)
          ""
      ^ " }"
  | TypeDecl ty ->
      "TypeDecl{ name = "
      ^ String.escaped ty.name
      ^ ", typ = "
      ^ Type.string_of_con ty.typ
      ^ Lib.Option.get
          (Option.map
             (fun pos -> ", definition = " ^ string_of_region pos)
             ty.definition)
          ""
      ^ " }"

let name_of_ide_decl (d : ide_decl) : string =
  match d with ValueDecl value -> value.name | TypeDecl ty -> ty.name

let decl_has_prefix prefix d =
  Option.is_some (Lib.String.chop_prefix prefix (name_of_ide_decl d))

module Index = Map.Make (String)

type declaration_index = {
  modules : ide_decl list Index.t;
  actors : ide_decl list Index.t;
  package_map : Pipeline.ResolveImport.package_map;
  ic_aliases : Pipeline.ResolveImport.aliases;
  actor_idl_path : Pipeline.ResolveImport.actor_idl_path;
}

type t = declaration_index

let add_module :
    string -> ide_decl list -> declaration_index -> declaration_index =
 fun path decls ix -> { ix with modules = Index.add path decls ix.modules }

let ic_id_to_lookup : string option -> string -> string -> string =
 fun actor_idl_path project_root id ->
  let crc = Lib.CRC.crc8 id |> Lib.Hex.hex_of_byte in
  let hex = Lib.Hex.hex_of_bytes id ^ crc in
  let idl_path = actor_idl_path |> Option.value ~default:project_root in
  Lib.FilePath.make_absolute project_root
    (Filename.concat idl_path (hex ^ ".did"))

let lookup_module (project_root : string) (path : string) (index : t) :
    (string * ide_decl list) option =
  let open Ic.Url in
  let open Lib.Option.Syntax in
  let make_absolute = Lib.FilePath.make_absolute project_root in
  match parse path with
  | Ok (Relative path) ->
      let path = Pipeline.ResolveImport.append_extension Sys.file_exists path in
      let+ decls = Index.find_opt (make_absolute path) index.modules in
      (path, decls)
  | Ok (Package (pkg, path)) ->
      let* pkg_path = Flags.M.find_opt pkg index.package_map in
      let path =
        Pipeline.ResolveImport.append_extension Sys.file_exists
          (Filename.concat pkg_path path)
      in
      let+ decls = Index.find_opt (make_absolute path) index.modules in
      (path, decls)
  | Ok Prim ->
      let+ decls = Index.find_opt "@prim" index.modules in
      ("@prim", decls)
  | Ok (Ic id) ->
      let lookup_path = ic_id_to_lookup index.actor_idl_path project_root id in
      let+ decls = Index.find_opt lookup_path index.modules in
      (path, decls)
  | Ok (IcAlias alias) ->
      let mic_id = Flags.M.find_opt alias index.ic_aliases in
      let _ =
        Debug.log "lookup_module.ic_alias.ic_id"
          (string_of_option (fun x -> x) mic_id)
      in
      let* id = mic_id in
      let lookup_path = ic_id_to_lookup index.actor_idl_path project_root id in
      let+ decls = Index.find_opt lookup_path index.modules in
      (alias, decls)
  | Error _ -> None

let rec drop_common_prefix eq l1 l2 =
  match (l1, l2) with
  | h1 :: t1, h2 :: t2 when eq h1 h2 -> drop_common_prefix eq t1 t2
  | _ -> (l1, l2)

let shorten_import_path :
    Pipeline.ResolveImport.package_map ->
    Pipeline.ResolveImport.aliases ->
    string ->
    string ->
    string =
 fun pkg_map ic_aliases base path ->
  let _ =
    Debug.log "shorten_import" (Printf.sprintf "base: %s, path: %s" base path)
  in
  if Filename.extension path = ".did" then
    let idl_basename = Filename.basename path in
    Flags.M.bindings ic_aliases
    |> List.find_map (fun (alias, id) ->
           Debug.log "basename" (Ic.Url.idl_basename_of_blob id);
           if Ic.Url.idl_basename_of_blob id = idl_basename then Some alias
           else None)
    |> function
    | None -> Printf.sprintf "ic:%s" (Filename.remove_extension idl_basename)
    | Some alias -> Printf.sprintf "canister:%s" alias
  else
    let pkg_path =
      Flags.M.bindings pkg_map
      |> List.find_map (fun (name, pkg_path) ->
             if Lib.FilePath.is_subpath pkg_path path then
               let rel_path =
                 Option.get (Lib.FilePath.relative_to pkg_path path)
               in
               Some
                 (Printf.sprintf "mo:%s/%s" name
                    (Filename.remove_extension rel_path))
             else None)
    in
    match pkg_path with
    | Some p -> p
    | None ->
        let base', path' =
          drop_common_prefix String.equal
            (Lib.String.split (Filename.dirname base) '/')
            (Lib.String.split path '/')
        in
        List.map (fun _ -> "..") base' @ path'
        |> String.concat "/"
        |> Filename.remove_extension

let find_with_prefix : string -> string -> t -> (string * ide_decl list) list =
 fun prefix base { modules; package_map; ic_aliases; _ } ->
  Index.bindings modules
  |> List.map (fun (p, ds) ->
         let import_path =
           if p = "@prim" then "mo:⛔"
           else shorten_import_path package_map ic_aliases base p
         in
         (import_path, List.filter (decl_has_prefix prefix) ds))
  |> List.filter (fun (_, ds) -> not (ds = []))

let empty : string -> t =
 fun cwd ->
  let open Pipeline.ResolveImport in
  let resolved_flags =
    Diag.run
      (resolve_flags
         {
           package_urls = !Flags.package_urls;
           actor_aliases = !Flags.actor_aliases;
           actor_idl_path = !Flags.actor_idl_path;
         })
  in
  {
    modules = Index.empty;
    actors = Index.empty;
    package_map =
      Flags.M.map (Lib.FilePath.make_absolute cwd) resolved_flags.packages;
    ic_aliases = resolved_flags.aliases;
    actor_idl_path = resolved_flags.actor_idl_path;
  }

module PatternMap = Map.Make (String)

type pattern_map = Source.region PatternMap.t

let rec gather_pat ve pat : pattern_map =
  match pat.it with
  | WildP | LitP _ | SignP _ -> ve
  | VarP id -> PatternMap.add id.it id.at ve
  | TupP pats -> List.fold_left gather_pat ve pats
  | ObjP pfs -> List.fold_left gather_pat_field ve pfs
  | TagP (_, pat1) | AltP (pat1, _) | OptP pat1 | AnnotP (pat1, _) | ParP pat1
    ->
      gather_pat ve pat1

and gather_pat_field ve pf = gather_pat ve pf.it.pat

let string_of_index : declaration_index -> string =
 fun index ->
  Index.bindings index.modules
  |> string_of_list (fun (path, decls) ->
         path ^ " =>\n    " ^ string_of_list string_of_ide_decl decls ^ "\n")

let read_single_module_lib (ty : Type.typ) : ide_decl list option =
  match ty with
  | Type.Obj ((Type.Module | Type.Actor), fields) ->
      fields
      |> List.map (fun Type.{ lab = name; typ; _ } ->
             match typ with
             | Type.Typ con ->
                 TypeDecl
                   { name; typ = con; definition = None; doc_comment = None }
             | typ ->
                 ValueDecl { name; typ; definition = None; doc_comment = None })
      |> Option.some
  | _ -> None

let unwrap_module_ast (lib : Syntax.lib) : Syntax.dec_field list option =
  match lib.it.body with
  | { it = Syntax.ModuleU (_, fields); _ } -> Some fields
  | _ -> None

let populate_definitions (project_root : string) (libs : Syntax.lib list)
    (path : string) (decls : ide_decl list) : ide_decl list =
  let is_let_bound dec_field =
    match dec_field.it.Syntax.dec.it with
    | Syntax.LetD (pat, _) -> Some pat
    | _ -> None
  in
  let is_type_def dec_field =
    match dec_field.it.Syntax.dec.it with
    | Syntax.TypD (typ_id, _, _) -> Some typ_id
    | Syntax.ClassD (_, typ_id, _, _, _, _, _, _) -> Some typ_id
    | _ -> None
  in
  let extract_binders env (pat : Syntax.pat) = gather_pat env pat in
  let find_def lib def =
    let find_doc_comment (parser_pos : Source.region) : string option =
      Trivia.PosHashtbl.find_opt lib.note.trivia
        Trivia.{ line = parser_pos.left.line; column = parser_pos.left.column }
      |> Option.get
      |> Trivia.doc_comment_of_trivia_info
    in
    match def with
    | ValueDecl value -> (
        let fields = Lib.Option.get (unwrap_module_ast lib) [] in
        let find_binder field =
          let open Lib.Option.Syntax in
          let* pat = is_let_bound field in
          let* binder_definition =
            extract_binders PatternMap.empty pat
            |> PatternMap.find_opt value.name
          in
          Some (binder_definition, find_doc_comment field.at)
        in
        match List.find_map find_binder fields with
        | None -> ValueDecl { value with definition = None }
        | Some (definition, doc_comment) ->
            ValueDecl { value with definition = Some definition; doc_comment })
    | TypeDecl typ -> (
        let fields = Lib.Option.get (unwrap_module_ast lib) [] in
        let find_type_binder field =
          let open Lib.Option.Syntax in
          let* ty_id = is_type_def field in
          if ty_id.it = typ.name then Some (ty_id.at, find_doc_comment field.at)
          else None
        in
        match List.find_map find_type_binder fields with
        | None -> TypeDecl typ
        | Some (type_definition, doc_comment) ->
            TypeDecl { typ with definition = Some type_definition; doc_comment }
        )
  in
  let opt_lib =
    List.find_opt
      (fun lib ->
        String.equal path
          (Lib.FilePath.make_absolute project_root lib.note.filename))
      libs
  in
  match opt_lib with None -> decls | Some lib -> List.map (find_def lib) decls

let list_files_recursively dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f
        |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs
        |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  loop [] [ dir ]

let scan_packages : unit -> string list =
 fun _ ->
  let scan_package path =
    list_files_recursively path
    |> List.filter (fun f -> Filename.extension f = ".mo")
  in
  Flags.M.fold (fun _ v acc -> scan_package v @ acc) !Flags.package_urls []

let scan_actors : unit -> string list =
 fun _ ->
  match !Flags.actor_idl_path with
  | None -> []
  | Some idl_path ->
      list_files_recursively idl_path
      |> List.filter (fun f -> Filename.extension f = ".did")

let index_from_scope : string -> t -> Syntax.lib list -> Scope.t -> t =
 fun project_root initial_index libs scope ->
  Type.Env.fold
    (fun path ty acc ->
      let path =
        if path = "@prim" then path
        else Lib.FilePath.make_absolute project_root path
      in
      add_module path
        (ty
        |> read_single_module_lib
        |> Fun.flip Lib.Option.get []
        |> populate_definitions project_root libs path)
        acc)
    scope.Scope.lib_env initial_index

let make_index_inner project_root vfs entry_points : t Diag.result =
  let package_paths =
    List.map (fun f -> LibPath f @@ Source.no_region) (scan_packages ())
  in
  let package_env =
    Pipeline.chase_imports
      (fun _ -> Vfs.parse_file vfs)
      Pipeline.initial_stat_env package_paths
  in
  let lib_index =
    match package_env with
    | Result.Error errs ->
        List.iter
          (fun err -> Debug.log "lib_index_errors" (Diag.string_of_message err))
          errs;
        empty project_root
    | Result.Ok ((libs, scope), _) ->
        index_from_scope project_root (empty project_root) libs scope
  in
  let actor_paths = scan_actors () in
  let _ = Debug.log "Actor paths" (string_of_list (fun x -> x) actor_paths) in
  let actor_env =
    List.fold_left
      (fun acc f ->
        Idllib.Pipeline.check_file f |> function
        | Result.Error errs ->
            List.iter
              (fun err ->
                Debug.log "actor_index_errors" (Diag.string_of_message err))
              errs;
            acc
        | Result.Ok ((prog, idl_scope, actor_opt), _) ->
            let actor = Mo_idl.Idl_to_mo.check_prog idl_scope actor_opt in
            Scope.adjoin acc (Scope.lib f actor))
      Pipeline.initial_stat_env actor_paths
  in
  let actor_index = index_from_scope project_root lib_index [] actor_env in
  (* TODO(Christoph): We should be able to return at least the
     actor_index even if compiling from the entry points fails *)
  Pipeline.load_progs
    (fun _ -> Vfs.parse_file vfs)
    entry_points Pipeline.initial_stat_env
  |> Diag.map (fun (libs, _, scope) ->
         index_from_scope project_root actor_index libs scope)

let make_index project_root vfs entry_points : t Diag.result =
  (* TODO(Christoph): Actually handle errors here *)
  try make_index_inner project_root vfs entry_points
  with _ -> Diag.return (empty project_root)
