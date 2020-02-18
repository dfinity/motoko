open Mo_types
open Mo_config
open Mo_def
open Source
open Syntax

type value_decl = {
    name : string;
    typ: Type.typ;
    definition: region option;
  }

type type_decl = {
    name : string;
    typ: Type.con;
    definition: region option;
  }

type ide_decl =
  | ValueDecl of value_decl
  | TypeDecl of type_decl

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
  match d with
  | ValueDecl value -> value.name
  | TypeDecl ty -> ty.name

let decl_has_prefix prefix d =
  Option.is_some (Lib.String.chop_prefix prefix (name_of_ide_decl d))

module Index = Map.Make(String)

type declaration_index = {
    modules: ide_decl list Index.t;
    actors: ide_decl list Index.t;
    package_map: Pipeline.ResolveImport.package_map;
    (* TODO: Add the mapping for IC urls here  *)
  }
type t = declaration_index
let add_module : string -> ide_decl list -> declaration_index -> declaration_index =
  fun path decls ix -> { ix with modules = Index.add path decls ix.modules}

let lookup_module
      (path : string)
      (index : t)
    : (string * ide_decl list) option =
  let open Pipeline.URL in
  let make_absolute = Lib.FilePath.make_absolute (Sys.getcwd ()) in
  match parse path with
  | Ok (Relative path) ->
     let path =
       Pipeline.ResolveImport.append_extension Sys.file_exists path in
     Index.find_opt (make_absolute path) index.modules
     |> Option.map (fun decls -> (path, decls))
  | Ok (Package (pkg, path)) ->
     Option.bind
       (Flags.M.find_opt pkg index.package_map)
       (fun pkg_path ->
         let path =
           Pipeline.ResolveImport.append_extension
             Sys.file_exists (Filename.concat pkg_path path) in
         Index.find_opt
           (make_absolute path)
           index.modules
         |> Option.map (fun decls -> (path, decls))
       )
  | Ok Prim ->
     Index.find_opt "@prim" index.modules
     |> Option.map (fun decls -> ("@prim", decls))
  | Ok (Ic _ | IcAlias _) -> (* TODO *) None
  | Error _ -> None

let rec drop_common_prefix eq l1 l2 =
  match l1, l2 with
  | h1 :: t1, h2 :: t2 when eq h1 h2 ->
      drop_common_prefix eq t1 t2
  | _ -> (l1, l2)

let shorten_import_path
    : Pipeline.ResolveImport.package_map
   -> string
   -> string
   -> string =
  fun pkg_map base path ->
  let pkg_path =
    Flags.M.bindings pkg_map
    |> Lib.List.first_opt (fun (name, pkg_path) ->
           if Lib.FilePath.is_subpath pkg_path path
           then
             let rel_path = Option.get (Lib.FilePath.relative_to pkg_path path) in
             Some (Printf.sprintf "mo:%s/%s" name (Filename.remove_extension rel_path))
           else
             None
         )
  in match pkg_path with
     | Some p -> p
     | None ->
        let (base', path') =
          drop_common_prefix
            String.equal
            (Lib.String.split (Filename.dirname base) '/')
            (Lib.String.split path '/') in
        List.map (fun _ -> "..") base' @ path'
        |> String.concat "/"
        |> Filename.remove_extension

let find_with_prefix
    : string
   -> string
   -> t
   -> (string * ide_decl list) list =
  fun prefix base {modules; package_map; _} ->
  Index.bindings modules
  |> List.map (fun (p, ds) ->
         let import_path =
           if p = "@prim" then
             "mo:prim"
           else
             shorten_import_path package_map base p in
         (import_path, List.filter (decl_has_prefix prefix) ds))
  |> List.filter (fun (_, ds) -> not (ds = []))

let empty : unit -> t = fun _ ->
  let open Pipeline.ResolveImport in
  let resolved_flags =
    Diag.run
      (resolve_flags
         { package_urls = !Flags.package_urls;
           actor_aliases = !Flags.actor_aliases;
           actor_idl_path = !Flags.actor_idl_path;
      }) in
  { modules = Index.empty;
    actors = Index.empty;
    package_map = resolved_flags.packages
  }

module PatternMap = Map.Make(String)
type pattern_map = Source.region PatternMap.t

let rec gather_pat ve pat : pattern_map =
  match pat.it with
  | WildP | LitP _ | SignP _ -> ve
  | VarP id -> PatternMap.add id.it id.at ve
  | TupP pats -> List.fold_left gather_pat ve pats
  | ObjP pfs -> List.fold_left gather_pat_field ve pfs
  | TagP (_, pat1) | AltP (pat1, _) | OptP pat1
  | AnnotP (pat1, _) | ParP pat1 -> gather_pat ve pat1

and gather_pat_field ve pf =
  gather_pat ve pf.it.pat

let string_of_list f xs =
  List.map f xs
  |> String.concat "; "
  |> fun x -> "[ " ^ x ^ " ]"

let string_of_index : declaration_index -> string =
  fun index ->
  Index.bindings index.modules
  |> string_of_list
       (fun (path, decls) ->
         path
         ^ " =>\n    "
         ^ string_of_list string_of_ide_decl decls
         ^ "\n")

let read_single_module_lib (ty: Type.typ): ide_decl list option =
  match ty with
  | Type.Obj (Type.Module, fields) ->
     fields
     |> List.map
          (fun Type.{ lab = name; typ } ->
            (match typ with
             | Type.Typ con -> TypeDecl { name; typ = con; definition = None }
             | typ -> ValueDecl { name; typ; definition = None }
            )
          )
     |> Option.some
  | _ -> None

let unwrap_module_ast (lib : Syntax.lib): Syntax.exp_field list option =
  match lib.it.it with
  | Syntax.BlockE [] -> None
  | Syntax.BlockE decs ->
    (match Lib.List.last decs with
    | {it=Syntax.ExpD {it= Syntax.ObjE(_,fields) ;_} ;_} -> Some fields
    | _ -> None)
  | _ -> None


let populate_definitions
    (libs : Syntax.lib list)
    (path : string)
    (decls : ide_decl list)
    : ide_decl list =
  let is_let_bound exp_field =
    match exp_field.it.Syntax.dec.it with
    | Syntax.LetD(pat, _) -> Some pat
    | _ -> None in
  let is_type_def exp_field =
    match exp_field.it.Syntax.dec.it with
    | Syntax.TypD (typ_id, _, _) ->
       Some typ_id
    | Syntax.ClassD (typ_id, _, _, _, _, _, _) ->
       Some typ_id
    | _ -> None in
  let extract_binders env (pat : Syntax.pat) = gather_pat env pat in
  let find_def (lib : Syntax.lib) def =
    match def with
    | ValueDecl value ->
       let fields = Lib.Option.get (unwrap_module_ast lib) [] in
       let positioned_binder =
         fields
         |> List.filter_map is_let_bound
         |> List.fold_left extract_binders PatternMap.empty
         |> PatternMap.find_opt value.name
       in
       ValueDecl { value with definition = positioned_binder }
    | TypeDecl typ ->
       let fields = Lib.Option.get (unwrap_module_ast lib) [] in
       let type_definition =
         fields
         |> List.filter_map is_type_def
         |> Lib.List.first_opt (fun ty_id ->
                if ty_id.it = typ.name
                then Some ty_id.at
                else None)
       in
       TypeDecl { typ with definition = type_definition } in
  let opt_lib =
    List.find_opt
      (fun lib -> String.equal path (Lib.FilePath.make_absolute (Sys.getcwd ()) lib.note))
      libs in
  match opt_lib with
  | None -> decls
  | Some lib ->
     List.map (find_def lib) decls

let make_index_inner logger vfs entry_points : t Diag.result =
  Pipeline.load_progs
    (Vfs.parse_file vfs)
    entry_points
    Pipeline.initial_stat_env
  |> Diag.map (fun (libs, _, scope) ->
         Type.Env.fold
           (fun path ty acc ->
             let path =
               if path = "@prim" then
                 path
               else
                 Lib.FilePath.make_absolute (Sys.getcwd ()) path in
             add_module
               path
               (ty
                |> read_single_module_lib
                |> Fun.flip Lib.Option.get []
                |> populate_definitions libs path)
               acc)
           scope.Scope.lib_env
           (empty ()))

let make_index logger vfs entry_points : t Diag.result =
  (* TODO(Christoph): Actually handle errors here *)
  try make_index_inner logger vfs entry_points
  with _ -> Diag.return (empty ())
