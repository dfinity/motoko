open As_types
open As_def
open Source
open Syntax

let flat_map f xs = List.flatten (List.map f xs)

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
           (Lib.Option.map
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
           (Lib.Option.map
              (fun pos -> ", definition = " ^ string_of_region pos)
              ty.definition)
           ""
       ^ " }"

let name_of_ide_decl (d : ide_decl) : string =
  match d with
  | ValueDecl value -> value.name
  | TypeDecl ty -> ty.name

module Index = Map.Make(String)
type declaration_index = (ide_decl list) Index.t

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

let string_of_index index =
  Index.bindings index
  |> string_of_list
       (fun (path, decls) ->
         path
         ^ " =>\n    "
         ^ string_of_list string_of_ide_decl decls
         ^ "\n")

let project_files () : string list =
  let rec read_dir d =
    if Sys.is_directory d
    then
      let entries =
        Array.to_list (Sys.readdir d)
        |> List.map (fun file -> Filename.concat d file) in
      flat_map read_dir entries
    else [d] in
  read_dir "."
  |> List.filter (fun file -> String.equal (Filename.extension file) ".as")

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
     |> Lib.Option.some
  | _ -> None

let unwrap_module_ast (prog : Syntax.dec list): Syntax.exp_field list option =
  match prog with
  | ({it=Syntax.ExpD {it= Syntax.ObjE(_,fields) ;_} ;_} :: _) ->
     Some fields
  | _ -> None


let populate_definitions
    (libraries : Syntax.libraries)
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
    | _ -> None in
  let extract_binders env (pat : Syntax.pat) = gather_pat env pat in
  let find_def (prog : Syntax.dec list) def = match def with
    | ValueDecl value ->
       let fields = Lib.Option.get (unwrap_module_ast prog) [] in
       let positioned_binder =
         fields
         |> Lib.List.map_filter is_let_bound
         |> List.fold_left extract_binders PatternMap.empty
         |> PatternMap.find_opt value.name
       in
       ValueDecl { value with definition = positioned_binder }
    | TypeDecl typ ->
       let fields = Lib.Option.get (unwrap_module_ast prog) [] in
       let type_definition =
         fields
         |> Lib.List.map_filter is_type_def
         |> Lib.List.first_opt (fun ty_id ->
                if ty_id.it = typ.name
                then Some ty_id.at
                else None)
       in
       TypeDecl { typ with definition = type_definition } in
  let opt_lib =
    List.find_opt
      (fun (path', _) -> String.equal path path')
      libraries in
  match opt_lib with
  | None -> decls
  | Some (_, prog) ->
     List.map (find_def prog.it) decls

let make_index_inner () : declaration_index Diag.result =
  let entry_points = project_files () in
  Pipeline.chase_imports
    Pipeline.initial_stat_env
    (Pipeline__.Resolve_import.S.of_list entry_points)
  |> Diag.map (fun (libraries, scope) ->
      Type.Env.fold
        (fun path ty acc ->
        Index.add
            path
            (ty
            |> read_single_module_lib
            |> Lib.Fun.flip Lib.Option.get []
            |> populate_definitions libraries path)
            acc)
        scope.Scope.lib_env
        Index.empty)

let make_index () : declaration_index Diag.result =
  (* TODO(Christoph): Actually handle errors here *)
  try make_index_inner () with _ -> Diag.return Index.empty
