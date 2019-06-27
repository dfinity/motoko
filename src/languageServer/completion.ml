open As_frontend
open As_types
open Printf

let string_of_list f xs =
  List.map f xs
  |> String.concat "; "
  |> fun x -> "[" ^ x ^ "]"

let lib_files (_: unit) : string list =
  let lib_dir = "lib" in
  Sys.readdir lib_dir
  |> Array.to_list
  |> List.filter (fun file -> String.equal (Filename.extension file) ".as")
  |> List.map (fun file -> Filename.concat lib_dir file)

(* We create a synthetic module containing imports for all known
   library modules.

   This way when we run the frontend on that module we end up with a
   library environment containing all available library definitions *)
let synthetic_module (): Syntax.prog =
  let empty_note x =
    Source.{ it = x; at = no_region; note = Syntax.empty_typ_note } in
  let empty_region x =
    Source.{ it = x; at = no_region; note = () } in
  let empty_var x =
    Source.{ it = Syntax.VarP(empty_region x)
           ; at = no_region
           ; note = Type.Pre } in
  let make_import ix path: Syntax.dec =
    empty_note (Syntax.LetD
                  ( empty_var ("I" ^ string_of_int ix)
                  , empty_note (Syntax.ImportE(path, ref "")))) in
  let module_wrapper (fields: Syntax.dec list): Syntax.dec =
    empty_note
      (Syntax.ExpD
         (empty_note
            (Syntax.ObjE
               (empty_region Type.Module,
                (List.map
                   (fun dec ->
                     empty_region
                       (Syntax.{ dec = dec
                               ; vis = empty_region Syntax.Private}))
                   fields))))) in
  let imports =
    lib_files ()
    |> List.mapi make_import
    |> module_wrapper in
  Source.{ it = [imports]; at = no_region; note = "" }

let read_single_module_lib ({Source.it = prog;_}: Syntax.prog)
    : Syntax.exp_field list option =
  match prog with
  | [{ Source.it = Syntax.ExpD({ Source.it = md;_});_}] ->
     (match md with
      | Syntax.ObjE({Source.it = sort;_}, fields) when sort = Type.Module ->
         Some(fields)
      | _ -> None)
  | _ -> None

let name_of_decl dec =
  match dec.Source.it with
  (* | ExpD of exp                                (\* plain expression *\) *)
  | Syntax.LetD(Source.{ it = Syntax.VarP Source.{it = name;_};_}, _) ->
     name
  | Syntax.VarD(Source.{ it = name;_}, _) ->
     name
  | Syntax.TypD(Source.{ it = name;_}, _, _) -> name
  | _ -> "unknown"
  (* | ClassD of                                  (\* class *\)
   *     typ_id * typ_bind list * obj_sort * pat * id * exp_field list *)

let string_of_vis Source.{it = vis;_} =
  match vis with
  | Syntax.Public -> "public"
  | Syntax.Private -> "private"

let string_of_exp_field ({Source.it = exp_field;_}: Syntax.exp_field): string =
  string_of_vis exp_field.Syntax.vis ^ " " ^ name_of_decl exp_field.Syntax.dec

let libraries () =
  let synthetic_parse = Diag.return [(synthetic_module (), ".")] in
  let (libraries, _, _) = Diag.run (Pipeline.load_libraries synthetic_parse) in
  libraries |> string_of_list (fun (path, prog) ->
                   path
                   ^ ": "
                   ^ Base.Option.value_map
                       (read_single_module_lib prog)
                       ~f:(string_of_list string_of_exp_field)
                       ~default: "")

let complete () =
  libraries ()
  |> printf "%s"

(* let is_module_export (exp: Syntax.exp): bool = *)
