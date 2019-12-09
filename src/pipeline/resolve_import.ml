open Mo_def

(*
This module traverses the syntax tree. For each `import` statement, it looks
at the given relative path and tries to resolve it to a full path (where
full means relative to the current working directory, so that source
directories do not creep into the build output). If no file can be found
there, this prints an error message, otherwise it stores the real path
in the second, mutable field of the ImportE statement.

It returns a list of all imported file names.

*)

(* written as a functor so we can allocate some temporary shared state without making it global *)

type filepath = string

module S = Set.Make(String)
module M = Map.Make(String)

(* a map of type package_map will map each package name to a(n optional) package URL,
   which for now is just a filesystem path:

   e.g.,
   packages("std") = "/Users/home/username/.dfinity-sdk/src/mo-stdlib/0.1.0/"
   packages("foo") = "/Users/home/username/fooPackage/1.2.3/src"
*)

type package_map = string M.t

type env = {
  msgs : Diag.msg_store;
  base : filepath;
  packages : package_map;
  imported : S.t ref;
}

open Syntax
open Source

(* match `f` against the URL pattern 'mo://package-name/path',
   optionally returning the package-name and path components as a pair of strings.

   e.g.,
   match_package_name "mo:std/list"    = Some("std", "list")
   match_package_name "mo:std/foo/bar" = Some("std", "foo/bar")
   match_package_name "mo:foo/bar"     = Some("foo", "bar")

   match_package_name "mo:"            = None
   match_package_name "mo:std"         = None
   match_package_name "std/foo"        = None
   match_package_name "std/foo/bar"    = None

*)
let match_package_name (f: string) : (string * string) option =
  let rec loop (f: string) (path_accum:string) : (string * string) option =
    let (dir, base) = (Filename.dirname f, Filename.basename f) in
    match dir with
    | "." -> if path_accum = "" then None else Some (f, path_accum)
    | _   ->
      let path_accum =
        match path_accum with
        | "" -> base
        | _  -> Filename.concat base path_accum
      in
      loop dir path_accum
  in
  if String.length f < 3 then None else
    let (prefix, suffix) = (
        String.sub f 0 3,
        String.sub f 3 ((String.length f) - 3)
      )
    in
    match prefix with
    | "mo:" -> loop suffix ""
    | _     -> None

(* using env, resolve import strings of the form "mo:package-name/mod1/mod2/item"
   into the triple ("package-name", "package-url", "mod1/mod2/item") when the package name is defined.
   Does not validate the package url or path.
 *)
let resolve_package env (f: string) : (string * string * string) option =
  match match_package_name f with
  | None -> None
  | Some (name, path) ->
    if M.mem name env.packages then
      Some (name, M.find name env.packages, path)
    else
      None

let resolve_import_string env region (f: string) (fp: string ref) =
  let f =
    match resolve_package env f with
    | None -> f
    | Some (_pname, url, path) -> Filename.concat url path
  in
  let f =
    if Filename.is_relative f
    then Filename.concat env.base f
    else f in
  let f =
    if Sys.file_exists f && Sys.is_directory f
    then Filename.concat f "lib.mo"
    else f in
  let f = File_path.normalise f in
  if Sys.file_exists f
  then begin
      fp := f;
      env.imported := S.add f !(env.imported)
    end else
    let open Diag in
    add_msg env.msgs {
        sev = Error;
        at = region;
        cat = "import";
        text = Printf.sprintf "File \"%s\" does not exist" f
      }

(* compare to the filesystem semantics of function `resolve_import_string`:
   the import-string to filesystem-path resolution semantics are related, but distinct.
 *)
let resolve_package_url (msgs:Diag.msg_store) (base:filepath) (pname:string) (f: string) : string option =
  let f =
    if Filename.is_relative f
    then Filename.concat base f
    else f in
  let f = File_path.normalise f in
  if Sys.file_exists f then
    Some f
  else
    let open Diag in
    add_msg msgs {
        sev = Error;
        at = no_region;
        cat = "package";
        text = Printf.sprintf "File \"%s\" (for package `%s`) does not exist" f pname
      };
    None

let rec over_exp (f : exp -> exp) (exp : exp) : exp = match exp.it with
  | ImportE _ | PrimE _ | VarE _ | LitE _ -> f exp
  | UnE (x, y, exp1) -> f { exp with it=UnE (x, y, over_exp f exp1) }
  | ShowE (x, exp1) -> f { exp with it=ShowE (x, over_exp f exp1) }
  | ProjE (exp1, x) -> f { exp with it=ProjE (over_exp f exp1, x) }
  | OptE exp1 -> f { exp with it=OptE (over_exp f exp1) }
  | TagE (x, exp1) -> f { exp with it=TagE (x, over_exp f exp1) }
  | DotE (exp1, x) -> f { exp with it=DotE (over_exp f exp1, x) }
  | NotE exp1 -> f { exp with it=NotE (over_exp f exp1) }
  | AssertE exp1 -> f { exp with it=AssertE (over_exp f exp1) }
  | LabelE (x, y, exp1) -> f { exp with it=LabelE (x, y, over_exp f exp1) }
  | BreakE (x, exp1) -> f { exp with it=BreakE (x, over_exp f exp1) }
  | RetE exp1 -> f { exp with it=RetE (over_exp f exp1) }
  | AnnotE (exp1, x) -> f { exp with it=AnnotE (over_exp f exp1, x) }
  | AsyncE exp1 -> f { exp with it=AsyncE (over_exp f exp1) }
  | AwaitE exp1 -> f { exp with it=AwaitE (over_exp f exp1) }
  | ThrowE exp1 -> f { exp with it=ThrowE (over_exp f exp1) }
  | BinE (x, exp1, y, exp2) ->
     f { exp with it=BinE (x, over_exp f exp1, y, over_exp f exp2) }
  | IdxE (exp1, exp2) ->
     f { exp with it=IdxE (over_exp f exp1, over_exp f exp2) }
  | RelE (x, exp1, y, exp2) ->
     f { exp with it=RelE (x, over_exp f exp1, y, over_exp f exp2) }
  | AssignE (exp1, exp2) ->
     f { exp with it=AssignE (over_exp f exp1, over_exp f exp2) }
  | CallE (exp1, x, exp2) ->
     f { exp with it=CallE (over_exp f exp1, x, over_exp f exp2) }
  | AndE (exp1, exp2) ->
     f { exp with it=AndE (over_exp f exp1, over_exp f exp2) }
  | OrE (exp1, exp2) ->
     f { exp with it=OrE (over_exp f exp1, over_exp f exp2) }
  | WhileE (exp1, exp2) ->
     f { exp with it=WhileE (over_exp f exp1, over_exp f exp2) }
  | LoopE (exp1, exp2_opt) ->
     f { exp with it=LoopE (over_exp f exp1, Lib.Option.map (over_exp f) exp2_opt) }
  | ForE (x, exp1, exp2) ->
     f { exp with it=ForE (x, over_exp f exp1, over_exp f exp2) }
  | DebugE exp1 ->
     f { exp with it=DebugE (over_exp f exp1) }
  | TupE exps ->
     f { exp with it=TupE (List.map (over_exp f) exps) }
  | ArrayE (x, exps) ->
     f { exp with it=ArrayE (x, List.map (over_exp f) exps) }
  | BlockE ds ->
     f { exp with it=BlockE (List.map (over_dec f) ds) }
  | ObjE (x, efs) ->
     f { exp with it=ObjE (x, List.map (over_exp_field f) efs) }
  | IfE (exp1, exp2, exp3) ->
     f { exp with it=IfE(over_exp f exp1, over_exp f exp2, over_exp f exp3) }
  | TryE (exp1, cases) ->
     f { exp with it=TryE (over_exp f exp1, cases) }
  | SwitchE (exp1, cases) ->
     f { exp with it=SwitchE (over_exp f exp1, cases) }
  | FuncE (a, b, c, d, g, e) ->
    f { exp with it=FuncE (a, b, c, d, g, over_exp f e) }

and over_dec (f : exp -> exp) (d : dec) : dec = match d.it with
  | TypD _ -> d
  | ExpD e -> { d with it=ExpD (over_exp f e)}
  | VarD (x, e) ->
     { d with it=VarD (x, over_exp f e)}
  | LetD (x, e) ->
     { d with it=LetD (x, over_exp f e)}
  | ClassD (a, b, c, d1, e, g, efs) ->
     { d with it=ClassD (a, b, c, d1, e, g, List.map (over_exp_field f) efs)}

and over_exp_field (f : exp -> exp) (ef : exp_field) : exp_field =
  { ef with it={ ef.it with dec=over_dec f ef.it.dec } }

let collect_imports (prog : prog): string list =
  let res = ref [] in
  let f e = match e.it with
    | ImportE (f, _) -> res := f::!res; e
    | _ -> e in
  let _ = List.iter (fun d -> ignore (over_dec f d)) prog.it in
  !res

let prog env p =
  let f e = match e.it with
    | ImportE (f, fp) -> resolve_import_string env e.at f fp; e
    | _ -> e in
  List.iter (fun d -> ignore (over_dec f d)) p.it

type package_urls = (string * string) list

let resolve_packages : package_urls -> filepath -> package_map Diag.result = fun purls base ->
  Diag.fold (fun package_map (package_name, package_url) ->
      if M.mem package_name package_map then
        Diag.with_message_store (fun msgs ->
            let open Diag in
            Diag.add_msg msgs {
                sev = Error;
                at = no_region;
                cat = "--package";
                text = Printf.sprintf "Package name \"%s\" already defined" package_name;
              };
            None
          )
      else
        Diag.with_message_store (fun msgs ->
            match resolve_package_url msgs base package_name package_url with
            | None              -> None
            | Some resolved_url -> Some (M.add package_name resolved_url package_map)
          )
    )
    M.empty purls

let resolve : package_urls -> Syntax.prog -> filepath -> S.t Diag.result = fun purls p base ->
  Diag.bind (resolve_packages purls base) (fun (packages:package_map) ->
      Diag.with_message_store (fun msgs ->
          let base = if Sys.is_directory base then base else Filename.dirname base in
          let env = { msgs; base; imported = ref S.empty; packages } in
          prog env p;
          Some !(env.imported)
        )
    )
