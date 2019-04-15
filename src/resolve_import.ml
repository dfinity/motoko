(*
This module traverses the syntax tree. For each `import` statement, it looks
at the given relative path and tries to resolve it to a full path (where
full means relative to the current working directory, so that source
directories do not creep into the build output). If no file can be found
there, this prints an error message, otherwise it stores the real path
in the second, mutable field of the ImportE statement.

It returns a list of all imported file names.

At some point, SEARCH_PATH functionality would be added here.
*)

(* written as a functor so we can allocate some temporary shared state without making it global *)

type filepath = string

module S = Set.Make(String)

type env = {
  msgs : Diag.msg_store;
  base : filepath;
  imported : S.t ref;
}

open Syntax
open Source

let rec
  exp env (e : exp) = match e.it with
  | ImportE (f, fp) ->
    let f =
      if Filename.is_relative f
      then Filename.concat env.base f
      else f in
    let f =
      if Sys.file_exists f && Sys.is_directory f
      then Filename.concat f "lib.as"
      else f in
    if Sys.file_exists f
    then begin
      fp := f;
      env.imported := S.add f !(env.imported)
    end else
      let open Diag in
      add_msg env.msgs {
        sev = Error;
        at = e.at;
        cat = "import";
        text = Printf.sprintf "File \"%s\" does not exist" f
      }
  (* The rest is just a boring syntax traversal *)
  | (PrimE _ | VarE _ | LitE _) -> ()
  | UnE (_, _, exp1)
  | ShowE (_, exp1)
  | ProjE (exp1, _)
  | OptE exp1
  | VariantE (_, exp1)
  | DotE (exp1, _)
  | NotE exp1
  | AssertE exp1
  | LabelE (_, _, exp1)
  | BreakE (_, exp1)
  | RetE exp1
  | AnnotE (exp1, _)
  | AsyncE exp1
  | AwaitE exp1
  | LoopE (exp1, None) ->
    exp env exp1
  | BinE (_, exp1, _, exp2)
  | IdxE (exp1, exp2)
  | RelE (_, exp1, _, exp2)
  | AssignE (exp1, exp2)
  | CallE (exp1, _, exp2)
  | AndE (exp1, exp2)
  | OrE (exp1, exp2)
  | WhileE (exp1, exp2)
  | LoopE (exp1, Some exp2)
  | ForE (_, exp1, exp2) ->
    exp env exp1; exp env exp2
  | TupE exps
  | ArrayE (_, exps) ->
    List.iter (exp env) exps
  | BlockE ds ->
    decs env ds
  | ObjE (_, efs) ->
    List.iter (fun ef -> dec env ef.it.dec) efs
  | IfE (exp1, exp2, exp3) ->
    exp env exp1;
    exp env exp2;
    exp env exp3
  | SwitchE (exp1, cases) ->
    exp env exp1;
    List.iter (fun c -> exp env c.it.exp) cases
  | FuncE (_, _, _ , _ , _, e) ->
    exp env e

and decs env = List.iter (dec env)
and dec env d = match d.it with
  | TypD _ -> ()
  | ExpD e
  | VarD (_, e)
  | LetD (_, e) ->
    exp env e
  | ClassD (_, _, _, _ , _, efs) ->
    List.iter (fun ef -> dec env ef.it.dec) efs

let prog env p = decs env p.it

let resolve : Syntax.prog -> filepath -> S.t Diag.result = fun p base ->
  Diag.with_message_store (fun msgs ->
    let base = if Sys.is_directory base then base else Filename.dirname base in
    let env = { msgs; base; imported = ref S.empty } in
    prog env p;
    Some !(env.imported)
  )
