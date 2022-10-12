open Source
open Syntax

open Format

let line = ref 0

let pr = pp_print_string

let comma ppf () = fprintf ppf ",@ "

let semi ppf () = fprintf ppf ";@ "

let pp_info ppf NoInfo = ()

let rec pp_prog ppf p =
  match p.it with
  | is ->
   fprintf ppf "@[<v 0>%a@]"
   (pp_print_list pp_item) is

and pp_item ppf i =
  match i.it with
  | FieldI (id, typ) ->
    fprintf ppf "@[<2>field %s:@ %a@]"
      id.it
      pp_typ typ
  | MethodI (id, locals, rets, pres, posts, bo) ->
     fprintf ppf "@[<hov 2>method %s%a@ %a@ %a@ %a@; %a@]"
      id.it
      pp_locals locals
      pp_returns rets
      pp_pres pres
      pp_posts posts
      pp_block_opt bo

and pp_block_opt ppf bo =
  match bo with
  | None -> ()
  | Some seqn ->
    pp_seqn ppf seqn

and pp_seqn ppf seqn =
    let (ds, ss) = seqn.it in
    fprintf ppf "@[<v 2>{ %a@ %a @;<0 -2>}@]"
     (pp_print_list pp_decl) ds
     (pp_print_list pp_stmt) ss

and pp_decl ppf decl =
    let (id, typ) = decl.it in
    fprintf ppf "@[<v 0>var %s: %a@]"
    id.it
    pp_typ typ

and pp_pres ppf exps =
   fprintf ppf "@[<v 0>%a@]" (pp_print_list pp_pre) exps

and pp_pre ppf exp =
   fprintf ppf "requires @[<2>%a@]" pp_exp exp

and pp_posts ppf exps =
   fprintf ppf "@[<v 0>%a@]" (pp_print_list pp_pre) exps

and pp_post ppf exp =
   fprintf ppf "ensures @[<2>%a@]" pp_exp exp

and pp_local ppf (id, typ) =
  fprintf ppf "@[<2>%s: %a@]"
    id.it
    pp_typ typ

and pp_locals ppf pars =
  fprintf ppf "@[<1>(%a)@]"
    (pp_print_list ~pp_sep:comma (pp_local)) pars

and pp_returns ppf pars =
  match pars with
  | [] -> ()
  | _ ->
    fprintf ppf "returns @[<1>(%a)@]"
      (pp_print_list ~pp_sep:comma (pp_local)) pars

and pp_typ ppf t =
  match t.it with
  | IntT -> pr ppf "Int"
  | BoolT -> pr ppf "Bool"
  | RefT -> pr ppf "Ref"

and pp_exp ppf exp =
  match exp.it with
  | LocalVar (id, _) ->
     fprintf ppf "%s" id.it
  | FldAcc fldacc ->
     pp_fldacc ppf fldacc
  | NotE e ->
     fprintf ppf "@[(!%a)@]" pp_exp e
  | BoolLitE b ->
     fprintf ppf "%s" (if b then "true" else "false")
  | IntLitE i ->
     fprintf ppf "%s" (Mo_values.Numerics.Int.to_string i)

and pp_stmt ppf stmt =
  Printf.eprintf "\nLINES: %d -> %d" stmt.at.left.line !line;
  match stmt.it with
  | SeqnS seqn -> pp_seqn ppf seqn
  | IfS(exp1, s1, { it = ([],[]); _ }) ->
    fprintf ppf "@[<v 2>if %a@ %a@]"
      pp_exp exp1
      pp_seqn s1
  | IfS(exp1, s1, s2) ->
    fprintf ppf "@[<v 2>if %a@ %aelse@ %a@]"
      pp_exp exp1
      pp_seqn s1
      pp_seqn s2
  | VarAssignS(id, exp) ->
    fprintf ppf "@[<v 2>%s := %a@]"
      id.it
      pp_exp exp
  | FieldAssignS(fldacc, exp2) ->
    fprintf ppf "@[<v 2>%a := %a@]"
      pp_fldacc fldacc
      pp_exp exp2

and pp_fldacc ppf fldacc =
  match fldacc with
  | (exp1, id) ->
    fprintf ppf "@[(%a).%s@]" pp_exp exp1 id.it

let prog p =
    let b = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer b in
    let outfs = pp_get_formatter_out_functions ppf () in
    let out_newline () = line := !line + 1; outfs.out_newline () in
    pp_set_formatter_out_functions ppf { outfs with out_newline };
    Format.fprintf ppf "@[%a@]" pp_prog p;
    Format.pp_print_flush ppf ();
    Printf.eprintf "\nLINES: %d" !line;
    Buffer.contents b
