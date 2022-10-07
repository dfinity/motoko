open Source
open Syntax

open Format

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
    fprintf ppf "@[<2>field %s :@ %a@]"
      id.it
      pp_typ typ
  | MethodI (id, locals, rets, pres, posts, bo) ->
    fprintf ppf "@[<2>method %s%a :@ %a %a %a %a @]"
      id.it
      pp_locals locals
      pp_returns rets
      pp_pres pres
      pp_posts posts
      pp_block_opt bo

and pp_block_opt ppf bo =
  match bo with
  | None -> ()
  | Some ss -> () (* TODO *)

and pp_pres ppf exps =
   fprintf ppf "@[<v 0>%a@]" (pp_print_list pp_pre) exps

and pp_pre ppf exp =
   fprintf ppf "requires @[<2>%a@]" pp_exp exp

and pp_posts ppf exps =
   fprintf ppf "@[<v 0>%a@]" (pp_print_list pp_pre) exps

and pp_post ppf exp =
   fprintf ppf "ensures @[<2>%a@]" pp_exp exp

and pp_local ppf (id, typ) =
  fprintf ppf "@[<2>%s :@ %a@]"
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

and pp_exp ppf exp =
  match exp.it with
  | _ -> pr ppf "?" (* TBC *)

let prog p =
  Lib.Format.with_str_formatter (fun ppf ->
    pp_prog ppf) p

