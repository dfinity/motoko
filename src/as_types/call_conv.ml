open Type

type call_conv = {
  sort: sharing;
  control : control;
  n_args : int;
  n_res : int;
}
type t = call_conv

let local_cc n m = { sort = Local; control = Returns; n_args = n; n_res = m}
let message_cc n = { sort = Sharable; control = Returns; n_args = n; n_res = 0}
let async_cc n = { sort = Sharable; control = Promises; n_args = n; n_res = 1}

let call_conv_of_typ typ =
  match typ with
  | Func(sort, control, tbds, dom, res) ->
    { sort; control; n_args = List.length dom; n_res = List.length res }
  | Non ->
    { sort = Type.Local; control = Type.Returns; n_args = 1; n_res = 1 }
  | _ -> raise (Invalid_argument ("call_conv_of_typ " ^ string_of_typ typ))

let string_of_call_conv {sort;control;n_args;n_res} =
  Printf.sprintf "(%s %i %s %i)"
    (string_of_sharing sort)
    n_args
    (match control with Returns -> "->" | Promises -> "@>")
    n_res

