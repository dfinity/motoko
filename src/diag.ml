type severity = Warning | Error
type message = {
  sev : severity;
  at : Source.region;
  cat : string;
  text : string
}
type messages = message list

type 'a result = ('a * messages, messages) Pervasives.result

let map_result f = function
  | Pervasives.Error msgs -> Pervasives.Error msgs
  | Ok (x, msgs) -> Ok (f x, msgs)

type msg_store = messages ref
let add_msg s m = s := m :: !s
let get_msgs s = List.rev !s

let has_errors : messages -> bool =
  List.fold_left (fun b msg -> b || msg.sev == Error) false

let fatal_error at text = { sev = Error; at; cat = "fatal"; text }

let string_of_message msg =
  let label = match msg.sev with
    | Error -> Printf.sprintf "%s error"  msg.cat
    | Warning -> "warning" in
  Printf.sprintf "%s: %s, %s\n" (Source.string_of_region msg.at) label msg.text

let print_message msg =
  Printf.eprintf "%s%!" (string_of_message msg)

let print_messages = List.iter print_message

let with_message_store f =
  let s = ref [] in
  let r = f s in
  let msgs = get_msgs s in
  match r with
  | Some x when not (has_errors msgs) -> Ok (x, msgs)
  | _ -> Error msgs
