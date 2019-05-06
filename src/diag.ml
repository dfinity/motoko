type severity = Warning | Error
type message = {
  sev : severity;
  at : Source.region;
  cat : string;
  text : string
}
type messages = message list

type 'a result = ('a * messages, messages) Pervasives.result

let return x = Ok (x, [])

let map_result f = function
  | Pervasives.Error msgs -> Pervasives.Error msgs
  | Ok (x, msgs) -> Ok (f x, msgs)

let ignore = function
  | Pervasives.Error msgs -> Pervasives.Error msgs
  | Ok (_, msgs) -> Ok ((), msgs)

let bind x f = match x with
  | Pervasives.Error msgs -> Pervasives.Error msgs
  | Ok (y, msgs1) -> match f y with
    | Ok (z, msgs2) -> Ok (z, msgs1 @ msgs2)
    | Pervasives.Error msgs2 -> Error (msgs1 @ msgs2)

let rec traverse : ('a -> 'b result) -> 'a list -> 'b list result = fun f -> function
  | [] -> return []
  | x :: xs -> bind (f x) (fun y -> map_result (fun ys -> y :: ys) (traverse f xs))

let rec traverse_ : ('a -> unit result) -> 'a list -> unit result = fun f -> function
  | [] -> return ()
  | x :: xs -> bind (f x) (fun () -> traverse_ f xs)

type msg_store = messages ref
let add_msg s m = s := m :: !s
let add_msgs s ms = s := List.rev ms @ !s
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


let flush_messages : 'a result -> 'a result = function
  | Pervasives.Error msgs -> print_messages msgs; Pervasives.Error []
  | Ok (x, msgs) -> print_messages msgs; Ok (x, [])

