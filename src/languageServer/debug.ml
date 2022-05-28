let logger : (string -> string -> unit) ref = ref (fun _ _ -> ())
let log : string -> string -> unit = fun msg -> !logger msg
