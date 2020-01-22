let logger : (string -> unit) ref = ref ignore
let log : string -> unit = fun msg -> !logger msg
