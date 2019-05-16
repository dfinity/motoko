type received_message =
  { id : int option
  ; method_ : string
  ; params : Yojson.Basic.t
  }

val parse : Yojson.Basic.t -> received_message (* option *)

val response : int option -> Yojson.Basic.t -> Yojson.Basic.t -> Yojson.Basic.t

val notification : string -> (string * Yojson.Basic.t) list -> Yojson.Basic.t
