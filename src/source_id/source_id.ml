let release = Generated.release
let id = Generated.id
let banner = match Generated.release with
  | None -> "(source " ^ Generated.id ^ ")"
  | Some r -> r ^ " (source " ^ Generated.id ^ ")"
