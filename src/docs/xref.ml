type t =
  | XType of string
  | XValue of string
  | XNested of string * t
  | XClass of string * t
  | XFile of string * t option
  | XPackage of string * t option

let rec to_string : t -> string = function
  | XType s -> "type " ^ s
  | XValue s -> "value " ^ s
  | XNested (s, xref) -> Printf.sprintf "%s.%s" s (to_string xref)
  | XFile (s, None) -> "file " ^ s
  | XFile (s, Some xref) -> Printf.sprintf "file %s -> %s" s (to_string xref)
  | XClass (s, xref) -> Printf.sprintf "class %s -> %s" s (to_string xref)
  | XPackage (s, None) -> "package " ^ s
  | XPackage (s, Some xref) ->
      Printf.sprintf "package %s -> %s" s (to_string xref)

let rec extend : t -> (t -> t) option = function
  | XType _ | XValue _ -> None
  | XFile (s, None) -> Some (fun xref -> XFile (s, Some xref))
  | XPackage (s, None) -> Some (fun xref -> XPackage (s, Some xref))
  | XNested (s, xref) -> Option.map (fun f x -> XNested (s, f x)) (extend xref)
  | XClass (s, xref) -> Option.map (fun f x -> XClass (s, f x)) (extend xref)
  | XFile (s, Some xref) ->
      Option.map (fun f x -> XFile (s, Some (f x))) (extend xref)
  | XPackage (s, Some xref) ->
      Option.map (fun f x -> XPackage (s, Some (f x))) (extend xref)
