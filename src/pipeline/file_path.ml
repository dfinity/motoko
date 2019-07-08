let normalise file_path =
  let has_trailing_slash =
    Base.Option.is_some (Base.String.chop_suffix ~suffix:"/" file_path) in
  let has_leading_slash = not (Filename.is_relative file_path) in
  let acc = Stack.create () in
  Lib.String.split file_path '/'
  |> List.iter
       (function
        | "" -> ()
        | "." -> ()
        | ".." ->
           if Stack.is_empty acc
           then Stack.push ".." acc
           else ignore (Stack.pop acc)
        | segment -> Stack.push segment acc);
  let result = Stack.fold (fun x y -> y ^ "/" ^ x) "" acc in
  let prepend_leading_slash s = if has_leading_slash then "/" ^ s else s in
  prepend_leading_slash
    (if has_trailing_slash
     then result
     else Base.String.chop_suffix_exn ~suffix:"/" result)

let relative_to base path =
  Base.String.chop_prefix
    ~prefix:(normalise (base ^ "/"))
    (normalise path)
