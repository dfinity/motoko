module Message_adapter =
Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (struct
  let type_field_name = "method"

  let value_field_name = "params"

  let known_tags = None
end)

module Response_message_adapter = struct
  open Yojson.Safe

  let normalize = Fun.id

  let restore (x : t) : t =
    let value_field_name = "result" in
    let unwrap_value (x : t) =
      match x with
      | `String tag -> None
      | `List [ _; v ] -> Some v
      | malformed -> failwith ("Malformed json field " ^ value_field_name)
    in

    match x with
    | `Assoc fields ->
        let fields =
          List.fold_right
            (fun ((k, tagged) as field) acc ->
              if k = value_field_name then
                match unwrap_value tagged with
                | None -> acc
                | Some v -> (value_field_name, v) :: acc
              else field :: acc)
            fields []
        in
        `Assoc fields
    | malformed -> malformed
end
