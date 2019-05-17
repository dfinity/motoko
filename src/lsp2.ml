module Message_adapter = Atdgen_runtime.Json_adapter.Type_and_value_fields.Make(
  struct
    let type_field_name = "method"
    let value_field_name = "params"
    let known_tags = None
  end
)
