open Mo_config.Flags

val error_codes : (string * string option) list
val warning_codes : (string * string option * string * lint_level) list
val try_find_explanation : string -> (string * string option) option
val default_warning_level_overrides : lint_level M.t
