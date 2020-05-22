val start :
  string (** The entry point *) ->
  bool (** Log debug messages to ls.log? *) ->
  int option
  (** Listen on the given port rather than communicating via stdin/out? *) ->
  'a
