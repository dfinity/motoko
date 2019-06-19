open As_values

let counters : Counters.t = Counters.zeros ()

let bump_region reg =
  Counters.bump_region counters reg

let bump_label lab reg =
  Counters.bump_label counters lab reg

let process_prog_result result =
  if !ProfilerFlags.profile then
    try
      match result with
        Some(Value.Async a,_) -> begin
          match Lib.Promise.value_opt a.Value.result with
          | Some v -> Counters.dump counters (Value.as_obj v)
          | None   -> ()
        end
      | _  -> ()
    with
    | Invalid_argument _ -> () ;
