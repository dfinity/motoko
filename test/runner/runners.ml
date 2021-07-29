(** This module defines runners for `moc`, `diff`, `drun`, and other
    executables that we want to run in tests. *)

(** Returns `moc` exit code *)
let moc ~(mo_file_path : string) ?(moc_output_path : string option = None)
    ?(args : string list = []) ?(env : string list = []) () : int =
  let args = String.concat " " args in
  let env = String.concat " " env in

  let cmd = Printf.sprintf "env %s moc %s %s" env mo_file_path args in

  let cmd =
    match moc_output_path with
    | None -> cmd
    | Some output_path -> cmd ^ " > " ^ output_path
  in

  Sys.command cmd

(** Returns `drun` exit code *)
let drun ~(config_path : string) ~(script_path : string)
    ~(drun_output_path : string) : int =
  let cmd =
    Printf.sprintf "drun -c %s --extra-batches 1 %s > %s 2>&1" config_path
      script_path drun_output_path
  in

  Sys.command cmd

(** Run `diff` on expected and actual output files. If `diff` returns 1 (i.e.
    outputs do not match) then fail with "outputs do not match". If it returns
    2 then something else is wrong (probably file doesn't exist) so report a
    diff error.

    `diff` output won't be redirected so check the test output to see it. *)

let diff ~(expected_output_path : string) ~(actual_output_path : string) : unit
    =
  (* TODO: We probably should't call Alcotest here? *)
  let cmd =
    Printf.sprintf "diff -a -u -N --label expected %s --label actual %s"
      expected_output_path actual_output_path
  in

  let exit_code = Sys.command cmd in

  if exit_code <> 1 then
    Alcotest.fail
      (Printf.sprintf
         "Expected and actual drun outputs do not match, diff returned %d"
         exit_code)
  else
    Alcotest.fail
      "Expected and actual drun outputs do not match. See test output for the \
       diff."
