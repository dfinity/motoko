
(* TODO: replace with Printexc.default_uncaught_exception_handler with OCaml 4.11*)
let default_uncaught_exception_handler exn raw_backtrace =
  Printexc.(
    Printf.eprintf "Fatal error: exception %s\n" (to_string exn);
    print_raw_backtrace stderr raw_backtrace;
    flush stderr)

let setup_handler () =
  Printexc.record_backtrace true;
  Printexc.set_uncaught_exception_handler (fun exn rb ->
    Printf.eprintf "OOPS! You've triggered a compiler bug.\n";
    Printf.eprintf "Please report this Motoko issue at forum.dfinity.org with the following details:\n\nMotoko (revision %s)\n\n" Source_id.id;
    default_uncaught_exception_handler exn rb
  );
