
let setup_handler () =
  Printexc.record_backtrace true;
  Printexc.set_uncaught_exception_handler (fun exn rb ->
    Printf.eprintf "OOPS! You've triggered a compiler bug.\n";
    Printf.eprintf "Please report this at https://github.com/dfinity/motoko/issues/new with the following details:\n\nMotoko %s\n\n" Source_id.banner;
    Printexc.default_uncaught_exception_handler exn rb
  );
