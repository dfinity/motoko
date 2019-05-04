(* TODO: Move to protocol module *)

(* type t = { foo: int; } [@@deriving rpcty] *)
(* type t = { foo: int; } [@@deriving rpc] *)

(* Jsonrpc.to_string (rpc_of_t x) *)

(* open Rpc
 * open Idl *)

(* type int = Rpc.Types.int *)
(* module Foo = Rpc.Idl *)

(*
module CalcInterface(R : Idl.RPC) = struct
  open R

  let int_p = Idl.Param.mk Rpc.Types.int

  let add = R.declare "add"
    ["Add two numbers"]
    (int_p @-> int_p @-> returning int_p Idl.DefaultError.err)

  let mul = R.declare "mul"
    ["Multiply two numbers"]
    (int_p @-> int_p @-> returning int_p Idl.DefaultError.err)

  let implementation = implement
    { Idl.Interface.name = "Calc"
    ; namespace = Some "Calc"
    ; description = ["Calculator supporting addition and multiplication"]
    ; version = (1,0,0)
    }
end

module M = Idl.IdM
module MyIdl = Idl.Make(M)

module CalcClient :
  sig
    val add :
      (Rpc.call -> Rpc.response) ->
      int -> int -> (int, Idl.DefaultError.t) result
    val mul :
      (Rpc.call -> Rpc.response) ->
      int -> int -> (int, Idl.DefaultError.t) result
  end = CalcInterface(MyIdl.GenClient ())
*)

(*
module MyAPI(R : Idl.RPC) = struct
  open R
  open Idl

  let implementation = implement
    { name = "MyAPI"
    ; namespace = Some "MyApi"
    ; version = (1, 0, 0)
    ; description =
        [ "This is an example showing how to use the IDL part of the ocaml-rpc "
        ; "library. What goes here is a description of the interface we're "
        ; "currently describing."
        ]
    ;
    }

  (* We can define some named parameters here. These can be used in different
     method declarations. *)
  let i1 = Param.mk ~name:"i1" ~description:["Parameter i1"] Rpc.Types.int
  let s1 = Param.mk ~name:"s1" ~description:["Parameter s1"] Rpc.Types.string

  (* Parameters don't _have_ to have names and descriptions, in which case they
     will inherit from the name and description of the type. *)
  let b1 = Param.mk Rpc.Types.bool

  (* For the following methods, we use the default error type for any errors
     the methods may throw *)
  let e1 = Idl.DefaultError.err

  (* `declare` is defined in the RPC module passed in, and can do several
     different things. It is only the following two lines that actually do
     anything in this module - the declarations of parameters above are useful
     only in allowing the two declarations here to be succinct. *)
  let api1 = declare "api1" ["Description 1"] (i1 @-> s1 @-> returning b1 e1)
  let api2 = declare "api2" ["Description 2"] (s1 @-> returning i1 e1)

end

(* You can swap the rpc engine, by using a different monad here,
   note however that if you are using an asynchronous one, like
   lwt or async, you should also use their specific IO functions
   including the print functions. *)

(* You can easily put ExnM here and the code would stay unchanged*)
module M = Idl.IdM
module MyIdl = Idl.Make(M)

(* By passing in different modules to the `MyAPI` functor above, we can
   generate Client and Server modules. *)
module Client = MyAPI(MyIdl.GenClient ())
module Server = MyAPI(MyIdl.GenServer ())

let _ =
  (* The Client module generated above makes use of the Result.result type,
     and hence it is convenient to use the Monadic `bind` and `return`
     functions in Result.R *)
  let open MyIdl in

  (* The server is used by associating the RPC declarations with their
     implementations. The return type is expected to be Result.result, hence the
     use of `ok` here. *)
  Server.api1 (fun i s ->
    Printf.printf "Received '%d' and '%s': returning '%b'\n" i s true;
    ErrM.return true
  );

  Server.api2 (fun s ->
    Printf.printf "Received '%s': returning '%d'\n%!" s 56;
    ErrM.return 56
  );

  (* The Server module has a 'server' function that can be used to service RPC
     requests by passing the funcs value created above. *)
  let rpc_fn = server Server.implementation in

  (* ... *)

  let open ErrM in
  (* ... *)
  return (Result.Ok ())
*)

(* TODO: Example2 *)


(* let () =
 *   print_string "Hello, World!\n" *)
