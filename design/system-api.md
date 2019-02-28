System API
==========

This documents describes the system API provided to WebAssembly module by the
DFINITY system, as expected by the ActorScript runtime system.

Motivation, history and related documents
-----------------------------------------

It thus grew out and subsumes the document “[DFINITY System API (System
Calls)]”, making changes and simplifications based on our experience building
the ActorScrpit compiler against that interface.

This document includes the interfaces proposed by and required for the
“[Bidirectional Asynchronous Messaging]”, but does not define the semantics of
messaging (e.g. retry mechanisms, delivery guarantees).

This document also assumes a binary representation of the “[Dfinity IDL]”,
again without specifying or depending on the details of that.

[DFINITY System API (System Calls)]: https://dfinity.atlassian.net/wiki/spaces/M1/pages/80773443/Execution+M1+Design+Document#ExecutionM1DesignDocument-DFINITYSystemAPI(SystemCalls)
[Bidirectional Asynchronous Messaging]: https://github.com/dfinity-lab/discussion/issues/64
[Dfinity IDL]: https://github.com/dfinity-lab/discussion/issues/66

High-level ideas
----------------

One goal of this design, compared to the previous system API, is to avoid the
need for ephemeral references, such as `databuf` and `elembuf`. It turned out
that managing their lifetimes was an unnecessary complication, and required
additional system calls (create the reference, and then use it, and eventually
free it). Instead, this API is designed to move data in and out of the
WebAssembly heap and table directly when used. The concept of internalizing and
externalizing of references is avoided, as far as possible.

This design no longer optimizes to pass small data (e.g. 32-bit numbers)
directly using the WebAssembly types. The idea is that even that data will be
transmitted in binary form, according to the IDL, on the network or the block,
and there is not much to be gained by decoding that in the operating system.
This also makes the API more uniform (all message entry points have the same
WebAssembly type, there is no need to use type-overloaded imports, for
example).

The current state of this design is aimed at the feature set of the preview
release, and thus excludes features like
 * dynamic actor creation and deletion,
 * on-chain messaging,
 * reference passing,
 * access to time,
 * access to randomness,
 * payment and others.


It is, however, designed to be extended with these feature, and occasionaly we
note how that extension would look like.

WebAssembly Host references
---------------------------

This design mentions custom references types, such as `msgref`. If the
“[Reference Types for WebAssembly]” proposal is implemented, these would be
such opaque host references of type `anyref`, or a custom reference type if
type import is implemented; until then, they are represented by a `i32`. The
lifespan of such references is noted below.

[Reference Types for WebAssembly]: https://github.com/WebAssembly/reference-types/blob/master/proposals/reference-types/Overview.md

Accepting messages
------------------

To define a method of name `name`, a WebAssembly module exports a function with
that name and type `(msgref) -> ()`, where `msgref` is a host reference type,
as introduced in the [Bidirectional Asynchronous Messaging] proposal. We call
this the *message entry point*.

The proivided `msgref` is valid only during the execution of this method.

*Alternative design:* A single static entry-point `receive` that can query the requested
`name` via the `msgref`, and do dynamic dispatch.

Reading message argument payload
--------------------------------

The system stores the data payload with the  `msgref`, and the WebAssembly
module can query it, and load it into its heap, using the following two
function calls:


 * To query the length:
   ```
   msg.arg_length : (msg : msgref) -> (i32)
   ```
   returns the size of the argument, in bytes.
 * To load the data:
   ```
   msg.arg_load : (msg : msgref, dst : i32, length : i32, offset : i32) -> ()
   ```
   loads `length` bytes, starting with byte `offset`, from the message argument
   into the WebAssembly heap starting at location `dst`.

   This traps if `offset+length` is greater than the size of the argument, or
   if `dst+length` exceeds the size of the WebAssembly heap.

The binary representation of the argument is defined by the [Dfinity IDL]
document. Possible guarantees about the well-formedness of the data are also
within the scope of the IDL specification.

*Motivation*: We would not pass the argument data directly with the message
entry point call, even if that were possible with WebAssembly. This allows the
WebAssembly module to do various checks (do I have enough space? does the
argument have the size I expect? Eventually: is the sender authenticated and is
enough payment provided?) before actually loading the data. By allowing partial
access the receiver can parse large amount of data in a streaming fashion.

*Future note* (relevant once we allow inter-canister messages and/or local
actors): The `msgref` is re-used when message sends return and callbacks are
executed. During these callbacks, the payload refers to the data sent to the
callback (if applicable); the payload of the original message is no longer
available, and the operating system may drop it as soon as the message entry
point returns.

*Future note* (relevant once messages carry more than data, e.g. references or
payments): Additional aspects of a message, e.g. sender, payment, etc. will be
accessed using additional methods, something along the lines of
```
msg.payment (msg : msgref) -> (i32)
msg.refs_length (msg : msgref) -> (i32)
msg.refs_load (msg : msgref, dst : i32, length : i32, offset : i32) -> ()  // loads reference into the table
```

*Alternative design*: If the IDL defines multiple arguments, then we extend
this API with `msg.arg_count : (msg : msgref) -> (i32)`, and both
`msg.arg_length` and `msg.arg_load` gain an extra argument `arg_num : i32`.


Responding to a message
-----------------------

As specified in the [Bidirectional Asynchronous Messaging] proposal, simply
returning from the message entry point is not enough to indicate success. The
entry point must invoke the `msg.reply` function:

```
msg.reply : (msg : msgref, offset : i32, length : i32) -> ()
```

This system call loads `length` bytes at position `offset` of the WebAssembly
heap, to be used as the result value upon successful competion of the entry
point.

It traps if `length+offset` exceeds the size of the WebAssembly heap. I also
traps if it is called more than once on a given `msgref`, or if `msg.reject`
has been called.

The binary representation of the argument is defined by the [Dfinity IDL]
document. It is up to the IDL specification to determine whether and when the
system would check the data for well-formedness.

The actual response is only sent once the message entry point returns normally
(without a trap).

*Motivation:* This call returns, and does not immediately send the response, to
allow the WebAssembly module to do possible clean-up (e.g. to free the heap
allocation where the respose was assembled), or to configure additional
parameters of the response (such as responding with references or include
payment in the response).

*Alternative design*: For large responses, we may want to be able to move them
to the hypervisor in chunks. We can easily provide an optional interface for
that later.

*Future note* (once we can respond with references): To send references, an
additional system call
```
msg.refs_reply : (msg msgref, offset : i32, length : i32) -> ()
```
is provided that reads references from the WebAssembly module table.


Rejecting a message
-------------------

If the message entry point returns without calling `msg.reply`, this indicates
failure. The WebAssembly code may also fail more explicitly, by calling
```
msg.reject : (msg : msgref, errorcode : i32) -> ()
```

The meaning of the error codes is not specified in this proposal.

Like `msg.reply`, this function returns to the WebAssembly module and does not
immediatelly send a response. It traps if `msg.reject` or `msg.reply` has been
called before.

*Alternative design:* Additional methods that add more information to the
rejection (e.g. a log message) could be added.


Serializaing and deserializing the actor state
----------------------------------------------

The system expects WebAssembly modules to be able to serialize and
de-serialize their state. To that end, each module has to provide message entry
points for two messages with IDL signature
```
serialize : () -> t
deserialize  : t -> ()
```
where `t` is an IDL type chosen by the developer of the WebAssembly module.
Because these are, from the point of view of the program, just normal messages,
the mechanics above apply as well.

Upgrading is only allowed if the new version of the module specifies a type
`t_new` that is a supertype of `t_old`, as specified in section “Upgrading and
Subtyping” of the [DFINITY IDL]. This ensures that the new version can deal
with the data from the old version.
