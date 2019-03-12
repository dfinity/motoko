Produce Exchange Example
----------------------------

The produce exchange (PE) is a canonical example Dapp, illustrating
the DFINITY Dapp design process on a realistic marketplace-like
application.  The PE concept began as a [two page Google drive
document](https://docs.google.com/document/d/1AxpcuFH-x_0ZSa32DfM_BCYnGxCS37ETPNWE4BXDNdo/edit)
giving its functional specifications.

The design of PE now evolves in three places:

 1. [Documentation on confluence, under the ActorScript space](https://dfinity.atlassian.net/wiki/spaces/AST/pages/104401122/Example+Dapp+Produce+Exchange).

 2. The SDK team's documentation:

  i. [The design document, under the SDK
     space](https://dfinity.atlassian.net/wiki/x/MwD2Bg).

  ii. [The requirements document for the MVP
      Design](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements)

 3. This example folder in the ActorScript repo.  
 which is implementing PE as a way to push the development of the
 ActorScript language, its standard library, and elsewhere, the
 ambient DFINITY system that runs ActorScript canisters.

To do list
-----------

This example is a work in progress.  

It will be finished (and merged to `master`) when the following are in
a stable state, and working locally, as locally-running ActorScript:

  1. **the exchange’s interface definition**, as an ActorScript actor.  
   (See
   [`stdlib/examples/produceExchange.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produceExchange.as),
   for this item, and items 2 and 3).

  2.  **the behavior of the exchange**, as that actor’s prototype
      implementation.

  3. **the internal data model representation**, based on the
     ActorScript language and standard library, used internally to the
     actor, to implement the specification behavior.

  4. **tests** of the above, using local execution and
     ActorScript-based test cases.
