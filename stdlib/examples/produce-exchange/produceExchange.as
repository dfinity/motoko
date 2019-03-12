/**

Matt-Says: Let's use markdown in the longer comments, in anticipation
of a documentation tool for converting ActorScript into Markdown
files.  I'll use an extra `*` in the opening comment when I expect the
comment to be processed as markdown.

Produce Exchange Dapp
=====================

 Start here:
 - Detailed examples: https://dfinity.atlassian.net/wiki/x/joXUBg
 - More background: https://dfinity.atlassian.net/wiki/x/4gg5Bg


Produce Exchange Standards (PES)
=================================

The Produce Exchange is a DFINITY canister whose implementation
defines a set of _standards_ that to which we refer to collectively as
the Produce Exchange Standards, or PES.


PES, defined formally:
-----------------------

We break this definition into several files, two of which contain content that gives the
PES, formally.

```
import types.as
import actor.as
```

These files make the PES formal, to the same degree that ActorScript
has a formal semantics of its own, in terms of DFINITY's semantics,
etc.

Additionally, the `model.as` file defines types used to implement the
specification behavior given in `actor.as`; this file is not part of
the PES.

The file `types.as` defines ActorScript data types that are included
in the PES.  These will appear in the messages to and from the produce
exchange.  The actor class itself (see `actor.as`) gives the interface
for the PE service, is also part of the formal PES.  The _behavior_ of
this actor's implementation defines the _semantic_ aspects of the PES
standard.

The implementation details of this actor lie outside the PES but are
also present in the file `actor.as`, in terms of types defined in
`model.as`.

The actor interface boundary only uses types from `types.as`, and none
from `model.as`; the implementation details of this actor behavior are
subject to change over time (including changes to `model.as`),
independently of the standards' own evolution.  We include the full
implementation details here because the associated behavior is needed
to define the semantics of the PES, as explained above.


PES evolution via canister upgrade
-----------------------------------

The PES evolves according to the "central authority" (cf PE spec
document), who we identify as the github repo and open source
developer community that surrounds this implementation file.

Updating the types in the PES requires changing this file, and
performing a canister upgrade on the running system.  Similarly, to
evolve the behavioral definition of PES, the implementation of this
actor will change, and will require a canister upgrade.

*/

// xxx regression tests go somewhere
