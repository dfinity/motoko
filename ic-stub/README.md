The IC Stub
===========

The IC stub is a partial implementation of the public interface of the DFINITY
Internet Computer, as specified in
https://docs.dfinity.systems/spec/public/
with the primary goal of providing a mock environment to test the output of the
Motoko compiler.

It currently provides one binary, `ic-stub-run`, that allows you to script the
execution of a single canister.

Status
------

This is neither complete nor authorative. Since this is primarily meant for
testing the output of Motoko, code paths not exercised by Motoko may not be
present; in particular, error handling and input validation is incomplete.

Installation of `ic-stub-run`
-----------------------------

From the top-level repository directory, run


    nix-env -i -f . -A ic-stub


Developing on ic-stub
---------------------

Running `nix-shell` in the `ic-stub/` directory should give you an environment
that allows you to build the project using `cabal new-build`.
