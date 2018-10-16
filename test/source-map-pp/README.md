This script is a filter for error messags.

It looks for lines of the form

    foo.wasm:1.2-3.4:

if checks if there is a `foo.map`. If it is, it looks up this position in the
map and replaces it in the error message by the original location.

## Installing

Install using `npm install` or `nix-env -i -f . -A package`

## Creating the nix expressions

The nix derivations were created by running

    node2nix --nodejs-10
