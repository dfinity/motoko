## Nix setup

The Motoko build system relies on [Nix](https://nixos.org/) to manage
dependencies, drive the build and run the test suite. You should install nix by
running, as a normal user with `sudo` permissions,
```
curl -L https://nixos.org/nix/install | sh
```

You should also enable a nix cache to get all dependencies pre-built.
```
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use ic-hs-test
```
Technically, this is optional, but without this you will build lots of build
dependencies manually, which takes several hours.


## Installation using Nix

If you want just to _use_ `moc`, you can install the `moc` binary into your nix
environment with
```
$ nix-env -i -f . -A moc
```

## Development using Nix

To enter a shell with the necessary dependencies available, use
```
$ nix-shell
```

Within this shell you can run
 * `make` in `src/` to build all binaries,
 * `make moc` in `src/` to build just the `moc` binary,
 * `make DUNE_OPTS=--watch moc` to keep rebuilding as source files are changing
 * `make` in `rts/` to build the Motoko runtime
 * `make` in `test/` to run the test suite.

This invokes `dune` under the hood, which will, as a side effect, also create
`.merlin` files for integration with Merlin, the Ocaml Language Server

## Replicating CI locally

A good way to check that everything is fine, i.e. if this will pass CI, is to run
```
$ nix-build --no-out-link
```

For more details on our CI and CI setup, see `CI.md`.


## Making releases

We make frequent releases, at least weekly. The steps to make a release (say, version 0.6.16) are:

 * Make sure that the top section of `Changelog.md` has a title like

        ## 0.6.16 (2021-12-01)

   with today’s date.

 * Define a shell variable `export MOC_MINOR=16`

 * Look at `git log --first-parent 0.6.$(expr $MOC_MINOR - 1)..HEAD` and check
   that everything relevant is mentioned in the changelog section, and possibly
   clean it up a bit, curating the information for the target audience.

 * `git commit -am "Releasing 0.6.$MOC_MINOR"`
 * Create a PR from this commit, and label it `automerge-squash`.  Mergify will
   merge it into master without additional approval, within 2 or 3 minutes.
 * `git switch master; git pull`. The release commit should be your `HEAD`
 * `git tag 0.6.$MOC_MINOR -m "Motoko 0.6.$MOC_MINOR"`
 * `git branch -f release 0.6.$MOC_MINOR`
 * `git push origin release 0.6.$MOC_MINOR`

The `release` branch should thus always reference the latest release commit.

Pushing the tag should cause GitHub Actions to create a “Release” on the github
project. This will fail if the changelog is not in order (in this case, fix and
force-push the tag).  It will also fail if the nix cache did not yet contain
the build artifacts for this revision. In this case, restart the GitHub Action
on GitHub’s UI.

After releasing the compiler you can update `motoko-base`'s `master`
branch to the `next-moc` branch.

* Wait ca. 5min after releasing to give the CI/CD pipeline time to upload the release artifacts
* Change into `motoko-base`
* `git switch next-moc; git pull`
* `git switch -c $USER/update-moc-0.6.$MOC_MINOR`
* Update the `moc_version` env variable in `.github/workflows/{ci, package-set}.yml`
  to the new released version:
  `perl -pi -e "s/moc_version: \"0\.6\.\\d+\"/moc_version: \"0.6.$MOC_MINOR\"/g" .github/workflows/ci.yml .github/workflows/package-set.yml`
* `git add .github/ && git commit -m "Motoko 0.6.$MOC_MINOR"`
* You can `git push` now

Make a PR off of that branch and merge it using a _normal merge_ (not
squash merge) once CI passes. It will eventually be imported into this
repo by a scheduled `niv-updater-action`.

## Profile the compiler

(This section is currently defunct, and needs to be update to work with the dune
build system.)

1. Build with profiling within nix-shell (TODO: How to do with dune)
   ```
   make -C src clean
   make BUILD=p.native -C src moc
   ```
2. Run `moc` as normal, e.g.
   ```
   moc -g -c foo.mo -o foo.wasm
   ```
   this should dump a `gmon.out` file in the current directory.
3. Create the report, e.g. using
   ```
   gprof --graph src/moc
   ```
   (Note that you have to _run_ this in the directory with `gmon.out`, but
   _pass_ it the path to the binary.)


## Benchmarking the RTS

Specifically some advanced techniques to obtain performance deltas for the
GC can be found in `rts/Benchmarking.md`.

## Updating Haskell Packages

When the `.cabal` file of a Haskell package is changed you need to make sure the
corresponding `.nix` file (stored in `nix/generated/`) is kept in sync with it. These files are automatically generated; run
```
nix-shell nix/generate.nix
```
to update.

Don't worry if you forget to update the `default.nix` file, the CI job
`check-generated` checks if these files are in sync and fail with a diff if
they aren't.
