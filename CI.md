Motoko CI and CD setup
======================

This file gives a comprehensive overview of our CI and CD setup. Target
audience are developers working on Motoko (to know what services and behaviours
they can expect), and engineers who support the setup.

This document distinguishes use-cases (requirements, goals) from
implementations, to allow evaluating alternative implementations.

The implementation is currently a careful choreography between Github, Github
Actions, Hydra, the nix cache and mergify.

Knowing if it is broken
-----------------------

**Use-case:**
Everything is built and tested upon every push to a branch of the repository,
and the resulting status is visible (at derivation granularity) to developers.

**Implementation:**
All pushes to `master`, as well as to all branches with open PRs cause hydra to
build the jobs described in `ci.nix` resp. `ci-pr.nix`.

The build status is visible via the Github status (coarsly: only evaluation,
`all-systems-go` and `all-jobs`) and via hydra directly (full per-job detail
including logs).

This includes linux and darwin builds.

Preventing `master` from breaking
---------------------------------

**Use-case:**
A PR that breaks requires jobs (`all-systems-go`) cannot be merged into `master`.

**Implementation:**
Github branch protection is enabled for `master`, and requires the
`all-systems-go` job from hydra to succeed.

Require a second pair of eyeballs
---------------------------------

**Use-case:**
A PR needs to be approved by any other developer in order to be merged (with
the exceptions listed below).

**Implementation:**
Github branch protection requires a review.

Warm cache
----------

**Use-case:**
Developers can rely on all build artifacts (espcially, but not exclusively, the
dependencies of the nix shell) being present in a nix cache.

**Implementation:**
Hydra pushes all builds to the internal nix cache.

Push releases
-------------

**Use-case:**
Tagged versions cause a tarball with a Motoko release to be pushed to https://download.dfinity.systems/

**Implementation:**
The jobs in `release.nix` detect tagged versions. In these cases, the `release`
jobs defined in `ci.nix`, with help from `nix/publish.nix`, will cause our
Hydra instance to publish files via S3.

Automatically merge when ready
------------------------------

**Use-case:**
Developers can indicate that a PR should be merged as soon as all requirements
pass, using the PR description as the commit message.

This can be done before approvals are in and/or CI has turned green, and will
reliably be acted on once requirements are fulfilled.

**Implementation:**
Mergify reacts to the `automerge-squash` label. Once approved and CI passes, it
merges master into the branch (using normal merge, which is important for
stashed PRs) and then squash-merges the PR.

Render and show generated base docs
-----------------------------------
**Use-case:**
To be able to see and share the effects on the generated documentation for the base library, caused by a change to `mo-doc`, without needing the reviewer to generate the documentation locally.

**Implementation:**
Hydra hosts the build product of the `base-doc` CI job. This can be found via the Hydra job status page, and the there is a stable link for the latest build of `master` and of each PR.

Render and show slides
----------------------
**Use-case:**
To be able to see and share the “overview slides”, the rendered version should be hosted somewhere.

**Implementation:**
Hydra hosts the build product of the `overview-slides` CI job. This can be found via the Hydra job status page, and the there is a stable link for the latest build of `master` and of each PR.


Performance changes are known
-----------------------------

**Use-case:**
For every PR, the developer is told about performance changes relative to the
merge point, via an continuously updated comment on the PR.

**Implementation:**
 * Hydra calculates the correct merge base using `git-merge-base` (_not_ the
   latest version of the target branch) and passes a checkout of that revision
   as `src.mergeBase` to `ci-pr.nix`.
 * The job `perf-delta` compares metrics and generates a report.
 * The hydra Github commenter plugin updates the comment upon every new push to
   the PR.

Dependencies are updated automatically
--------------------------------------

**Use-case:**
Several dependencies, as pinned by `nix/souces.json`, should be updated without
human intervention. Some dependencies are updated daily, others weekly. For
some dependency, it should only be _tested_ if it builds, but not merged.

**Implementation:**
 * Multiple files (with different settings) in `.github/workflows/` use
   [niv-updater-action](https://github.com/knl/niv-updater-action) to create
   pull requests with the version bumps, as `dfinity-bot`, setting
   `automerge-squash` or `autoclose`.
 * Mergify automatically approves PRs from `dfinity-bot`.
 * Once CI passes, mergify merges or closes PRs, as per label.

Updates to the Changelog require no review
------------------------------------------

**Use-case:**
To make releasing releases frictionless (see section in `Building.md`), PRs
that only update `Changelog.md` do not require a human approver.

**Implementation:**
Mergify approves PRs that only change the `Changelog.md` file.
