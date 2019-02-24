# We need to set test-dvm to false because hydra has
# no access to the `dev` repo. This can go away once we join
# the monorepo.
import ./default.nix { test-dvm = false; }
