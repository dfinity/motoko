import ActorSpec "../src/ActorSpec";

type Group = ActorSpec.Group;

let assertTrue = ActorSpec.assertTrue;
let describe = ActorSpec.describe;
let it = ActorSpec.it;
let pending = ActorSpec.pending;
let run = ActorSpec.run;

run([
  describe("ActorSpec", [
    describe("passing specs", [
      it("should pass", {
        assertTrue(true);
      }),
    ]),

    describe("failing specs", [
      it("should fail", {
        assertTrue(false);
      }),
    ]),

    describe("pending specs", [
      pending("should be marked as pending"),
    ]),

    describe("nested groups", [
      it("should have the correct indentation", {
        assertTrue(true);
      }),
    ]),
  ]),
]);
