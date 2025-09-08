import Prim "mo:⛔";

persistent actor Self {
  type EnvVar = { name : Text; value : Text };

  func setEnvVars(env_vars : [EnvVar]) : async () {
    let ic = actor "aaaaa-aa" : actor {
      update_settings : shared {
        canister_id : Principal;
        settings : {
          environment_variables : ?[EnvVar];
        };
      } -> async ();
    };
    await ic.update_settings({
      canister_id = Prim.principalOfActor(Self);
      settings = { environment_variables = ?env_vars };
    });
  };

  public func test() : async () {
    await setEnvVars([{
      name = "TEST_ENV_VAR_NAME";
      value = "TEST_ENV_VAR_VALUE";
    }]);
    var names = Prim.envVarNames();
    Prim.debugPrint(debug_show names);
    assert names == ["TEST_ENV_VAR_NAME"];
    assert Prim.envVar("TEST_ENV_VAR_NAME") == ?"TEST_ENV_VAR_VALUE";
    assert Prim.envVar("OTHER_ENV_VAR_NAME") == null;
  };

};

//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL ingress test "DIDL\x00\x00"
