import Prim "mo:⛔";

persistent actor Self {
  type EnvVar = { name : Text; value : Text };

  let ic = actor "aaaaa-aa" : actor {
    canister_status : shared {
      canister_id : Principal;
    } -> async {
      settings : {
        environment_variables : [EnvVar];
      };
    };
    update_settings : shared {
      canister_id : Principal;
      settings : {
        environment_variables : ?[EnvVar];
      };
    } -> async ();
  };

  func getEnvVars() : async* [EnvVar] {
    (await ic.canister_status({ canister_id = Prim.principalOfActor(Self) })).settings.environment_variables;
  };

  func setEnvVars(vars : [EnvVar]) : async* () {
    await ic.update_settings({
      canister_id = Prim.principalOfActor(Self);
      settings = { environment_variables = ?vars };
    });
    let currentVars = await* getEnvVars();
    if (currentVars != vars) {
      Prim.debugPrint("Unexpected environment variables: " # debug_show currentVars);
      assert false;
    };
  };

  public func test() : async () {
    await* setEnvVars([
      {
        name = "TEST_ENV_VAR_NAME";
        value = "TEST_ENV_VAR_VALUE";
      },
      {
        name = "key";
        value = "value";
      },
    ]);
    var names = Prim.envVarNames();
    Prim.debugPrint(debug_show names);
    assert names == ["TEST_ENV_VAR_NAME", "key"];
    assert Prim.envVar("TEST_ENV_VAR_NAME") == ?"TEST_ENV_VAR_VALUE";
    assert Prim.envVar("OTHER_ENV_VAR_NAME") == null;
  };

};

//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL ingress test "DIDL\x00\x00"
