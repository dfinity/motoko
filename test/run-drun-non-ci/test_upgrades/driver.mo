import Prim "mo:⛔";
import { ic } "mo:ic";

import (v0 : Blob) = "blob:file:wasms/v0.wasm";
import (v1 : Blob) = "blob:file:wasms/v1.wasm";
import (v2 : Blob) = "blob:file:wasms/v2.wasm";
import (v3 : Blob) = "blob:file:wasms/v3.wasm";
import (v4 : Blob) = "blob:file:wasms/v4.wasm";
import (v5 : Blob) = "blob:file:wasms/v5.wasm";
import (v6 : Blob) = "blob:file:wasms/v6.wasm";
import (v7 : Blob) = "blob:file:wasms/v7.wasm";
import (v8 : Blob) = "blob:file:wasms/v8.wasm";
import (v9 : Blob) = "blob:file:wasms/v9.wasm";
import (v10 : Blob) = "blob:file:wasms/v10.wasm";
import (v11 : Blob) = "blob:file:wasms/v11.wasm";
import (v12 : Blob) = "blob:file:wasms/v12.wasm";
import (v13 : Blob) = "blob:file:wasms/v13.wasm";
import (v14 : Blob) = "blob:file:wasms/v14.wasm";
import (v15 : Blob) = "blob:file:wasms/v15.wasm";
import (v16 : Blob) = "blob:file:wasms/v16.wasm";
import (v17 : Blob) = "blob:file:wasms/v17.wasm";
import (v18 : Blob) = "blob:file:wasms/v18.wasm";
import (v19 : Blob) = "blob:file:wasms/v19.wasm";
import (v20 : Blob) = "blob:file:wasms/v20.wasm";

persistent actor {

  public func install_all() : async () {
    let controller = Prim.getSelfPrincipal<system>();
    Prim.debugPrint(debug_show (controller));
    let result = await ic.create_canister({
      settings = ?{
        controllers = ?[controller];
        compute_allocation = null;
        memory_allocation = null;
        freezing_threshold = null;
        reserved_cycles_limit = null;
        log_visibility = null;
        wasm_memory_limit = null;
        wasm_memory_threshold = null;
      };
      sender_canister_version = null;
    });

    let canister_id = result.canister_id;
    Prim.debugPrint(debug_show (canister_id));
    let res = await ic.install_code({
      mode = #install;
      canister_id = canister_id;
      wasm_module = v0;
      arg = "";
      sender_canister_version = null;
    });

    Prim.debugPrint(debug_show (res));

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null; // or ?true or ?false
        }
      );
      canister_id = canister_id;
      wasm_module = v1;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null; // or ?true or ?false
        }
      );
      canister_id = canister_id;
      wasm_module = v2;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null; // or ?true or ?false
        }
      );
      canister_id = canister_id;
      wasm_module = v3;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v4;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v5;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v6;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v7;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v8;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v9;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v10;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v11;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v12;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v13;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v14;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v15;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v16;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v17;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v18;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v19;
      arg = "";
      sender_canister_version = null;
    });

    await ic.install_code({
      mode = #upgrade(
        ?{
          wasm_memory_persistence = ?#keep;
          skip_pre_upgrade = null;
        }
      );
      canister_id = canister_id;
      wasm_module = v20;
      arg = "";
      sender_canister_version = null;
    });

  };
};
