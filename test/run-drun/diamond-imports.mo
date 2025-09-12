//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY

// Diamond import constellation:
//
//           Actor 
//             | 
//    ------------------- import
//    | A               | B
//    |                 | 
// module1.mo        module2.mo
//    |                 | 
//    | Shared          | Shared
//    ------------------- import
//             |
//      shared-module.mo

// On diamond imports, the shared module gets the first fully 
// qualified name by depth-first traversal of the identified imports.
// i.e. here `A.Shared.testFunc` for `testFunc` in `shared-module.mo`.

import A "diamond-imports/module1";
import B "diamond-imports/module2";

persistent actor {
    let f0 = A.testFunc;
    f0();
    let f1 = A.getShared();
    f1();

    let f2 = B.testFunc;
    f2();
    let f3 = A.getShared();
    f3();
};

//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP comp-ref
//CALL upgrade ""
