//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
//Multiple imports of same library
import A "stable-function-scopes/module1";
import B "stable-function-scopes/module1";

actor {
    // Functions and objects in A and B are not stable.
    stable let f0 = A.TestClass().testFunc;
    f0();

    stable let f1 = A.TestObject.testFunc;
    f1();

    stable let f2 = B.TestClass().testFunc;
    f2();

    stable let f3 = B.TestObject.testFunc;
    f3();
};
