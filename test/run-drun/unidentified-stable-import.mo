//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import { TestClass; TestObject } "stable-function-scopes/module1";

actor {
    // Functions and objects referred to unidentified module are not stable.
    stable let f0 = TestClass().testFunc;
    f0();

    stable let f1 = TestObject.testFunc;
    f1();
};
