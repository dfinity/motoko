ignore {
actor a { public func foo(){ debugPrint("Hi\n"); }; a.foo() };
};
ignore {
let a = actor { public func foo(){ debugPrint("Ho\n"); }; a.foo() };
};
