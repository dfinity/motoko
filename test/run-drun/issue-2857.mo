// import Text "mo:base/Text";
// import Array "mo:base/Array";
// import Debug "mo:base/Debug";


actor Test {
module TestModule {
     public type Result = {
        #OK;
        #Err : Text;
    };

     public type Errs = {
        var errors: [Text];
        add: (Text, Result) -> ();
    };

    public type Entity<T> = {
        validate : T -> Errs;
    };

    public class entityErrors() : Errs {
        public var errors : [Text] = [];
        public func add(field : Text, res : Result) {
            errors := errors; //Array.append<Text>(errors, ["newError"]);
        };

    };

};

    type Result = TestModule.Result;
    type Entity<T> = TestModule.Entity<T>;
     type Errs = TestModule.Errs;
    let entityErrors = TestModule.entityErrors;

    public type Parent = {
        name: Text;
        description: Text;
        extraValue : ?Text;
    };

    public type Child = {
        extraValue : ?Text;
    };


    // func f (e: Parent: Child ) : () {       };

    func isItABugOrAFeature () : Entity<Parent> {
        {
        validate = func (e: Parent: Child ) : Errs {
            let errs = entityErrors();
            errs.add("field", #OK);

            errs;
        };
        };
    };

    public func test() : async () {
        let t : Parent = {
            name = "test name";
            description = "test description";
            extraValue = null;
        };
        let v = isItABugOrAFeature().validate(t);
    };
};
