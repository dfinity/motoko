
actor Test {


    public type Parent = {
        name: Text;
        extraValue : Bool;
    };

    public type Child = {
        extraValue : Bool;
    };


    // func f (e: Parent: Child ) : () {       }; // rejected

    // let reject  = (func f (e: Parent: Child ) : () {       }) : Parent -> () ;
    let accept  = (func (e: Parent: Child ) : () {       }) : Parent -> () ; // accepted! WTF?


    // let reject = (do {func foo (e: Parent: Child ) : () {       };foo}) : Parent -> () ; // rejected

/*
    func isItABugOrAFeatureRejected () :
      Parent -> ()
    {
       func f (e: Parent: Child) : () { ignore e.name } // rejected!
    };
*/
    func isItABugOrAFeature () :
      Parent -> ()
    {
       func (e: Parent: Child) : () { ignore e.name } // accepted! WTF?
    };


    public func test() : async () {

        let t = {
            name = "foo";
            extraValue = true;
        };
        let v = isItABugOrAFeature()(t);
    };
};
