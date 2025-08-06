module {
    type OldActor = {
        required: stable () -> ();
        optional: stable () -> ();
    };

    type NewActor = {
        retained: stable () -> ();
    };

    public func run(old: OldActor): NewActor {
        old.required();
//	old.optional();
        { retained = old.required }
    }
}
