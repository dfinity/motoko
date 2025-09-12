module {
    type OldActor = {
        required: persistent () -> ();
        optional: persistent () -> ();
    };

    type NewActor = {
        retained: persistent () -> ();
    };

    public func run(old: OldActor): NewActor {
        { retained = old.required }
    }
}
