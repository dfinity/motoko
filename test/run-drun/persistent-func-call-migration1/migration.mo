module {
    type OldActor = {
        required: persistent () -> ();
    };

    type NewActor = {
        retained: persistent () -> ();
    };

    public func run(old: OldActor): NewActor {
        old.required();
        { retained = old.required }
    }
}
