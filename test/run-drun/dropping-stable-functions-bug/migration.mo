module {
    type OldActor = {
        required: stable required () -> ();
        optional: stable optional () -> ();
    };

    type NewActor = {
        retained: stable required () -> ();
    };

    public func run(old: OldActor): NewActor {
        old.required();
//	old.optional();
        { retained = old.required }
    }
}
