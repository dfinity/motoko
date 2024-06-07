use motoko_rts::stabilization::layout::StableObjectKind;

pub unsafe fn test() {
    println!("  Testing layout ...");

    test_stable_tags();
}

fn test_stable_tags() {
    for object_kind in [
        StableObjectKind::Array,
        StableObjectKind::MutBox,
        StableObjectKind::Object,
        StableObjectKind::Blob,
        StableObjectKind::Bits64,
        StableObjectKind::Region,
        StableObjectKind::Variant,
        StableObjectKind::Concat,
        StableObjectKind::BigInt,
        StableObjectKind::ObjInd,
        StableObjectKind::Some,
    ] {
        assert!(object_kind.encode().decode() == object_kind);
    }
}
