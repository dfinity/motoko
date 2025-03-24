use motoko_rts::stabilization::layout::StableObjectKind;

pub unsafe fn test() {
    println!("  Testing layout ...");

    test_stable_tags();
}

fn test_stable_tags() {
    for object_kind in [
        StableObjectKind::ArrayImmutable,
        StableObjectKind::ArrayMutable,
        StableObjectKind::ArrayTuple,
        StableObjectKind::ArraySharedFunction,
        StableObjectKind::MutBox,
        StableObjectKind::Object,
        StableObjectKind::BlobBytes,
        StableObjectKind::BlobText,
        StableObjectKind::BlobPrincipal,
        StableObjectKind::BlobActor,
        StableObjectKind::Bits64Unsigned,
        StableObjectKind::Bits64Signed,
        StableObjectKind::Bits64Float,
        StableObjectKind::Region,
        StableObjectKind::Variant,
        StableObjectKind::Concat,
        StableObjectKind::BigInt,
        StableObjectKind::Some,
    ] {
        assert!(object_kind.encode().decode() == object_kind);
    }
}
