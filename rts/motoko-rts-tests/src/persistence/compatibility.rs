use motoko_rts::memory::{alloc_blob, Memory};
use motoko_rts::persistence::compatibility::{
    memory_compatible, ACTOR_ENCODING_TAG, ANY_ENCODING_TAG, ARRAY_ENCODING_TAG,
    MUTABLE_ENCODING_TAG, NONE_ENCODING_TAG, OBJECT_ENCODING_TAG, OPTION_ENCODING_TAG,
    TUPLE_ENCODING_TAG, VARIANT_ENCODING_TAG,
};
use motoko_rts::types::{Bytes, Value};
use std::hash::Hasher;
use std::{collections::hash_map::DefaultHasher, hash::Hash};

use crate::memory::{initialize_test_memory, reset_test_memory, TestMemory};

struct BinaryData {
    byte_sequence: Vec<u8>,
}

impl BinaryData {
    fn new() -> BinaryData {
        BinaryData {
            byte_sequence: vec![],
        }
    }

    fn write_i32(&mut self, value: i32) {
        for byte in value.to_le_bytes() {
            self.byte_sequence.push(byte);
        }
    }

    fn write_hash(&mut self, value: &str) {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        self.write_i32(hasher.finish() as i32);
    }

    unsafe fn make_blob<M: Memory>(&self, mem: &mut M) -> Value {
        let length = Bytes(self.byte_sequence.len() as u32);
        let value = alloc_blob(mem, length);
        let target = value.as_blob_mut();

        for index in 0..length.as_usize() {
            let byte = target.payload_addr().add(index);
            *byte = *self.byte_sequence.get(index).unwrap();
        }
        value
    }
}

#[derive(Clone)]
enum Type {
    Object(FieldList),
    Mutable(TypeReference),
    Option(TypeReference),
    Array(TypeReference),
    Tuple(TypeList),
    Variant(FieldList),
    None,
    Any,
    Actor(FieldList),
}

impl Type {
    fn serialize(&self, output: &mut BinaryData) {
        match &self {
            Self::Object(field_list) => {
                output.write_i32(OBJECT_ENCODING_TAG);
                field_list.serialize(output);
            }
            Self::Mutable(type_reference) => {
                output.write_i32(MUTABLE_ENCODING_TAG);
                output.write_i32(type_reference.index);
            }
            Self::Option(type_reference) => {
                output.write_i32(OPTION_ENCODING_TAG);
                output.write_i32(type_reference.index);
            }
            Self::Array(type_reference) => {
                output.write_i32(ARRAY_ENCODING_TAG);
                output.write_i32(type_reference.index);
            }
            Self::Tuple(type_list) => {
                output.write_i32(TUPLE_ENCODING_TAG);
                type_list.serialize(output);
            }
            Self::Variant(field_list) => {
                output.write_i32(VARIANT_ENCODING_TAG);
                field_list.serialize(output);
            }
            Self::None => {
                output.write_i32(NONE_ENCODING_TAG);
            }
            Self::Any => {
                output.write_i32(ANY_ENCODING_TAG);
            }
            Self::Actor(field_list) => {
                output.write_i32(ACTOR_ENCODING_TAG);
                field_list.serialize(output);
            }
        }
    }
}

#[derive(PartialEq, Clone)]
struct TypeReference {
    index: i32,
}

// Keep identical to `compatibility.rs` and `persistence.ml`.
const NULL_ENCODING_TAG: i32 = -1;
const BOOL_ENCODING_TAG: i32 = -2;
const NAT_ENCODING_TAG: i32 = -3;
const NAT8_ENCODING_TAG: i32 = -4;
const NAT16_ENCODING_TAG: i32 = -5;
const NAT32_ENCODING_TAG: i32 = -6;
const NAT64_ENCODING_TAG: i32 = -7;
const INT_ENCODING_TAG: i32 = -8;
const INT8_ENCODING_TAG: i32 = -9;
const INT16_ENCODING_TAG: i32 = -10;
const INT32_ENCODING_TAG: i32 = -11;
const INT64_ENCODING_TAG: i32 = -12;
const FLOAT_ENCODING_TAG: i32 = -13;
const CHAR_ENCODING_TAG: i32 = -14;
const TEXT_ENCODING_TAG: i32 = -15;
const BLOB_ENCODING_TAG: i32 = -16;
const PRINCIPAL_ENCODING_TAG: i32 = -16;

impl TypeReference {
    fn primitive(index: i32) -> Self {
        assert!(index < 0);
        TypeReference { index }
    }

    fn null() -> Self {
        Self::primitive(NULL_ENCODING_TAG)
    }

    fn bool() -> Self {
        Self::primitive(BOOL_ENCODING_TAG)
    }

    fn nat() -> Self {
        Self::primitive(NAT_ENCODING_TAG)
    }

    fn nat8() -> Self {
        Self::primitive(NAT8_ENCODING_TAG)
    }

    fn nat16() -> Self {
        Self::primitive(NAT16_ENCODING_TAG)
    }

    fn nat32() -> Self {
        Self::primitive(NAT32_ENCODING_TAG)
    }

    fn nat64() -> Self {
        Self::primitive(NAT64_ENCODING_TAG)
    }

    fn int() -> Self {
        Self::primitive(INT_ENCODING_TAG)
    }

    fn int8() -> Self {
        Self::primitive(INT8_ENCODING_TAG)
    }

    fn int16() -> Self {
        Self::primitive(INT16_ENCODING_TAG)
    }

    fn int32() -> Self {
        Self::primitive(INT32_ENCODING_TAG)
    }

    fn int64() -> Self {
        Self::primitive(INT64_ENCODING_TAG)
    }

    fn float() -> Self {
        Self::primitive(FLOAT_ENCODING_TAG)
    }

    fn char() -> Self {
        Self::primitive(CHAR_ENCODING_TAG)
    }

    fn text() -> Self {
        Self::primitive(TEXT_ENCODING_TAG)
    }

    fn blob() -> Self {
        Self::primitive(BLOB_ENCODING_TAG)
    }

    fn principal() -> Self {
        Self::primitive(PRINCIPAL_ENCODING_TAG)
    }
}

#[derive(Clone)]
struct Field {
    name: String,
    field_type: TypeReference,
}

impl Field {
    fn serialize(&self, output: &mut BinaryData) {
        output.write_hash(&self.name);
        output.write_i32(self.field_type.index);
    }
}

#[derive(Clone)]
struct FieldList {
    fields: Vec<Field>,
}

impl FieldList {
    fn serialize(&self, output: &mut BinaryData) {
        output.write_i32(self.fields.len() as i32);
        for field in &self.fields {
            field.serialize(output);
        }
    }
}

#[derive(Clone)]
struct TypeList {
    items: Vec<TypeReference>,
}

impl TypeList {
    fn serialize(&self, output: &mut BinaryData) {
        output.write_i32(self.items.len() as i32);
        for item in &self.items {
            output.write_i32(item.index);
        }
    }
}

struct TypeTable {
    types: Vec<Type>,
}

impl TypeTable {
    fn new(types: Vec<Type>) -> TypeTable {
        TypeTable { types }
    }

    fn serialize(&self) -> BinaryData {
        let mut output = BinaryData::new();
        output.write_i32(self.types.len() as i32);
        for current_type in &self.types {
            current_type.serialize(&mut output);
        }
        output
    }
}

unsafe fn build<M: Memory>(mem: &mut M, types: Vec<Type>) -> Value {
    TypeTable::new(types).serialize().make_blob(mem)
}

unsafe fn are_compatible<M: Memory>(
    mem: &mut M,
    old_types: Vec<Type>,
    new_types: Vec<Type>,
) -> bool {
    let old_type_blob = build(mem, old_types);
    let new_type_blob = build(mem, new_types);
    memory_compatible(mem, old_type_blob, new_type_blob)
}

unsafe fn is_compatible<M: Memory>(mem: &mut M, old_type: Type, new_type: Type) -> bool {
    are_compatible(mem, vec![old_type], vec![new_type])
}

pub unsafe fn test() {
    println!("  Testing memory compatibility...");
    let mut heap = initialize_test_memory();
    test_primitive_types(&mut heap);
    test_sucessful_cases(&mut heap);
    test_failing_cases(&mut heap);
    reset_test_memory();
}

unsafe fn test_primitive_types(heap: &mut TestMemory) {
    let mut is_compatible = |first: &TypeReference, second: &TypeReference| {
        let old_types = Type::Object(FieldList {
            fields: vec![Field {
                name: String::from("Field"),
                field_type: first.clone(),
            }],
        });
        let new_types = Type::Object(FieldList {
            fields: vec![Field {
                name: String::from("Field"),
                field_type: second.clone(),
            }],
        });
        is_compatible(heap, old_types, new_types)
    };
    let all_types = [
        TypeReference::null(),
        TypeReference::bool(),
        TypeReference::nat(),
        TypeReference::nat8(),
        TypeReference::nat16(),
        TypeReference::nat32(),
        TypeReference::nat64(),
        TypeReference::int(),
        TypeReference::int8(),
        TypeReference::int16(),
        TypeReference::int32(),
        TypeReference::int64(),
        TypeReference::float(),
        TypeReference::char(),
        TypeReference::text(),
        TypeReference::blob(),
        TypeReference::principal(),
    ];
    for first in &all_types {
        for second in &all_types {
            assert_eq!(is_compatible(first, second), first == second);
        }
    }
}

unsafe fn test_sucessful_cases(heap: &mut TestMemory) {
    test_empty_actor(heap);
    test_multiple_primitive_fields(heap);
    test_reordered_main_actor_fields(heap);
    test_removed_main_actor_fields(heap);
    test_mutable_fields(heap);
    test_added_main_actor_fields(heap);
    test_removed_object_fields(heap);
    test_removed_actor_fields(heap);
    test_direct_recursive_type(heap);
    test_indirect_recursive_type(heap);
    test_option_types(heap);
    test_flat_array_types(heap);
    test_nested_array_types(heap);
    test_recursive_array_types(heap);
    test_empty_tuple_types(heap);
    test_simple_tuple_types(heap);
    test_nested_tuple_types(heap);
    test_recursive_tuple_types(heap);
    test_reordered_variant_fields(heap);
    test_added_variant_fields(heap);
    test_any_to_any(heap);
    test_some_to_any(heap);
    test_none_to_none(heap);
}

unsafe fn test_empty_actor(heap: &mut TestMemory) {
    let old_type = Type::Object(FieldList { fields: vec![] });
    let new_type = Type::Object(FieldList { fields: vec![] });
    assert!(is_compatible(heap, old_type, new_type));
}

unsafe fn test_multiple_primitive_fields(heap: &mut TestMemory) {
    let fields = vec![
        Field {
            name: String::from("NullField"),
            field_type: TypeReference::null(),
        },
        Field {
            name: String::from("BoolField"),
            field_type: TypeReference::bool(),
        },
        Field {
            name: String::from("NatField"),
            field_type: TypeReference::nat(),
        },
        Field {
            name: String::from("Nat8Field"),
            field_type: TypeReference::nat8(),
        },
        Field {
            name: String::from("Nat16Field"),
            field_type: TypeReference::nat16(),
        },
        Field {
            name: String::from("Nat32Field"),
            field_type: TypeReference::nat32(),
        },
        Field {
            name: String::from("Nat64Field"),
            field_type: TypeReference::nat64(),
        },
        Field {
            name: String::from("IntField"),
            field_type: TypeReference::int(),
        },
        Field {
            name: String::from("Int8Field"),
            field_type: TypeReference::int8(),
        },
        Field {
            name: String::from("Int16Field"),
            field_type: TypeReference::int16(),
        },
        Field {
            name: String::from("Int32Field"),
            field_type: TypeReference::int32(),
        },
        Field {
            name: String::from("Int64Field"),
            field_type: TypeReference::int64(),
        },
        Field {
            name: String::from("FloatField"),
            field_type: TypeReference::float(),
        },
        Field {
            name: String::from("CharField"),
            field_type: TypeReference::char(),
        },
        Field {
            name: String::from("TextField"),
            field_type: TypeReference::text(),
        },
        Field {
            name: String::from("BlobField"),
            field_type: TypeReference::blob(),
        },
        Field {
            name: String::from("PrincipalField"),
            field_type: TypeReference::principal(),
        },
    ];

    let types = Type::Object(FieldList { fields });

    assert!(is_compatible(heap, types.clone(), types));
}

unsafe fn test_reordered_main_actor_fields(heap: &mut TestMemory) {
    let field1 = Field {
        name: String::from("Field1"),
        field_type: TypeReference::int(),
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: TypeReference::nat(),
    };

    let old_type = Type::Object(FieldList {
        fields: vec![field1.clone(), field2.clone()],
    });
    let new_type = Type::Object(FieldList {
        fields: vec![field2.clone(), field1.clone()],
    });

    assert!(is_compatible(heap, old_type, new_type));
}

unsafe fn test_removed_main_actor_fields(heap: &mut TestMemory) {
    let field1 = Field {
        name: String::from("Field1"),
        field_type: TypeReference::nat8(),
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: TypeReference::nat16(),
    };
    let field3 = Field {
        name: String::from("Field3"),
        field_type: TypeReference::nat32(),
    };

    let old_type = Type::Object(FieldList {
        fields: vec![field1.clone(), field2.clone(), field3.clone()],
    });
    let new_type = Type::Object(FieldList {
        fields: vec![field2.clone()],
    });

    assert!(is_compatible(heap, old_type, new_type));
}

unsafe fn test_added_main_actor_fields(heap: &mut TestMemory) {
    let field1 = Field {
        name: String::from("Field1"),
        field_type: TypeReference::bool(),
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: TypeReference::char(),
    };
    let field3 = Field {
        name: String::from("Field3"),
        field_type: TypeReference::text(),
    };

    let old_type = Type::Object(FieldList {
        fields: vec![field2.clone()],
    });
    let new_type = Type::Object(FieldList {
        fields: vec![field1.clone(), field2.clone(), field3.clone()],
    });

    assert!(is_compatible(heap, old_type, new_type));
}

unsafe fn test_mutable_fields(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("MainActorField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let mutable_type = Type::Mutable(TypeReference::nat());
    let old_types = vec![main_actor.clone(), mutable_type.clone()];
    let new_types = vec![main_actor.clone(), mutable_type.clone()];
    assert!(are_compatible(heap, old_types, new_types));
}

unsafe fn test_removed_object_fields(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ActorField"),
            field_type: TypeReference { index: 1 },
        }],
    });

    let field1 = Field {
        name: String::from("Field1"),
        field_type: TypeReference::int8(),
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: TypeReference::int16(),
    };
    let field3 = Field {
        name: String::from("Field3"),
        field_type: TypeReference::int32(),
    };

    let old_type = Type::Object(FieldList {
        fields: vec![field1.clone(), field2.clone(), field3.clone()],
    });
    let new_type = Type::Object(FieldList {
        fields: vec![field2.clone()],
    });

    let old_types = vec![main_actor.clone(), old_type];
    let new_types = vec![main_actor.clone(), new_type];
    assert!(are_compatible(heap, old_types, new_types));
}

unsafe fn test_removed_actor_fields(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ActorField"),
            field_type: TypeReference { index: 1 },
        }],
    });

    let field1 = Field {
        name: String::from("Field1"),
        field_type: TypeReference::int8(),
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: TypeReference::int16(),
    };
    let field3 = Field {
        name: String::from("Field3"),
        field_type: TypeReference::int32(),
    };

    let old_type = Type::Actor(FieldList {
        fields: vec![field1.clone(), field2.clone(), field3.clone()],
    });
    let new_type = Type::Actor(FieldList {
        fields: vec![field2.clone()],
    });

    let old_types = vec![main_actor.clone(), old_type];
    let new_types = vec![main_actor.clone(), new_type];
    assert!(are_compatible(heap, old_types, new_types));
}

unsafe fn test_direct_recursive_type(heap: &mut TestMemory) {
    let actor_field = Field {
        name: String::from("ActorField"),
        field_type: TypeReference { index: 1 },
    };
    let main_actor = Type::Object(FieldList {
        fields: vec![actor_field],
    });
    let recursive_field = Field {
        name: String::from("RecursiveField"),
        field_type: TypeReference { index: 1 },
    };
    let recursive_type = Type::Object(FieldList {
        fields: vec![recursive_field],
    });
    let types = vec![main_actor, recursive_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_indirect_recursive_type(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ActorField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let first_type = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("Field1"),
            field_type: TypeReference { index: 2 },
        }],
    });
    let second_type = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("Field2"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let types = vec![main_actor, first_type, second_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_option_types(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("OptionalField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let option_type = Type::Option(TypeReference::blob());
    let types = vec![main_actor, option_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_flat_array_types(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ArrayField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let array_type = Type::Array(TypeReference::nat());
    let types = vec![main_actor, array_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_nested_array_types(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ArrayField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let outer_array_type = Type::Array(TypeReference { index: 2 });
    let inner_array_type = Type::Array(TypeReference::nat());
    let types = vec![main_actor, outer_array_type, inner_array_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_recursive_array_types(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ArrayField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let recursive_array_type = Type::Array(TypeReference { index: 1 });
    let types = vec![main_actor, recursive_array_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_empty_tuple_types(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("TupleField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let unit_type = Type::Tuple(TypeList { items: vec![] });
    let types = vec![main_actor, unit_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_simple_tuple_types(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("TupleField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let tuple_type = Type::Tuple(TypeList {
        items: vec![
            TypeReference::nat(),
            TypeReference::text(),
            TypeReference::int32(),
        ],
    });
    let types = vec![main_actor, tuple_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_nested_tuple_types(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("TupleField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let outer_tuple_type = Type::Tuple(TypeList {
        items: vec![TypeReference { index: 2 }],
    });
    let inner_tuple_type = Type::Tuple(TypeList {
        items: vec![TypeReference::nat()],
    });
    let types = vec![main_actor, outer_tuple_type, inner_tuple_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_recursive_tuple_types(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("TupleField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let first_tuple_type = Type::Tuple(TypeList {
        items: vec![TypeReference { index: 2 }],
    });
    let second_tuple_type = Type::Tuple(TypeList {
        items: vec![TypeReference { index: 1 }],
    });
    let types = vec![main_actor, first_tuple_type, second_tuple_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_reordered_variant_fields(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("VariantField"),
            field_type: TypeReference { index: 1 },
        }],
    });

    let field1 = Field {
        name: String::from("Field1"),
        field_type: TypeReference::int8(),
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: TypeReference::int16(),
    };
    let field3 = Field {
        name: String::from("Field3"),
        field_type: TypeReference::int32(),
    };

    let old_variant = Type::Variant(FieldList {
        fields: vec![field3.clone(), field2.clone(), field1.clone()],
    });
    let new_variant = Type::Variant(FieldList {
        fields: vec![field1.clone(), field2.clone(), field3.clone()],
    });

    let old_types = vec![main_actor.clone(), old_variant];
    let new_types = vec![main_actor.clone(), new_variant];
    assert!(are_compatible(heap, old_types, new_types));
}

unsafe fn test_added_variant_fields(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("VariantField"),
            field_type: TypeReference { index: 1 },
        }],
    });

    let field1 = Field {
        name: String::from("Field1"),
        field_type: TypeReference::int8(),
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: TypeReference::int16(),
    };
    let field3 = Field {
        name: String::from("Field3"),
        field_type: TypeReference::int32(),
    };

    let old_variant = Type::Variant(FieldList {
        fields: vec![field2.clone()],
    });
    let new_variant = Type::Variant(FieldList {
        fields: vec![field1.clone(), field2.clone(), field3.clone()],
    });

    let old_types = vec![main_actor.clone(), old_variant];
    let new_types = vec![main_actor.clone(), new_variant];
    assert!(are_compatible(heap, old_types, new_types));
}

unsafe fn test_any_to_any(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("Field"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let any_type = Type::Any;
    let types = vec![main_actor, any_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_some_to_any(heap: &mut TestMemory) {
    let old_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("Field"),
            field_type: TypeReference::text(),
        }],
    });
    let new_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("Field"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let any_type = Type::Any;
    let old_types = vec![old_actor];
    let new_types = vec![new_actor, any_type];
    assert!(are_compatible(heap, old_types, new_types));
}

unsafe fn test_none_to_none(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("Field"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let none_type = Type::None;
    let types = vec![main_actor, none_type];
    assert!(are_compatible(heap, types.clone(), types.clone()));
}

unsafe fn test_failing_cases(heap: &mut TestMemory) {
    test_added_object_fields(heap);
    test_added_actor_fields(heap);
    test_actor_to_object(heap);
    test_object_to_actor(heap);
    test_mutable_mismatch(heap);
    test_immutable_mismatch(heap);
    test_recursion_mismatch(heap);
    test_option_mismatch(heap);
    test_array_mismatch(heap);
    test_array_mutability_mismatch(heap);
    test_reordered_tuple_types(heap);
    test_removed_variant_fields(heap);
    test_any_to_some(heap);
    test_any_to_none(heap);
}

unsafe fn test_recursion_mismatch(heap: &mut TestMemory) {
    let old_actor_field = Field {
        name: String::from("ActorField"),
        field_type: TypeReference { index: 1 },
    };
    let old_actor = Type::Object(FieldList {
        fields: vec![old_actor_field],
    });
    let recursive_field = Field {
        name: String::from("Field"),
        field_type: TypeReference { index: 1 },
    };
    let recursive_type = Type::Object(FieldList {
        fields: vec![recursive_field],
    });
    let new_actor_field = Field {
        name: String::from("ActorField"),
        field_type: TypeReference { index: 1 },
    };
    let new_actor = Type::Object(FieldList {
        fields: vec![new_actor_field],
    });
    let non_recursive_field = Field {
        name: String::from("Field"),
        field_type: TypeReference::principal(),
    };
    let non_recursive_type = Type::Object(FieldList {
        fields: vec![non_recursive_field],
    });
    let old_types = vec![old_actor, recursive_type];
    let new_types = vec![new_actor, non_recursive_type];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_added_object_fields(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ActorField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let field1 = Field {
        name: String::from("Field1"),
        field_type: TypeReference::nat64(),
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: TypeReference::float(),
    };
    let field3 = Field {
        name: String::from("Field3"),
        field_type: TypeReference::int64(),
    };

    let old_type = Type::Object(FieldList {
        fields: vec![field2.clone()],
    });
    let new_type = Type::Object(FieldList {
        fields: vec![field1.clone(), field2.clone(), field3.clone()],
    });
    let old_types = vec![main_actor.clone(), old_type];
    let new_types = vec![main_actor.clone(), new_type];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_added_actor_fields(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ActorField"),
            field_type: TypeReference { index: 1 },
        }],
    });

    let field1 = Field {
        name: String::from("Field1"),
        field_type: TypeReference::nat64(),
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: TypeReference::float(),
    };
    let field3 = Field {
        name: String::from("Field3"),
        field_type: TypeReference::int64(),
    };

    let old_type = Type::Actor(FieldList {
        fields: vec![field2.clone()],
    });
    let new_type = Type::Actor(FieldList {
        fields: vec![field1.clone(), field2.clone(), field3.clone()],
    });
    let old_types = vec![main_actor.clone(), old_type];
    let new_types = vec![main_actor.clone(), new_type];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_actor_to_object(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ActorField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let old_type = Type::Actor(FieldList { fields: vec![] });
    let new_type = Type::Object(FieldList { fields: vec![] });
    let old_types = vec![main_actor.clone(), old_type];
    let new_types = vec![main_actor.clone(), new_type];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_object_to_actor(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ActorField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let old_type = Type::Actor(FieldList { fields: vec![] });
    let new_type = Type::Object(FieldList { fields: vec![] });
    let old_types = vec![main_actor.clone(), old_type];
    let new_types = vec![main_actor.clone(), new_type];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_mutable_mismatch(heap: &mut TestMemory) {
    let old_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ActorField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let mutable_type = Type::Mutable(TypeReference::nat());
    let new_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ActorField"),
            field_type: TypeReference::nat(),
        }],
    });
    let old_types = vec![old_actor, mutable_type];
    let new_types = vec![new_actor];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_immutable_mismatch(heap: &mut TestMemory) {
    let old_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ActorField"),
            field_type: TypeReference::bool(),
        }],
    });
    let new_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ActorField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let mutable_type = Type::Mutable(TypeReference::nat());
    let old_types = vec![old_actor];
    let new_types = vec![new_actor, mutable_type];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_option_mismatch(heap: &mut TestMemory) {
    let old_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("OptionalField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let option_type = Type::Option(TypeReference::nat());
    let new_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("OptionalField"),
            field_type: TypeReference::float(),
        }],
    });
    let old_types = vec![old_actor, option_type];
    let new_types = vec![new_actor];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_array_mismatch(heap: &mut TestMemory) {
    let old_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ArrayField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let array_type = Type::Array(TypeReference::nat());
    let new_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ArrayField"),
            field_type: TypeReference::text(),
        }],
    });
    let old_types = vec![old_actor, array_type];
    let new_types = vec![new_actor];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_array_mutability_mismatch(heap: &mut TestMemory) {
    let old_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ArrayField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let immutable_array_type = Type::Array(TypeReference::nat());
    let new_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("ArrayField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let mutable_array_type = Type::Array(TypeReference { index: 2 });
    let mutable_variable = Type::Mutable(TypeReference::nat());
    let old_types = vec![old_actor, immutable_array_type];
    let new_types = vec![new_actor, mutable_array_type, mutable_variable];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_reordered_tuple_types(heap: &mut TestMemory) {
    let old_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("TupleField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let old_tuple = Type::Tuple(TypeList {
        items: vec![TypeReference::nat(), TypeReference::text()],
    });
    let new_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("TupleField"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let new_tuple = Type::Tuple(TypeList {
        items: vec![TypeReference::text(), TypeReference::nat()],
    });
    let old_types = vec![old_actor, old_tuple];
    let new_types = vec![new_actor, new_tuple];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_removed_variant_fields(heap: &mut TestMemory) {
    let main_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("VariantField"),
            field_type: TypeReference { index: 1 },
        }],
    });

    let field1 = Field {
        name: String::from("Field1"),
        field_type: TypeReference::int8(),
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: TypeReference::int16(),
    };
    let field3 = Field {
        name: String::from("Field3"),
        field_type: TypeReference::int32(),
    };

    let old_variant = Type::Variant(FieldList {
        fields: vec![field1.clone(), field2.clone(), field3.clone()],
    });
    let new_variant = Type::Variant(FieldList {
        fields: vec![field2.clone()],
    });

    let old_types = vec![main_actor.clone(), old_variant];
    let new_types = vec![main_actor.clone(), new_variant];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_any_to_some(heap: &mut TestMemory) {
    let old_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("Field"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let any_type = Type::Any;
    let new_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("Field"),
            field_type: TypeReference::text(),
        }],
    });
    let old_types = vec![old_actor, any_type];
    let new_types = vec![new_actor];
    assert!(!are_compatible(heap, old_types, new_types));
}

unsafe fn test_any_to_none(heap: &mut TestMemory) {
    let old_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("Field"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let any_type = Type::Any;
    let new_actor = Type::Object(FieldList {
        fields: vec![Field {
            name: String::from("Field"),
            field_type: TypeReference { index: 1 },
        }],
    });
    let none_type = Type::None;
    let old_types = vec![old_actor, any_type];
    let new_types = vec![new_actor, none_type];
    assert!(!are_compatible(heap, old_types, new_types));
}
