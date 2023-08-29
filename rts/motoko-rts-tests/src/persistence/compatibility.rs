use motoko_rts::memory::{alloc_blob, Memory};
use motoko_rts::persistence::compatibility::memory_compatible;
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

#[derive(PartialEq, Clone)]
enum Type {
    Nat,
    Object(ObjectType),
}

impl Type {
    fn is_primitive(&self) -> bool {
        match &self {
            Self::Nat => true,
            Self::Object(_) => false,
        }
    }

    fn inner_types(&self) -> Vec<Type> {
        match &self {
            Self::Nat => vec![],
            Self::Object(object_type) => object_type.inner_types(),
        }
    }

    fn serialize(&self, output: &mut BinaryData, table: &TypeTable) {
        match &self {
            Self::Nat => unreachable!(),
            Self::Object(object_type) => object_type.serialize(output, table),
        }
    }

    fn type_id(&self, table: &TypeTable) -> i32 {
        const NAT_TYPE_ID: i32 = -1;
        match &self {
            Self::Nat => NAT_TYPE_ID,
            Self::Object(_) => table.index_of(self),
        }
    }
}

#[derive(PartialEq, Clone)]
struct Field {
    name: String,
    field_type: Type,
}

impl Field {
    fn serialize(&self, output: &mut BinaryData, table: &TypeTable) {
        output.write_hash(&self.name);
        output.write_i32(self.field_type.type_id(table));
    }
}

#[derive(PartialEq, Clone)]
struct ObjectType {
    fields: Vec<Field>,
}

impl ObjectType {
    fn inner_types(&self) -> Vec<Type> {
        self.fields
            .iter()
            .map(|field| field.field_type.clone())
            .collect()
    }

    fn serialize(&self, output: &mut BinaryData, table: &TypeTable) {
        const OBJECT_TAG: i32 = 1;
        output.write_i32(OBJECT_TAG);
        output.write_i32(self.fields.len() as i32);
        for field in &self.fields {
            field.serialize(output, table);
        }
    }
}

struct TypeTable {
    types: Vec<Type>,
}

impl TypeTable {
    fn new(actor_type: Type) -> TypeTable {
        let mut table = TypeTable { types: vec![] };
        table.collect_types(actor_type);
        table
    }

    fn collect_types(&mut self, current_type: Type) {
        if !current_type.is_primitive() && !self.types.contains(&current_type) {
            self.types.push(current_type.clone());
            for inner_type in current_type.inner_types() {
                self.collect_types(inner_type);
            }
        }
    }

    fn index_of(&self, search_type: &Type) -> i32 {
        assert!(!search_type.is_primitive());
        for index in 0..self.types.len() {
            if self.types.get(index).unwrap() == search_type {
                return index as i32;
            }
        }
        unreachable!()
    }

    fn serialize(&self) -> BinaryData {
        let mut output = BinaryData::new();
        output.write_i32(self.types.len() as i32);
        for current_type in &self.types {
            current_type.serialize(&mut output, &self);
        }
        output
    }
}

unsafe fn build<M: Memory>(mem: &mut M, actor_type: Type) -> Value {
    TypeTable::new(actor_type).serialize().make_blob(mem)
}

unsafe fn is_compatible<M: Memory>(mem: &mut M, old_type: Type, new_type: Type) -> bool {
    let old_type_blob = build(mem, old_type);
    let new_type_blob = build(mem, new_type);
    memory_compatible(mem, old_type_blob, new_type_blob)
}

pub unsafe fn test() {
    println!("  Testing memory compatibility...");
    let mut heap = initialize_test_memory();
    test_sucessful_cases(&mut heap);
    test_failing_cases(&mut heap);
    reset_test_memory();
}

unsafe fn test_sucessful_cases(heap: &mut TestMemory) {
    test_empty_actor(heap);
    test_reordered_actor_fields(heap);
    test_removed_actor_fields(heap);
    test_added_actor_fields(heap);
}

unsafe fn test_empty_actor(heap: &mut TestMemory) {
    let old_type = Type::Object(ObjectType { fields: vec![] });
    let new_type = Type::Object(ObjectType { fields: vec![] });
    assert!(is_compatible(heap, old_type, new_type));
}

unsafe fn test_reordered_actor_fields(heap: &mut TestMemory) {
    let field1 = Field {
        name: String::from("Field1"),
        field_type: Type::Nat,
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: Type::Nat,
    };

    let old_type = Type::Object(ObjectType {
        fields: vec![field1.clone(), field2.clone()],
    });
    let new_type = Type::Object(ObjectType {
        fields: vec![field2.clone(), field1.clone()],
    });

    assert!(is_compatible(heap, old_type, new_type));
}

unsafe fn test_removed_actor_fields(heap: &mut TestMemory) {
    let field1 = Field {
        name: String::from("Field1"),
        field_type: Type::Nat,
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: Type::Nat,
    };
    let field3 = Field {
        name: String::from("Field3"),
        field_type: Type::Nat,
    };

    let old_type = Type::Object(ObjectType {
        fields: vec![field1.clone(), field2.clone(), field3.clone()],
    });
    let new_type = Type::Object(ObjectType {
        fields: vec![field2.clone()],
    });

    assert!(is_compatible(heap, old_type, new_type));
}

unsafe fn test_added_actor_fields(heap: &mut TestMemory) {
    let field1 = Field {
        name: String::from("Field1"),
        field_type: Type::Nat,
    };
    let field2 = Field {
        name: String::from("Field2"),
        field_type: Type::Nat,
    };
    let field3 = Field {
        name: String::from("Field3"),
        field_type: Type::Nat,
    };

    let old_type = Type::Object(ObjectType {
        fields: vec![field2.clone()],
    });
    let new_type = Type::Object(ObjectType {
        fields: vec![field1.clone(), field2.clone(), field3.clone()],
    });

    assert!(is_compatible(heap, old_type, new_type));
}

unsafe fn test_failing_cases(_heap: &mut TestMemory) {}
