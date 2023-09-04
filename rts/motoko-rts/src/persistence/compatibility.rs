//! Persistent type compatibility check.
//! Determines whether the a new actor type is compatible with the existing persistent state.
//!
//! This is determined by focusing on the stable actor fields (comprised in `stable_actor`).
//!
//! To be compatible, all the fields of an actor must be compatible to the previous state.
//! A field is compatible if at least one of the condition holds:
//! * The field is declared flexible, i.e. not stable, in the previous or new actor version.
//! * The previous actor does not contain a field with that same name.
//! * The field type is compatible to the type of the equally named field in the previous version.
//!
//! A new type is compatible to a previous type if
//! * the types are identical
//! * The new type is sub-type of the previous field.
//!
//! The existing sub-type relations are also memory-compatible at the runtime system level:
//! * Nat can be interpreted as Int.
//! * If record fields are dropped in a declaration, old records with more fields can still
//!   be accessed.
//!   -> However, redundant old fields should be removed in the future, e.g. by the GC.
//! * Variant options can be added, without invalidating the existing variant tags.
//!
//! Binary format of the stable type encoding, generated by the Motoko compiler backend, `persistence.ml`.
//!
//! All composed non-primitive types are encoded in a table without defined order, except for the
//! first entry.
//! The first type in the table denotes the stable sub-type of the actor, ie. all its stable field
//! declarations, without option wrapping.
//!
//! Non-primitive types are referred to by a positive index according to the type table.
//! Primitive types are encoded by predefined negative indices.
//! All numbers (type indices etc.) are encoded as little endian i32.
//!
//! ```
//! <type_table> ::= length:i32 (<type>)^length
//! <type> ::= <object> | <actor> | <function> | <mutable> | <option> | <array> | <tuple> | <variant> | <none> | <any>
//! <object> ::= 1l <field_list>
//! <actor> ::= 2l <field_list>
//! <function> ::= 3l function_sort:i32 <type_list> <type_list>
//! <mutable> ::= 4l type_index:i32
//! <option> ::= 5l type_index:i32
//! <array> ::= 6l type_index:i32
//! <tuple> ::= 7l length:i32 (type_index:i32)^length
//! <variant> ::= 8l <field_list>
//! <none> ::= 9l
//! <any> ::= 10l
//! <field_list> ::= length:i32 (<field>)^length
//! <field> ::= label_hash:i32 type_index:i32
//! <type_list> ::= length:i32 <type_index:i32>^length
//! ```
//!
//! Predefined primitive type indices
//! Type      | Index
//! --------- | --------
//! Null      | -1l
//! Bool      | -2l
//! Nat       | -3l
//! Nat8      | -4l
//! Nat16     | -5l
//! Nat32     | -6l
//! Nat64     | -7l
//! Int       | -8l
//! Int8      | -9l
//! Int16     | -10l
//! Int32     | -11l
//! Int64     | -12l
//! Float     | -13l
//! Char      | -14l
//! Text      | -15l
//! Blob      | -16l
//! Principal | -17l
//!
//! Function sort
//! Sort      | Encoding
//! --------- | ---------
//! Query     | 1l
//! Write     | 2l
//! Composite | 3l

use core::mem::swap;

use crate::{
    mem_utils::memzero_bytes,
    memory::{alloc_blob, Memory},
    types::{size_of, Bytes, Value},
};

pub const OBJECT_ENCODING_TAG: i32 = 1;
pub const ACTOR_ENCODING_TAG: i32 = 2;
pub const FUNCTION_ENCODING_TAG: i32 = 3;
pub const MUTABLE_ENCODING_TAG: i32 = 4;
pub const OPTION_ENCODING_TAG: i32 = 5;
pub const ARRAY_ENCODING_TAG: i32 = 6;
pub const TUPLE_ENCODING_TAG: i32 = 7;
pub const VARIANT_ENCODING_TAG: i32 = 8;
pub const NONE_ENCODING_TAG: i32 = 9;
pub const ANY_ENCODING_TAG: i32 = 10;

const ACTOR_TYPE_INDEX: i32 = 0;

const TYPE_TAG_LENGTH: usize = 1;
const FUNCTION_SORT_LENGTH: usize = 1;
const LENGTH_HEADER: usize = 1;
const TYPE_INDEX_LENGTH: usize = 1;
const FIELD_ENCODING_LENGTH: usize = 2; // label_hash type_index

const NAT_TYPE_INDEX: i32 = -3;
const INT_TYPE_INDEX: i32 = -8;

struct EncodedData {
    words: *const i32,
    size: usize,
}

impl EncodedData {
    pub fn new(words: *const i32, size: usize) -> EncodedData {
        EncodedData { words, size }
    }

    unsafe fn read(&self, offset: usize) -> i32 {
        assert!(offset < self.size);
        let location = self.words.add(offset);
        *location
    }

    unsafe fn sub_view(&self, offset: usize) -> EncodedData {
        assert!(offset <= self.size);
        EncodedData {
            words: self.words.add(offset),
            size: self.size - offset,
        }
    }
}

struct TypeTable {
    data: EncodedData,
}

impl TypeTable {
    unsafe fn new(value: Value) -> TypeTable {
        let blob = value.as_blob();
        assert_eq!(blob.len().as_usize() % size_of::<i32>().as_usize(), 0);
        let words = blob.payload_const() as *const i32;
        let size = blob.len().as_usize() / size_of::<i32>().to_bytes().as_usize();
        let data = EncodedData::new(words, size);
        TypeTable { data }
    }

    unsafe fn count_types(&self) -> usize {
        let count = self.data.read(0);
        assert!(count >= 0);
        count as usize
    }

    unsafe fn get_type(&self, type_index: i32) -> Type {
        assert!(type_index >= 0);
        assert!(type_index <= self.count_types() as i32);
        let mut start = 1;
        for _ in 0..type_index {
            let type_view = self.data.sub_view(start);
            start += Type::size(type_view);
        }
        let type_view = self.data.sub_view(start);
        Type::get_type(type_view)
    }

    unsafe fn get_actor_fields(&self) -> FieldList {
        match self.get_type(ACTOR_TYPE_INDEX) {
            Type::Object(field_list) => field_list,
            _ => panic!("Invalid stable type encoding"),
        }
    }
}

enum Type {
    Object(FieldList),
    Actor(FieldList),
    Function(FunctionSignature),
    Mutable(TypeIndex),
    Option(TypeIndex),
    Array(TypeIndex),
    Tuple(TypeList),
    Variant(FieldList),
    None,
    Any,
}

impl Type {
    unsafe fn size(data: EncodedData) -> usize {
        TYPE_TAG_LENGTH
            + match Self::get_type(data) {
                Self::Object(field_list) => field_list.size(),
                Self::Actor(field_list) => field_list.size(),
                Self::Function(signature) => signature.size(),
                Self::Mutable(type_index) => type_index.size(),
                Self::Option(type_index) => type_index.size(),
                Self::Array(type_index) => type_index.size(),
                Self::Tuple(tuple_list) => tuple_list.size(),
                Self::Variant(field_list) => field_list.size(),
                Self::None | Self::Any => 0,
            }
    }

    unsafe fn get_type(data: EncodedData) -> Type {
        let tag = data.read(0);
        let data = data.sub_view(TYPE_TAG_LENGTH);
        match tag {
            OBJECT_ENCODING_TAG => Self::Object(FieldList::new(data)),
            ACTOR_ENCODING_TAG => Self::Actor(FieldList::new(data)),
            FUNCTION_ENCODING_TAG => Self::Function(FunctionSignature::new(data)),
            MUTABLE_ENCODING_TAG => Self::Mutable(TypeIndex::new(data)),
            OPTION_ENCODING_TAG => Self::Option(TypeIndex::new(data)),
            ARRAY_ENCODING_TAG => Self::Array(TypeIndex::new(data)),
            TUPLE_ENCODING_TAG => Self::Tuple(TypeList::new(data)),
            VARIANT_ENCODING_TAG => Self::Variant(FieldList::new(data)),
            NONE_ENCODING_TAG => Self::None,
            ANY_ENCODING_TAG => Self::Any,
            _ => unreachable!(),
        }
    }
}

struct FieldList {
    data: EncodedData,
}

impl FieldList {
    unsafe fn new(data: EncodedData) -> FieldList {
        FieldList { data }
    }

    unsafe fn size(&self) -> usize {
        LENGTH_HEADER + self.count_fields() * FIELD_ENCODING_LENGTH
    }

    unsafe fn count_fields(&self) -> usize {
        let count = self.data.read(0);
        assert!(count >= 0);
        count as usize
    }

    unsafe fn get_field(&self, field_index: usize) -> Field {
        let field_offset = LENGTH_HEADER + field_index * FIELD_ENCODING_LENGTH;
        let label_hash = self.data.read(field_offset);
        let type_index = self.data.read(field_offset + 1);
        Field {
            label_hash,
            type_index,
        }
    }

    unsafe fn find_field(&self, label_hash: i32) -> Option<Field> {
        for field_index in 0..self.count_fields() {
            let field = self.get_field(field_index);
            if field.label_hash == label_hash {
                return Some(field);
            }
        }
        None
    }
}

struct Field {
    label_hash: i32,
    type_index: i32,
}

struct TypeList {
    data: EncodedData,
}

impl TypeList {
    unsafe fn new(data: EncodedData) -> TypeList {
        TypeList { data }
    }

    unsafe fn size(&self) -> usize {
        LENGTH_HEADER + self.count_items() * TYPE_INDEX_LENGTH
    }

    unsafe fn count_items(&self) -> usize {
        let count = self.data.read(0);
        assert!(count >= 0);
        count as usize
    }

    unsafe fn get_item(&self, item_index: usize) -> TypeIndex {
        let item_offset = LENGTH_HEADER + item_index * TYPE_INDEX_LENGTH;
        let type_index = self.data.read(item_offset);
        TypeIndex { type_index }
    }
}

struct TypeIndex {
    type_index: i32,
}

impl TypeIndex {
    unsafe fn new(data: EncodedData) -> TypeIndex {
        let type_index = data.read(0);
        TypeIndex { type_index }
    }

    unsafe fn size(&self) -> usize {
        TYPE_INDEX_LENGTH
    }
}

#[derive(PartialEq)]
enum FunctionSort {
    Query,
    Write,
    Composite,
}

pub const QUERY_FUNCTION_SORT: i32 = 1;
pub const WRITE_FUNCTION_SORT: i32 = 2;
pub const COMPOSITE_FUNCTION_SORT: i32 = 3;

struct FunctionSignature {
    data: EncodedData,
}

impl FunctionSignature {
    unsafe fn new(data: EncodedData) -> FunctionSignature {
        FunctionSignature { data }
    }

    unsafe fn size(&self) -> usize {
        self.return_types_offset() + LENGTH_HEADER + self.count_return_types() * TYPE_INDEX_LENGTH
    }

    unsafe fn sort(&self) -> FunctionSort {
        match self.data.read(0) {
            QUERY_FUNCTION_SORT => FunctionSort::Query,
            WRITE_FUNCTION_SORT => FunctionSort::Write,
            COMPOSITE_FUNCTION_SORT => FunctionSort::Composite,
            _ => unreachable!(),
        }
    }

    unsafe fn count_parameters(&self) -> usize {
        let count = self.data.read(FUNCTION_SORT_LENGTH);
        assert!(count >= 0);
        count as usize
    }

    unsafe fn get_parameter(&self, index: usize) -> TypeIndex {
        let offset = FUNCTION_SORT_LENGTH + LENGTH_HEADER + index * TYPE_INDEX_LENGTH;
        let type_index = self.data.read(offset);
        TypeIndex { type_index }
    }

    unsafe fn return_types_offset(&self) -> usize {
        FUNCTION_SORT_LENGTH + LENGTH_HEADER + self.count_parameters() * TYPE_INDEX_LENGTH
    }

    unsafe fn count_return_types(&self) -> usize {
        let offset = self.return_types_offset();
        let count = self.data.read(offset);
        assert!(count >= 0);
        count as usize
    }

    unsafe fn get_return_type(&self, index: usize) -> TypeIndex {
        let offset = self.return_types_offset() + LENGTH_HEADER + index * TYPE_INDEX_LENGTH;
        let type_index = self.data.read(offset);
        TypeIndex { type_index }
    }
}

/// Cache for remembering previous type compatibility checks.
/// Necessary to avoid infinite loops on type recursion.
/// Helpful to optimize repeated checks.
struct TypeCheckCache {
    bitmap: *mut u8,
    count_new_types: usize,
    count_old_types: usize,
}

impl TypeCheckCache {
    pub unsafe fn new<M: Memory>(
        mem: &mut M,
        new_types: &TypeTable,
        old_types: &TypeTable,
    ) -> TypeCheckCache {
        let count_new_types = new_types.count_types();
        let count_old_types = old_types.count_types();
        let bit_size = count_new_types * count_old_types;
        let byte_size = Bytes((bit_size as u32 + u8::BITS - 1) / u8::BITS);
        let blob = alloc_blob(mem, byte_size);
        let bitmap = blob.as_blob_mut().payload_addr();
        memzero_bytes(bitmap as usize, byte_size);
        TypeCheckCache {
            bitmap,
            count_new_types,
            count_old_types,
        }
    }

    pub unsafe fn visited(&self, new_type_index: i32, old_type_index: i32) -> bool {
        let (byte, bit_index) = self.position(new_type_index, old_type_index);
        (*byte >> bit_index) & 0b1 != 0
    }

    pub unsafe fn visit(&mut self, new_type_index: i32, old_type_index: i32) {
        let (byte, bit_index) = self.position(new_type_index, old_type_index);
        *byte |= 0b1 << bit_index;
    }

    unsafe fn position(&self, new_type_index: i32, old_type_index: i32) -> (*mut u8, usize) {
        assert!(new_type_index >= 0);
        assert!(old_type_index >= 0);
        assert!((new_type_index as usize) < self.count_new_types);
        assert!((old_type_index as usize) < self.count_old_types);
        let index = new_type_index as usize * self.count_old_types + old_type_index as usize;
        let byte_index = index / u8::BITS as usize;
        let bit_index = index % u8::BITS as usize;
        let byte = self.bitmap.add(byte_index);
        (byte, bit_index)
    }
}

struct CompatibilityChecker {
    covariance_cache: TypeCheckCache,
    contravariance_cache: TypeCheckCache,
    target_type_table: TypeTable,
    source_type_table: TypeTable,
}

impl CompatibilityChecker {
    unsafe fn new<M: Memory>(
        mem: &mut M,
        target_type_table: TypeTable,
        source_type_table: TypeTable,
    ) -> CompatibilityChecker {
        let covariance_cache = TypeCheckCache::new(mem, &target_type_table, &source_type_table);
        let contravariance_cache = TypeCheckCache::new(mem, &source_type_table, &target_type_table);
        CompatibilityChecker {
            covariance_cache,
            contravariance_cache,
            target_type_table,
            source_type_table,
        }
    }

    fn flip_variance(&mut self) {
        swap(&mut self.covariance_cache, &mut self.contravariance_cache);
        swap(&mut self.target_type_table, &mut self.source_type_table);
    }

    unsafe fn compatible_object_fields(
        &mut self,
        target_field_list: &FieldList,
        source_field_list: &FieldList,
    ) -> bool {
        for field_index in 0..target_field_list.count_fields() {
            let target_field = target_field_list.get_field(field_index);
            match source_field_list.find_field(target_field.label_hash) {
                None => {
                    return false;
                }
                Some(source_field) => {
                    if !self.type_compatible(target_field.type_index, source_field.type_index) {
                        return false;
                    }
                }
            }
        }
        true
    }

    unsafe fn compatible_variant_fields(
        &mut self,
        target_field_list: &FieldList,
        source_field_list: &FieldList,
    ) -> bool {
        for field_index in 0..source_field_list.count_fields() {
            let source_field = source_field_list.get_field(field_index);
            match target_field_list.find_field(source_field.label_hash) {
                None => {
                    return false;
                }
                Some(target_field) => {
                    if !self.type_compatible(target_field.type_index, source_field.type_index) {
                        return false;
                    }
                }
            }
        }
        true
    }

    unsafe fn compatible_type_indices(
        &mut self,
        target_type: &TypeIndex,
        source_type: &TypeIndex,
    ) -> bool {
        self.type_compatible(target_type.type_index, source_type.type_index)
    }

    unsafe fn compatible_type_list(
        &mut self,
        target_tuple: &TypeList,
        source_tuple: &TypeList,
    ) -> bool {
        let target_count = target_tuple.count_items();
        let source_count = source_tuple.count_items();
        if target_count != source_count {
            return false;
        }
        for item_index in 0..target_count {
            let new_item = target_tuple.get_item(item_index);
            let old_item = source_tuple.get_item(item_index);
            if !self.compatible_type_indices(&new_item, &old_item) {
                return false;
            }
        }
        true
    }

    unsafe fn compatible_functions(
        &mut self,
        target_signature: &FunctionSignature,
        source_signature: &FunctionSignature,
    ) -> bool {
        target_signature.sort() == source_signature.sort()
            && self.compatible_parameters(target_signature, source_signature)
            && self.compatible_return_types(target_signature, source_signature)
    }

    unsafe fn compatible_parameters(
        &mut self,
        target_signature: &FunctionSignature,
        source_signature: &FunctionSignature,
    ) -> bool {
        let target_count = target_signature.count_parameters();
        let source_count = source_signature.count_parameters();
        if target_count != source_count {
            return false;
        }
        for index in 0..target_count {
            let target_parameter = target_signature.get_parameter(index);
            let source_parameter = source_signature.get_parameter(index);
            // Contravariance
            if !self.reverse_compatible(source_parameter.type_index, target_parameter.type_index) {
                return false;
            }
        }
        true
    }

    unsafe fn reverse_compatible(
        &mut self,
        source_type_index: i32,
        target_type_index: i32,
    ) -> bool {
        self.flip_variance();
        let result = self.type_compatible(source_type_index, target_type_index);
        self.flip_variance();
        result
    }

    unsafe fn compatible_return_types(
        &mut self,
        target_signature: &FunctionSignature,
        source_signature: &FunctionSignature,
    ) -> bool {
        let target_count = target_signature.count_return_types();
        let source_count = source_signature.count_return_types();
        if target_count != source_count {
            return false;
        }
        for index in 0..target_count {
            let target_return_type = target_signature.get_return_type(index);
            let source_return_type = source_signature.get_return_type(index);
            // Covariance
            if !self.type_compatible(target_return_type.type_index, source_return_type.type_index) {
                return false;
            }
        }
        true
    }

    unsafe fn compatible_primitives(
        &mut self,
        target_type_index: i32,
        source_type_index: i32,
    ) -> bool {
        debug_assert!(target_type_index < 0 || source_type_index < 0);
        if target_type_index == source_type_index {
            return true;
        }
        if target_type_index == INT_TYPE_INDEX && source_type_index == NAT_TYPE_INDEX {
            return true;
        }
        if target_type_index >= 0 {
            if let Type::Any = self.target_type_table.get_type(target_type_index) {
                return true;
            }
        }
        false
    }

    unsafe fn type_compatible(&mut self, target_type_index: i32, source_type_index: i32) -> bool {
        if target_type_index < 0 || source_type_index < 0 {
            return self.compatible_primitives(target_type_index, source_type_index);
        }
        if self
            .covariance_cache
            .visited(target_type_index, source_type_index)
        {
            return true;
        }
        self.covariance_cache
            .visit(target_type_index, source_type_index);
        let target_type = self.target_type_table.get_type(target_type_index);
        let source_type = self.source_type_table.get_type(source_type_index);
        match (&target_type, &source_type) {
            (Type::Any, _) => true,
            (Type::Object(new_fields), Type::Object(old_fields)) => {
                self.compatible_object_fields(new_fields, old_fields)
            }
            (Type::Object(_), _) => false,
            (Type::Actor(new_fields), Type::Actor(old_fields)) => {
                self.compatible_object_fields(new_fields, old_fields)
            }
            (Type::Actor(_), _) => false,
            (Type::Function(new_signature), Type::Function(old_signature)) => {
                self.compatible_functions(new_signature, old_signature)
            }
            (Type::Function(_), _) => false,
            (Type::Mutable(new_variable), Type::Mutable(old_variable)) => {
                self.compatible_type_indices(new_variable, old_variable)
            }
            (Type::Mutable(_), _) => false,
            (Type::Option(new_option), Type::Option(old_option)) => {
                self.compatible_type_indices(new_option, old_option)
            }
            (Type::Option(_), _) => false,
            (Type::Array(new_element), Type::Array(old_element)) => {
                self.compatible_type_indices(new_element, old_element)
            }
            (Type::Array(_), _) => false,
            (Type::Tuple(new_type_list), Type::Tuple(old_type_list)) => {
                self.compatible_type_list(new_type_list, old_type_list)
            }
            (Type::Tuple(_), _) => false,
            (Type::Variant(new_fields), Type::Variant(old_fields)) => {
                self.compatible_variant_fields(new_fields, old_fields)
            }
            (Type::Variant(_), _) => false,
            (Type::None, Type::None) => true,
            (Type::None, _) => false,
        }
    }

    unsafe fn compatible_main_actors(&mut self) -> bool {
        let target_field_list = self.target_type_table.get_actor_fields();
        let source_field_list = self.source_type_table.get_actor_fields();
        for target_field_index in 0..target_field_list.count_fields() {
            let target_field = target_field_list.get_field(target_field_index);
            match source_field_list.find_field(target_field.label_hash) {
                Some(source_field) => {
                    if !self.type_compatible(target_field.type_index, source_field.type_index) {
                        return false;
                    }
                }
                None => {}
            }
        }
        true
    }
}

/// Test whether the new stable type complies with the existing old stable type.
/// Both arguments point to blobs encoding a stable actor type.
pub unsafe fn memory_compatible<M: Memory>(mem: &mut M, old_type: Value, new_type: Value) -> bool {
    let new_type_table = TypeTable::new(new_type);
    let old_type_table = TypeTable::new(old_type);
    let mut checker = CompatibilityChecker::new(mem, new_type_table, old_type_table);
    checker.compatible_main_actors()
}
