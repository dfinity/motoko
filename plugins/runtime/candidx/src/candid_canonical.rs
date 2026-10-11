//! Internal used-shape support for compiler-generated canonical serialization.
//! Payload traversal, sizing, and emission belong to generated Wasm code.

use alloc::boxed::Box;
use alloc::vec::Vec;
use core::mem::{align_of, size_of, take};

const MAX_DEPTH: usize = 256;
const MAX_ITEMS: usize = 2_000_000;
const MAX_TYPES: usize = 1024;
const MAX_BYTES: usize = 32 * 1024 * 1024;
const MAX_SCRATCH: usize = 256 * 1024 * 1024;
const MISSING: i64 = i64::MIN;
// Internal builder mode, outside the supported compound opcode range.
const PRIMITIVE_VARIANTS: i64 = 0;

fn invalid(message: &str) -> ! {
    crate::trap_with_prefix("CandidX error: ", message)
}

// The ephemeral allocator backs allocations with a three-word Blob header.
// Include its alignment padding and heap-word rounding, as well as the entire
// new allocation on every growth: old buffers remain garbage until GC.
fn allocation_size<T>(count: usize) -> usize {
    let word = size_of::<usize>();
    let alignment = align_of::<T>().max(word);
    count
        .checked_mul(size_of::<T>())
        .and_then(|n| n.checked_add(3 * word + alignment - 1))
        .unwrap_or_else(|| invalid("scratch byte limit"))
}
struct Scratch {
    used: usize,
    limit: usize,
}
impl Scratch {
    fn charge<T>(&mut self, count: usize) {
        self.used = self
            .used
            .checked_add(allocation_size::<T>(count))
            .filter(|used| *used <= self.limit)
            .unwrap_or_else(|| invalid("scratch byte limit"));
    }
    fn reserve<T>(&mut self, values: &mut Vec<T>, required: usize) {
        if values.capacity() >= required {
            return;
        }
        let capacity = required.max(values.capacity().saturating_mul(2));
        self.charge::<T>(capacity);
        values
            .try_reserve_exact(capacity - values.len())
            .unwrap_or_else(|_| invalid("scratch allocation failed"));
    }
    fn push<T>(&mut self, values: &mut Vec<T>, value: T) {
        let required = values
            .len()
            .checked_add(1)
            .unwrap_or_else(|| invalid("scratch byte limit"));
        self.reserve(values, required);
        values.push(value);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Field {
    id: u32,
    child: i64,
}
struct Node {
    op: i64,
    child: i64,
    fields: Vec<Field>,
    hash: u64,
    // Compound levels only: an unused empty-vector scalar element does not
    // consume a value visit. Source value depths are checked by `visit`.
    height: usize,
}
struct Builder {
    op: i64,
    fields: Vec<Field>,
    height: usize,
    previous: i64,
    // Includes repeated source elements skipped by a primitive collector.
    count: usize,
}
struct Joiner {
    // Each occupied level represents a contiguous block twice the size of
    // the previous level. Carrying equal-sized blocks prevents a growing
    // variant union from retaining a quadratic number of field entries.
    levels: Vec<i64>,
    last: i64,
}
// Unary keys use a negative opcode; record-union keys use nonnegative node
// IDs. Exact key checks separate both namespaces, including cache collisions.
#[derive(Clone, Copy)]
struct ShapeMemo {
    a: i64,
    b: i64,
    result: i64,
}
const SHAPE_MEMOS: usize = 32;
struct Context {
    scratch: Scratch,
    visits: usize,
    nodes: Vec<Node>,
    slots: Vec<usize>,
    builders: Vec<Builder>,
    building: usize,
    joins: Vec<Vec<Field>>,
    shape_memos: Option<Box<[ShapeMemo; SHAPE_MEMOS]>>,
    joiners: Vec<Joiner>,
    joining: usize,
    roots: Vec<i64>,
    wire_ids: Vec<i64>,
    table: Vec<i64>,
    header_size: Option<usize>,
}

fn primitive(t: i64) -> bool {
    (-17..=-1).contains(&t) || t == -24
}
fn index(value: u64) -> usize {
    usize::try_from(value).unwrap_or_else(|_| invalid("index range"))
}
fn label(value: u64) -> u32 {
    u32::try_from(value).unwrap_or_else(|_| invalid("field ID range"))
}
// MurmurHash64A word rounds and final avalanche (Austin Appleby, public
// domain). Shape keys are whole words, so no byte-buffer or tail is needed.
// https://github.com/aappleby/smhasher/blob/master/src/MurmurHash2.cpp
const HASH_MULTIPLIER: u64 = 0xc6a4_a793_5bd1_e995;
fn mix(hash: &mut u64, word: u64) {
    let word = word.wrapping_mul(HASH_MULTIPLIER);
    let word = (word ^ (word >> 47)).wrapping_mul(HASH_MULTIPLIER);
    *hash = (*hash ^ word).wrapping_mul(HASH_MULTIPLIER);
}
fn shape_hash(op: i64, child: i64, fields: &[Field]) -> u64 {
    let bytes = (3 + 2 * fields.len()) as u64 * 8;
    let mut hash = bytes.wrapping_mul(HASH_MULTIPLIER);
    mix(&mut hash, op as u64);
    mix(&mut hash, child as u64);
    mix(&mut hash, fields.len() as u64);
    for field in fields {
        mix(&mut hash, u64::from(field.id));
        mix(&mut hash, field.child as u64);
    }
    hash ^= hash >> 47;
    hash = hash.wrapping_mul(HASH_MULTIPLIER);
    hash ^ (hash >> 47)
}

impl Context {
    fn new() -> Self {
        let mut scratch = Scratch {
            used: 0,
            limit: MAX_SCRATCH,
        };
        scratch.charge::<Self>(1);
        Self {
            scratch,
            visits: 0,
            nodes: Vec::new(),
            slots: Vec::new(),
            builders: Vec::new(),
            building: 0,
            joins: Vec::new(),
            shape_memos: None,
            joiners: Vec::new(),
            joining: 0,
            roots: Vec::new(),
            wire_ids: Vec::new(),
            table: Vec::new(),
            header_size: None,
        }
    }
    fn open(&self) {
        if self.header_size.is_some() {
            invalid("shape context is finalized");
        }
    }
    fn check(&self, t: i64) {
        if t < 0 {
            if !primitive(t) {
                invalid("unsupported shape reference");
            }
        } else if t as u64 >= self.nodes.len() as u64 {
            invalid("shape reference range");
        }
    }
    fn op(&self, t: i64) -> i64 {
        if t < 0 { t } else { self.nodes[t as usize].op }
    }
    fn height(&self, t: i64) -> usize {
        if t < 0 {
            0
        } else {
            self.nodes[t as usize].height
        }
    }
    fn visit(&mut self, depth: u64, count: u64) {
        if depth > MAX_DEPTH as u64 {
            invalid("value nesting limit");
        }
        self.visits = self
            .visits
            .checked_add(index(count))
            .filter(|visits| *visits <= MAX_ITEMS)
            .unwrap_or_else(|| invalid("value traversal limit"));
    }
    fn rebuild_slots(&mut self, size: usize) {
        let mut slots = Vec::new();
        self.scratch.reserve(&mut slots, size);
        slots.resize(size, 0);
        for (id, node) in self.nodes.iter().enumerate() {
            let mut slot = node.hash as usize & (size - 1);
            while slots[slot] != 0 {
                slot = (slot + 1) & (size - 1);
            }
            slots[slot] = id + 1;
        }
        self.slots = slots;
    }
    #[inline(always)]
    fn intern(&mut self, op: i64, child: i64, fields: &[Field]) -> i64 {
        self.open();
        let mut height = 1;
        match op {
            -18 | -19 => {
                if !fields.is_empty() {
                    invalid("invalid unary shape");
                }
                self.check(child);
                height += self.height(child);
            }
            -20 | -21 => {
                if child != MISSING || fields.len() > MAX_ITEMS {
                    invalid("invalid field shape");
                }
                let mut previous = None;
                for field in fields {
                    if previous.is_some_and(|id| id >= field.id) {
                        invalid("unordered field IDs");
                    }
                    previous = Some(field.id);
                    self.check(field.child);
                    height = height.max(self.height(field.child) + 1);
                }
            }
            _ => invalid("unsupported compound shape"),
        }
        self.intern_validated(op, child, fields, height)
    }
    // Callers establish valid child references, sorted field IDs and the
    // computed height while constructing the shape, in an open context.
    fn intern_validated(&mut self, op: i64, child: i64, fields: &[Field], height: usize) -> i64 {
        if fields.len() > MAX_ITEMS {
            invalid("invalid field shape");
        }
        if height > MAX_DEPTH + 1 {
            invalid("type nesting limit");
        }
        let hash = shape_hash(op, child, fields);
        if !self.slots.is_empty() {
            let mut slot = hash as usize & (self.slots.len() - 1);
            while self.slots[slot] != 0 {
                let id = self.slots[slot] - 1;
                let node = &self.nodes[id];
                if node.hash == hash
                    && node.op == op
                    && node.child == child
                    && node.fields == fields
                {
                    return id as i64;
                }
                slot = (slot + 1) & (self.slots.len() - 1);
            }
        }
        if self.nodes.len() + 1 > self.slots.len() / 2 {
            self.rebuild_slots(self.slots.len().saturating_mul(2).max(8));
        }
        let mut retained = Vec::new();
        if !fields.is_empty() {
            self.scratch.reserve(&mut retained, fields.len());
            retained.extend_from_slice(fields);
        }
        let id = self.nodes.len();
        self.scratch.push(
            &mut self.nodes,
            Node {
                op,
                child,
                fields: retained,
                hash,
                height,
            },
        );
        let mut slot = hash as usize & (self.slots.len() - 1);
        while self.slots[slot] != 0 {
            slot = (slot + 1) & (self.slots.len() - 1);
        }
        self.slots[slot] = id + 1;
        id as i64
    }
    fn begin(&mut self, op: i64) {
        self.open();
        if ![-20, -21].contains(&op) {
            invalid("invalid field builder opcode");
        }
        self.begin_builder(op);
    }
    fn begin_builder(&mut self, op: i64) {
        if self.building > MAX_DEPTH {
            invalid("type nesting limit");
        }
        if self.building == self.builders.len() {
            self.scratch.push(
                &mut self.builders,
                Builder {
                    op,
                    fields: Vec::new(),
                    height: 1,
                    previous: MISSING,
                    count: 0,
                },
            );
        }
        let builder = &mut self.builders[self.building];
        builder.op = op;
        builder.fields.clear();
        builder.height = 1;
        self.building += 1;
    }
    fn field(&mut self, id: u64, child: i64) {
        self.open();
        self.check(child);
        let id = label(id);
        let height = self.height(child) + 1;
        let builder = self
            .builders
            .get_mut(self.building.wrapping_sub(1))
            .unwrap_or_else(|| invalid("missing field builder"));
        if builder.op == PRIMITIVE_VARIANTS {
            invalid("invalid field builder mode");
        }
        if builder.fields.len() >= MAX_ITEMS {
            invalid("collection length limit");
        }
        if builder
            .fields
            .last()
            .is_some_and(|previous| previous.id >= id)
        {
            invalid("unordered field IDs");
        }
        self.scratch.push(&mut builder.fields, Field { id, child });
        builder.height = builder.height.max(height);
    }
    fn end(&mut self) -> i64 {
        self.open();
        if self.building == 0 {
            invalid("missing field builder");
        }
        if self.builders[self.building - 1].op == PRIMITIVE_VARIANTS {
            invalid("invalid field builder mode");
        }
        self.building -= 1;
        let builder = &mut self.builders[self.building];
        let op = builder.op;
        let height = builder.height;
        let fields = take(&mut builder.fields);
        let previous = builder.previous;
        // Repeated records at this nesting level often have the same shape.
        // Fields were checked as inserted; equality also preserves height.
        let result = if op == -20
            && previous != MISSING
            && self.nodes[previous as usize].op == op
            && self.nodes[previous as usize].fields == fields
        {
            previous
        } else {
            self.intern_validated(op, MISSING, &fields, height)
        };
        let builder = &mut self.builders[self.building];
        builder.fields = fields;
        builder.previous = result;
        result
    }
    fn variant(&mut self, id: u64, child: i64) -> i64 {
        self.open();
        self.check(child);
        let field = Field {
            id: label(id),
            child,
        };
        self.intern_validated(-21, MISSING, &[field], self.height(child) + 1)
    }
    fn variants_begin(&mut self) {
        self.open();
        self.begin_builder(PRIMITIVE_VARIANTS);
        self.builders[self.building - 1].count = 0;
    }
    fn variants_push(&mut self, id: u64, child: i64) {
        self.open();
        // The compiler uses this collector only when each source case has a
        // fixed primitive payload. Such a payload never needs a shape join.
        if !primitive(child) || child == -17 {
            invalid("invalid primitive variant payload");
        }
        let field = Field {
            id: label(id),
            child,
        };
        let builder = self
            .builders
            .get_mut(self.building.wrapping_sub(1))
            .unwrap_or_else(|| invalid("missing variant collector"));
        if builder.op != PRIMITIVE_VARIANTS {
            invalid("invalid variant collector mode");
        }
        if builder.count >= MAX_ITEMS {
            invalid("collection length limit");
        }
        builder.count += 1;
        // Only adjacent duplicates avoid storage. Cyclic repetitions remain
        // buffered until the final sort, so scratch is bounded by input size,
        // not merely by the number of distinct selected cases.
        if builder.fields.last() == Some(&field) {
            return;
        }
        self.scratch.push(&mut builder.fields, field);
    }
    fn variants_end(&mut self) -> i64 {
        self.open();
        let position = self
            .building
            .checked_sub(1)
            .unwrap_or_else(|| invalid("missing variant collector"));
        let builder = &mut self.builders[position];
        if builder.op != PRIMITIVE_VARIANTS {
            invalid("invalid variant collector mode");
        }
        let mut fields = take(&mut builder.fields);
        fields.sort_unstable_by_key(|field| field.id);
        fields.dedup_by(|later, earlier| {
            if earlier.id != later.id {
                return false;
            }
            if earlier.child != later.child {
                invalid("incompatible primitive variant payloads");
            }
            true
        });
        // Only the completed union survives: no singleton or intermediate
        // union nodes are needed for this array's element type.
        let result = if fields.is_empty() {
            -17
        } else {
            self.intern_validated(-21, MISSING, &fields, 1)
        };
        let builder = &mut self.builders[position];
        builder.fields = fields;
        builder.previous = MISSING;
        self.building = position;
        result
    }
    fn unary(&mut self, op: i64, child: i64) -> i64 {
        // Options commonly repeat. Cache vectors after the table exists,
        // avoiding an allocation for a lone outer vector or binary value.
        if op != -18 && (op != -19 || self.shape_memos.is_none()) {
            return self.intern(op, child, &[]);
        }
        self.open();
        let key = ((op as u64).rotate_left(7) ^ child as u64).wrapping_mul(0x9e37_79b1_85eb_ca87);
        let slot = (key >> 32) as usize & (SHAPE_MEMOS - 1);
        if self.shape_memos.is_none() {
            self.scratch.charge::<[ShapeMemo; SHAPE_MEMOS]>(1);
            self.shape_memos = Some(Box::new(
                [ShapeMemo {
                    a: MISSING,
                    b: MISSING,
                    result: MISSING,
                }; SHAPE_MEMOS],
            ));
        }
        let cached = self.shape_memos.as_ref().unwrap()[slot];
        if cached.a == op && cached.b == child {
            return cached.result;
        }
        let result = self.intern(op, child, &[]);
        self.shape_memos.as_mut().unwrap()[slot] = ShapeMemo {
            a: op,
            b: child,
            result,
        };
        result
    }
    fn join(&mut self, a: i64, b: i64, depth: usize) -> i64 {
        self.open();
        self.check(a);
        self.check(b);
        if depth > MAX_DEPTH {
            invalid("type nesting limit");
        }
        if a == b || b == -17 {
            return a;
        }
        if a == -17 {
            return b;
        }
        let a_op = self.op(a);
        let b_op = self.op(b);
        if a == -1 {
            return if b_op == -18 { b } else { self.unary(-18, b) };
        }
        if b == -1 {
            return if a_op == -18 { a } else { self.unary(-18, a) };
        }
        if a_op != -20 || b_op != -20 {
            return self.join_compound(a, b, a_op, b_op, depth);
        }
        // Optional-field presence patterns recur in record vectors. Share
        // the bounded per-call memo with unary shapes.
        let (a, b) = if a < b { (a, b) } else { (b, a) };
        let key = ((a as u64).rotate_left(7) ^ b as u64).wrapping_mul(0x9e37_79b1_85eb_ca87);
        let slot = (key >> 32) as usize & (SHAPE_MEMOS - 1);
        if self.shape_memos.is_none() {
            self.scratch.charge::<[ShapeMemo; SHAPE_MEMOS]>(1);
            self.shape_memos = Some(Box::new(
                [ShapeMemo {
                    a: MISSING,
                    b: MISSING,
                    result: MISSING,
                }; SHAPE_MEMOS],
            ));
        }
        let cached = self.shape_memos.as_ref().unwrap()[slot];
        // Near the depth limit, replay the recursive checks: a shallow cache
        // entry must not bypass an inner join that would exceed MAX_DEPTH.
        if cached.a == a && cached.b == b && depth + self.height(cached.result) <= MAX_DEPTH {
            return cached.result;
        }
        let result = self.join_compound(a, b, a_op, b_op, depth);
        self.shape_memos.as_mut().unwrap()[slot] = ShapeMemo { a, b, result };
        result
    }
    // Keep the non-record path free of another function call and op loads.
    #[inline(always)]
    fn join_compound(&mut self, a: i64, b: i64, a_op: i64, b_op: i64, depth: usize) -> i64 {
        if a_op == -18 || b_op == -18 {
            let left = if a_op == -18 {
                self.nodes[a as usize].child
            } else {
                a
            };
            let right = if b_op == -18 {
                self.nodes[b as usize].child
            } else {
                b
            };
            let child = self.join(left, right, depth + 1);
            return self.unary(-18, child);
        }
        if a_op != b_op || ![-19, -20, -21].contains(&a_op) {
            invalid("incompatible value shapes");
        }
        if a_op == -19 {
            let left = self.nodes[a as usize].child;
            let right = self.nodes[b as usize].child;
            let child = self.join(left, right, depth + 1);
            return self.unary(-19, child);
        }
        while self.joins.len() <= depth {
            self.scratch.push(&mut self.joins, Vec::new());
        }
        let mut fields = take(&mut self.joins[depth]);
        fields.clear();
        let left_len = self.nodes[a as usize].fields.len();
        let right_len = self.nodes[b as usize].fields.len();
        let mut left = 0;
        let mut right = 0;
        let mut height = 1;
        while left < left_len || right < right_len {
            let l = self.nodes[a as usize].fields.get(left).copied();
            let r = self.nodes[b as usize].fields.get(right).copied();
            let (field, child) = match (l, r) {
                (Some(l), Some(r)) if l.id == r.id => {
                    left += 1;
                    right += 1;
                    (l, self.join(l.child, r.child, depth + 1))
                }
                (Some(l), r) if r.is_none_or(|r| l.id < r.id) => {
                    left += 1;
                    (
                        l,
                        if a_op == -20 {
                            self.join(l.child, -1, depth + 1)
                        } else {
                            l.child
                        },
                    )
                }
                (_, Some(r)) => {
                    right += 1;
                    (
                        r,
                        if a_op == -20 {
                            self.join(-1, r.child, depth + 1)
                        } else {
                            r.child
                        },
                    )
                }
                _ => invalid("invalid shape union"),
            };
            height = height.max(self.height(child) + 1);
            self.scratch.push(
                &mut fields,
                Field {
                    id: field.id,
                    child,
                },
            );
        }
        let result = self.intern_validated(a_op, MISSING, &fields, height);
        self.joins[depth] = fields;
        result
    }
    fn join_begin(&mut self) {
        self.open();
        if self.joining > MAX_DEPTH {
            invalid("type nesting limit");
        }
        if self.joining == self.joiners.len() {
            self.scratch.push(
                &mut self.joiners,
                Joiner {
                    levels: Vec::new(),
                    last: MISSING,
                },
            );
        }
        let joiner = &mut self.joiners[self.joining];
        joiner.levels.clear();
        joiner.last = MISSING;
        self.joining += 1;
    }
    fn join_push(&mut self, mut t: i64) {
        self.open();
        self.check(t);
        let position = self
            .joining
            .checked_sub(1)
            .unwrap_or_else(|| invalid("missing shape joiner"));
        // Identical adjacent elements have the same union. Avoid advancing
        // the reduction tree for long homogeneous runs (including scalars).
        if self.joiners[position].last == t {
            return;
        }
        self.joiners[position].last = t;
        let mut level = 0;
        loop {
            let levels = &mut self.joiners[position].levels;
            if level == levels.len() {
                self.scratch.push(levels, t);
                return;
            }
            let left = levels[level];
            if left == MISSING {
                levels[level] = t;
                return;
            }
            levels[level] = MISSING;
            t = self.join(left, t, 0);
            level += 1;
        }
    }
    fn join_end(&mut self) -> i64 {
        self.open();
        self.joining = self
            .joining
            .checked_sub(1)
            .unwrap_or_else(|| invalid("missing shape joiner"));
        let position = self.joining;
        let mut joined = -17;
        // Higher levels precede lower levels in source order. The join is a
        // least upper bound, so balancing changes only intermediate shapes.
        for level in (0..self.joiners[position].levels.len()).rev() {
            let next = self.joiners[position].levels[level];
            if next != MISSING {
                joined = self.join(joined, next, 0);
            }
        }
        joined
    }
    fn root(&mut self, t: i64) {
        self.open();
        self.check(t);
        if self.roots.len() >= MAX_ITEMS {
            invalid("collection length limit");
        }
        self.scratch.push(&mut self.roots, t);
    }
    fn root_type(&self, position: u64) -> i64 {
        *self
            .roots
            .get(index(position))
            .unwrap_or_else(|| invalid("root index range"))
    }
    fn child(&self, t: i64) -> i64 {
        self.check(t);
        if ![-18, -19].contains(&self.op(t)) {
            invalid("missing unary shape");
        }
        self.nodes[t as usize].child
    }
    fn field_type(&self, t: i64, id: u64) -> i64 {
        self.check(t);
        if ![-20, -21].contains(&self.op(t)) {
            invalid("missing field shape");
        }
        let fields = &self.nodes[t as usize].fields;
        fields
            .binary_search_by_key(&label(id), |field| field.id)
            .map(|position| fields[position].child)
            .unwrap_or(MISSING)
    }
    fn case_index(&self, t: i64, id: u64) -> u64 {
        self.check(t);
        if self.op(t) != -21 {
            invalid("missing variant shape");
        }
        self.nodes[t as usize]
            .fields
            .binary_search_by_key(&label(id), |field| field.id)
            .unwrap_or_else(|_| invalid("missing derived variant")) as u64
    }
    fn number(&mut self, t: i64, depth: usize) {
        if t < 0 {
            return;
        }
        let id = t as usize;
        if depth
            .checked_add(self.nodes[id].height)
            .is_none_or(|n| n > MAX_DEPTH + 1)
        {
            invalid("type nesting limit");
        }
        if self.wire_ids[id] != MISSING {
            return;
        }
        if self.table.len() >= MAX_TYPES {
            invalid("output type-table limit");
        }
        self.wire_ids[id] = self.table.len() as i64;
        self.scratch.push(&mut self.table, t);
        let op = self.nodes[id].op;
        if [-18, -19].contains(&op) {
            self.number(self.nodes[id].child, depth + 1);
        } else {
            for field in 0..self.nodes[id].fields.len() {
                self.number(self.nodes[id].fields[field].child, depth + 1);
            }
        }
    }
    fn reference(&self, t: i64) -> i64 {
        if t < 0 { t } else { self.wire_ids[t as usize] }
    }
    fn header(&self, out: &mut Header) {
        for byte in b"DIDL" {
            out.byte(*byte);
        }
        out.unsigned(self.table.len() as u64);
        for t in &self.table {
            let node = &self.nodes[*t as usize];
            out.signed(node.op);
            if [-18, -19].contains(&node.op) {
                out.signed(self.reference(node.child));
            } else {
                out.unsigned(node.fields.len() as u64);
                for field in &node.fields {
                    out.unsigned(u64::from(field.id));
                    out.signed(self.reference(field.child));
                }
            }
        }
        out.unsigned(self.roots.len() as u64);
        for root in &self.roots {
            out.signed(self.reference(*root));
        }
    }
    fn types_size(&mut self) -> usize {
        if let Some(size) = self.header_size {
            return size;
        }
        if self.building != 0 {
            invalid("unfinished field builder");
        }
        if self.joining != 0 {
            invalid("unfinished shape joiner");
        }
        self.scratch.reserve(&mut self.wire_ids, self.nodes.len());
        self.wire_ids.resize(self.nodes.len(), MISSING);
        for root in 0..self.roots.len() {
            self.number(self.roots[root], 0);
        }
        let mut out = Header::new(None);
        self.header(&mut out);
        self.header_size = Some(out.position);
        out.position
    }
    unsafe fn write_types(&mut self, ptr: usize) -> usize {
        let size = self.types_size();
        if ptr == 0 || ptr.checked_add(size).is_none() {
            invalid("header pointer range");
        }
        let mut out = Header::new(Some(ptr));
        self.header(&mut out);
        if out.position != size {
            invalid("header size changed");
        }
        out.position
    }
}

struct Header {
    ptr: Option<usize>,
    position: usize,
}
impl Header {
    fn new(ptr: Option<usize>) -> Self {
        Self { ptr, position: 0 }
    }
    fn byte(&mut self, byte: u8) {
        if self.position >= MAX_BYTES {
            invalid("output byte limit");
        }
        if let Some(ptr) = self.ptr {
            // Generated code supplies an exclusively owned final Blob with
            // capacity at least the checked header and payload size.
            unsafe { (ptr as *mut u8).add(self.position).write(byte) };
        }
        self.position += 1;
    }
    fn unsigned(&mut self, mut value: u64) {
        loop {
            let byte = (value & 127) as u8;
            value >>= 7;
            self.byte(byte | if value == 0 { 0 } else { 128 });
            if value == 0 {
                return;
            }
        }
    }
    fn signed(&mut self, mut value: i64) {
        loop {
            let byte = (value & 127) as u8;
            value >>= 7;
            let done = (value == 0 && byte < 64) || (value == -1 && byte >= 64);
            self.byte(byte | if done { 0 } else { 128 });
            if done {
                return;
            }
        }
    }
}

// These pointers refer only to compiler-created contexts. Generated code keeps
// serialization synchronous and calls `done` before any GC safepoint.
unsafe fn context<'a>(ctx: usize) -> &'a mut Context {
    if ctx == 0 {
        invalid("null shape context");
    }
    unsafe { &mut *(ctx as *mut Context) }
}

#[unsafe(no_mangle)]
pub(crate) extern "C" fn candid_canonical_new() -> usize {
    Box::into_raw(Box::new(Context::new())) as usize
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_visit(ctx: usize, depth: u64, count: u64) {
    unsafe { context(ctx).visit(depth, count) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_begin(ctx: usize, op: i64) {
    unsafe { context(ctx).begin(op) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_field(ctx: usize, id: u64, child: i64) {
    unsafe { context(ctx).field(id, child) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_end(ctx: usize) -> i64 {
    unsafe { context(ctx).end() }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_variant(ctx: usize, id: u64, child: i64) -> i64 {
    unsafe { context(ctx).variant(id, child) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_variants_begin(ctx: usize) {
    unsafe { context(ctx).variants_begin() }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_variants_push(ctx: usize, id: u64, child: i64) {
    unsafe { context(ctx).variants_push(id, child) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_variants_end(ctx: usize) -> i64 {
    unsafe { context(ctx).variants_end() }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_unary(ctx: usize, op: i64, child: i64) -> i64 {
    unsafe { context(ctx).unary(op, child) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_join(ctx: usize, a: i64, b: i64) -> i64 {
    unsafe { context(ctx).join(a, b, 0) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_join_begin(ctx: usize) {
    unsafe { context(ctx).join_begin() }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_join_push(ctx: usize, t: i64) {
    unsafe { context(ctx).join_push(t) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_join_end(ctx: usize) -> i64 {
    unsafe { context(ctx).join_end() }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_root(ctx: usize, t: i64) {
    unsafe { context(ctx).root(t) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_root_type(ctx: usize, index: u64) -> i64 {
    unsafe { context(ctx).root_type(index) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_types_size(ctx: usize) -> u64 {
    unsafe { context(ctx).types_size() as u64 }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_write_types(ctx: usize, ptr: usize) -> u64 {
    unsafe { context(ctx).write_types(ptr) as u64 }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_child(ctx: usize, t: i64) -> i64 {
    unsafe { context(ctx).child(t) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_field_type(ctx: usize, t: i64, id: u64) -> i64 {
    unsafe { context(ctx).field_type(t, id) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_case_index(ctx: usize, t: i64, id: u64) -> u64 {
    unsafe { context(ctx).case_index(t, id) }
}
#[unsafe(no_mangle)]
pub(crate) unsafe extern "C" fn candid_canonical_done(ctx: usize) {
    let current = unsafe { context(ctx) };
    if current.building != 0 {
        invalid("unfinished field builder");
    }
    if current.joining != 0 {
        invalid("unfinished shape joiner");
    }
    unsafe { drop(Box::from_raw(ctx as *mut Context)) };
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use std::panic::{AssertUnwindSafe, catch_unwind};

    fn compound(ctx: &mut Context, op: i64, fields: &[(u32, i64)]) -> i64 {
        ctx.begin(op);
        for (id, child) in fields {
            ctx.field(u64::from(*id), *child);
        }
        ctx.end()
    }
    fn bytes(ctx: &mut Context) -> Vec<u8> {
        let size = ctx.types_size();
        let mut output = vec![0xaa; size + 1];
        assert_eq!(
            unsafe { ctx.write_types(output.as_mut_ptr() as usize) },
            size
        );
        assert_eq!(output.pop(), Some(0xaa));
        output
    }
    fn rejects(f: impl FnOnce()) {
        assert!(catch_unwind(AssertUnwindSafe(f)).is_err());
    }

    fn primitive_variants(ctx: &mut Context, fields: &[(u32, i64)]) -> i64 {
        ctx.variants_begin();
        for (id, child) in fields {
            ctx.variants_push(u64::from(*id), *child);
        }
        ctx.variants_end()
    }

    #[test]
    fn primitive_variant_collections_match_general_unions_and_canonical_headers() {
        let primitives: Vec<_> = (-16..=-1).chain(core::iter::once(-24)).collect();
        for length in 0..128 {
            let mut general = Context::new();
            let mut expected = -17;
            let mut fields = Vec::new();
            for i in 0..length {
                let case = (i * 17 + length * 11) % 23;
                let id = (case as u32).wrapping_mul(0x1000_0001);
                let child = primitives[case % primitives.len()];
                fields.push((id, child));
                let singleton = general.variant(u64::from(id), child);
                expected = general.join(expected, singleton, 0);
            }
            let expected = general.unary(-19, expected);
            general.root(expected);
            let mut collected = Context::new();
            let variant = primitive_variants(&mut collected, &fields);
            assert_eq!(collected.nodes.len(), usize::from(length != 0));
            let vector = collected.unary(-19, variant);
            collected.root(vector);
            assert_eq!(bytes(&mut collected), bytes(&mut general));
        }
    }

    #[test]
    fn primitive_variant_collections_reuse_storage_and_preserve_nested_builders() {
        let mut ctx = Context::new();
        ctx.begin(-20);
        ctx.variants_begin();
        ctx.variants_push(u64::from(u32::MAX), -24);
        let inner = primitive_variants(&mut ctx, &[(1, -2), (0, -1), (1, -2)]);
        assert_eq!(ctx.case_index(inner, 1), 1);
        ctx.variants_push(0, -1);
        let outer = ctx.variants_end();
        assert_eq!(ctx.field_type(outer, u64::from(u32::MAX)), -24);
        ctx.field(0, outer);
        ctx.field(1, inner);
        let record = ctx.end();
        assert_eq!(ctx.field_type(record, 0), outer);
        assert_eq!(ctx.field_type(record, 1), inner);
        let fields = [(9, -3), (2, -16), (9, -3), (0, -1)];
        let wanted = primitive_variants(&mut ctx, &fields);
        let allocated = ctx.scratch.used;
        for _ in 0..32 {
            assert_eq!(primitive_variants(&mut ctx, &fields), wanted);
            assert_eq!(primitive_variants(&mut ctx, &[]), -17);
        }
        assert_eq!(ctx.scratch.used, allocated);
        // A collector must not leave a negative Empty reference in the
        // record builder's previous-node cache when this level is reused.
        assert_eq!(compound(&mut ctx, -20, &[(0, outer), (1, inner)]), record);
    }

    #[test]
    fn primitive_variant_collections_enforce_payload_label_and_lifecycle_checks() {
        let mut ctx = Context::new();
        rejects(|| ctx.variants_push(0, -1));
        rejects(|| {
            ctx.variants_end();
        });
        let compound = ctx.unary(-18, -3);
        ctx.variants_begin();
        rejects(|| ctx.variants_push(0, compound));
        for invalid_child in [-17, -18, -19, -20, -21, -22, -23, -25, 0, MISSING] {
            rejects(|| ctx.variants_push(0, invalid_child));
        }
        rejects(|| ctx.variants_push(u64::from(u32::MAX) + 1, -1));
        rejects(|| ctx.field(0, -1));
        rejects(|| {
            ctx.end();
        });
        rejects(|| {
            ctx.types_size();
        });
        assert_eq!(ctx.variants_end(), -17);
        ctx.begin(-20);
        rejects(|| ctx.variants_push(0, -1));
        rejects(|| {
            ctx.variants_end();
        });
        ctx.end();
        ctx.types_size();
        rejects(|| ctx.variants_begin());
        rejects(|| ctx.variants_push(0, -1));
        rejects(|| {
            ctx.variants_end();
        });
        let mut ctx = Context::new();
        ctx.variants_begin();
        ctx.variants_push(7, -1);
        ctx.variants_push(8, -2);
        ctx.variants_push(7, -3);
        rejects(|| {
            ctx.variants_end();
        });
    }

    #[test]
    fn primitive_variant_collections_check_nesting_visits_length_and_scratch() {
        let mut ctx = Context::new();
        for _ in 0..=MAX_DEPTH {
            ctx.variants_begin();
        }
        rejects(|| ctx.variants_begin());
        for _ in 0..=MAX_DEPTH {
            assert_eq!(ctx.variants_end(), -17);
        }
        rejects(|| ctx.visit((MAX_DEPTH + 1) as u64, 0));
        ctx.visit(2, MAX_ITEMS as u64);
        rejects(|| ctx.visit(2, 1));
        let mut ctx = Context::new();
        ctx.scratch.limit = ctx.scratch.used;
        rejects(|| ctx.variants_begin());
        let mut ctx = Context::new();
        ctx.variants_begin();
        ctx.scratch.limit = ctx.scratch.used;
        rejects(|| ctx.variants_push(0, -1));
        let mut ctx = Context::new();
        ctx.variants_begin();
        for _ in 0..MAX_ITEMS {
            ctx.variants_push(0, -1);
        }
        // The source count is still enforced when identical adjacent pairs
        // consume no additional scratch storage.
        assert!(ctx.scratch.used < 2048);
        rejects(|| ctx.variants_push(0, -1));
        let shape = ctx.variants_end();
        assert_eq!(ctx.nodes[shape as usize].fields.len(), 1);
    }

    #[test]
    fn wide_primitive_variant_collections_retain_only_the_final_union() {
        let mut ctx = Context::new();
        ctx.variants_begin();
        for id in (0..6000).rev() {
            ctx.variants_push(id, -1);
        }
        let variant = ctx.variants_end();
        assert_eq!(ctx.nodes.len(), 1);
        for id in 0..6000 {
            assert_eq!(ctx.case_index(variant, id), id);
        }
        let vector = ctx.unary(-19, variant);
        ctx.root(vector);
        assert!(bytes(&mut ctx).len() < 18_000);
        // The general balanced collector retains ~4.8 MiB of intermediate
        // shapes here. This path retains a growing buffer and one field list.
        assert!(ctx.scratch.used < 400 * 1024);
    }

    #[test]
    fn largest_valid_cyclic_primitive_variant_array_stays_below_scratch_limit() {
        let mut ctx = Context::new();
        let count = (MAX_ITEMS - 1) / 2;
        ctx.visit(0, 1);
        ctx.visit(1, count as u64);
        ctx.visit(2, count as u64);
        ctx.variants_begin();
        for i in 0..count {
            ctx.variants_push(((i * 17) % 64) as u64, -1);
        }
        let variant = ctx.variants_end();
        assert_eq!(ctx.nodes.len(), 1);
        assert_eq!(ctx.nodes[variant as usize].fields.len(), 64);
        let vector = ctx.unary(-19, variant);
        ctx.root(vector);
        assert!(bytes(&mut ctx).len() < 160);
        // Unlike adjacent duplicates, cyclic cases are buffered until sort.
        // Account for every old growth buffer near the source visit limit.
        assert!(ctx.scratch.used < 34 * 1024 * 1024);
    }

    #[test]
    fn reused_builders_validate_current_fields_and_reset_height() {
        let mut ctx = Context::new();
        let original = compound(&mut ctx, -20, &[(7, -3)]);
        assert_eq!(compound(&mut ctx, -20, &[(7, -3)]), original);
        let variant = compound(&mut ctx, -21, &[(7, -3)]);
        assert_ne!(variant, original);
        assert_eq!(compound(&mut ctx, -20, &[(7, -3)]), original);
        assert_ne!(compound(&mut ctx, -20, &[(8, -3)]), original);
        let optional = ctx.unary(-18, -3);
        assert_ne!(compound(&mut ctx, -20, &[(7, optional)]), original);
        ctx.begin(-20);
        rejects(|| {
            ctx.field(7, -22);
        });
        let empty = ctx.end();
        assert_eq!(ctx.height(empty), 1);
        let mut child = -1;
        for _ in 0..MAX_DEPTH {
            child = ctx.unary(-19, child);
        }
        let deep = compound(&mut ctx, -20, &[(0, child)]);
        assert_eq!(ctx.height(deep), MAX_DEPTH + 1);
        let too_deep = ctx.unary(-18, child);
        rejects(|| {
            compound(&mut ctx, -20, &[(0, too_deep)]);
        });
        assert_eq!(compound(&mut ctx, -20, &[]), empty);
        // Missing record fields lift a nonoptional child; their new parent
        // height must be checked even though both input records are valid.
        rejects(|| {
            ctx.join(deep, empty, 0);
        });
    }

    #[test]
    fn unary_memo_does_not_bypass_reference_depth_or_finalization_checks() {
        let mut ctx = Context::new();
        let opt = ctx.unary(-18, -3);
        let vec = ctx.unary(-19, -3);
        assert_eq!(ctx.unary(-18, -3), opt);
        assert_eq!(ctx.unary(-19, -3), vec);
        rejects(|| {
            ctx.unary(-18, -22);
        });
        rejects(|| {
            ctx.unary(-19, MISSING);
        });
        let mut child = -1;
        for _ in 0..=MAX_DEPTH {
            child = ctx.unary(-18, child);
        }
        rejects(|| {
            ctx.unary(-18, child);
        });
        ctx.root(opt);
        ctx.types_size();
        rejects(|| {
            ctx.unary(-18, -3);
        });
        rejects(|| {
            ctx.unary(-19, -3);
        });
        let mut ctx = Context::new();
        ctx.scratch.limit = ctx.scratch.used;
        rejects(|| {
            ctx.unary(-18, -3);
        });
    }

    #[test]
    fn singleton_variants_match_builders_and_preserve_outer_record_builder() {
        let mut ctx = Context::new();
        let option = ctx.unary(-18, -3);
        let empty = ctx.unary(-19, -17);
        let record = compound(&mut ctx, -20, &[(7, -3)]);
        for child in [-1, -3, -5, option, empty, record] {
            for id in [0, 1, 128, u32::MAX] {
                let expected = compound(&mut ctx, -21, &[(id, child)]);
                assert_eq!(ctx.variant(u64::from(id), child), expected);
                assert_eq!(ctx.case_index(expected, u64::from(id)), 0);
                assert_eq!(ctx.field_type(expected, u64::from(id)), child);
            }
        }
        ctx.begin(-20);
        let nested = ctx.variant(123, -1);
        ctx.field(7, nested);
        let record = ctx.end();
        assert_eq!(ctx.field_type(record, 7), nested);
        ctx.root(record);
        assert_eq!(
            bytes(&mut ctx),
            b"DIDL\x02\x6c\x01\x07\x01\x6b\x01\x7b\x7f\x01\x00"
        );
    }

    #[test]
    fn singleton_variants_enforce_reference_label_depth_and_lifecycle_limits() {
        let mut ctx = Context::new();
        rejects(|| {
            ctx.variant(0, -22);
        });
        rejects(|| {
            ctx.variant(u64::from(u32::MAX) + 1, -1);
        });
        let mut child = -1;
        for _ in 0..MAX_DEPTH {
            child = ctx.variant(0, child);
        }
        child = ctx.variant(0, child);
        rejects(|| {
            ctx.variant(0, child);
        });
        ctx.root(child);
        ctx.types_size();
        rejects(|| {
            ctx.variant(0, -1);
        });
        let mut ctx = Context::new();
        ctx.scratch.limit = ctx.scratch.used;
        rejects(|| {
            ctx.variant(0, -1);
        });
    }

    #[test]
    fn cached_record_joins_recheck_relative_nesting_limits() {
        let mut ctx = Context::new();
        let opt = ctx.unary(-18, -3);
        let left = compound(&mut ctx, -20, &[(0, -3)]);
        let right = compound(&mut ctx, -20, &[(0, opt)]);
        assert_eq!(ctx.join(left, right, 0), right);
        for wrappers in [MAX_DEPTH - 2, MAX_DEPTH - 1] {
            let mut a = left;
            let mut b = right;
            for _ in 0..wrappers {
                a = ctx.unary(-18, a);
                b = ctx.unary(-18, b);
            }
            if wrappers == MAX_DEPTH - 2 {
                assert_eq!(ctx.join(a, b, 0), b);
            } else {
                rejects(|| {
                    ctx.join(a, b, 0);
                });
            }
        }
        assert_eq!(ctx.shape_memos.as_ref().unwrap().len(), SHAPE_MEMOS);
    }

    #[test]
    fn memo_storage_is_lazy_and_reused_by_records_and_options() {
        let mut ctx = Context::new();
        let a = compound(&mut ctx, -21, &[(0, -1)]);
        let b = compound(&mut ctx, -21, &[(1, -1)]);
        let variants = ctx.join(a, b, 0);
        assert!(ctx.shape_memos.is_none());
        ctx.unary(-19, variants);
        assert!(ctx.shape_memos.is_none());
        let a = ctx.unary(-18, a);
        let b = ctx.unary(-18, b);
        let wanted = ctx.unary(-18, variants);
        assert_eq!(ctx.join(a, b, 0), wanted);
        assert!(ctx.shape_memos.is_some());
        let a = compound(&mut ctx, -20, &[(0, -1)]);
        let b = compound(&mut ctx, -20, &[(1, -1)]);
        let wanted = ctx.join(a, b, 0);
        assert_eq!(ctx.shape_memos.as_ref().unwrap().len(), SHAPE_MEMOS);
        let scratch = ctx.scratch.used;
        for _ in 0..1024 {
            assert_eq!(ctx.join(a, b, 0), wanted);
            assert_eq!(ctx.join(b, a, 0), wanted);
        }
        assert_eq!(ctx.scratch.used, scratch);
    }

    #[test]
    fn interning_spreads_structured_field_labels_across_the_table() {
        let mut ctx = Context::new();
        for family in 0..4 {
            for id in 0u32..8192 {
                let id = match family {
                    0 => id,
                    1 => id.rotate_left(16),
                    2 => id.wrapping_mul(0x10001),
                    _ => id.wrapping_mul(223),
                };
                compound(&mut ctx, -21, &[(id, -1)]);
            }
        }
        let mut total_probes = 0;
        let mut longest_probe = 0;
        for (id, node) in ctx.nodes.iter().enumerate() {
            let mut slot = node.hash as usize & (ctx.slots.len() - 1);
            let mut probes = 1;
            while ctx.slots[slot] != id + 1 {
                assert_ne!(ctx.slots[slot], 0);
                slot = (slot + 1) & (ctx.slots.len() - 1);
                probes += 1;
            }
            total_probes += probes;
            longest_probe = longest_probe.max(probes);
        }
        assert!(total_probes < 4 * ctx.nodes.len());
        assert!(longest_probe < 128);
    }

    #[test]
    fn frozen_headers_and_dfs_numbering_are_independent_of_intern_order() {
        let mut ctx = Context::new();
        assert_eq!(bytes(&mut ctx), b"DIDL\0\0");
        let mut ctx = Context::new();
        ctx.root(-3);
        assert_eq!(bytes(&mut ctx), b"DIDL\0\x01\x7d");
        let mut ctx = Context::new();
        let blob = ctx.unary(-19, -5);
        ctx.root(blob);
        assert_eq!(bytes(&mut ctx), b"DIDL\x01\x6d\x7b\x01\x00");
        let mut ctx = Context::new();
        let opt = ctx.unary(-18, -1);
        let record = compound(&mut ctx, -20, &[(0, -1), (1, -16), (3, opt)]);
        ctx.root(record);
        assert_eq!(
            bytes(&mut ctx),
            b"DIDL\x02\x6c\x03\x00\x7f\x01\x70\x03\x01\x6e\x7f\x01\x00"
        );
        let mut ctx = Context::new();
        let opt = ctx.unary(-18, -3);
        let record = compound(&mut ctx, -20, &[(0, opt)]);
        ctx.root(record);
        ctx.root(opt);
        assert_eq!(
            bytes(&mut ctx),
            b"DIDL\x02\x6c\x01\x00\x01\x6e\x7d\x02\x00\x01"
        );
    }

    #[test]
    fn join_retains_scalar_option_and_empty_vector_rules() {
        let mut ctx = Context::new();
        let opt = ctx.unary(-18, -3);
        assert_eq!(ctx.join(-1, -3, 0), opt);
        assert_eq!(ctx.join(-3, -1, 0), opt);
        assert_eq!(ctx.join(-1, opt, 0), opt);
        assert_eq!(ctx.join(opt, -3, 0), opt);
        assert_eq!(ctx.join(-17, opt, 0), opt);
        assert_eq!(ctx.join(opt, -17, 0), opt);
        let empty = ctx.unary(-19, -17);
        let blob = ctx.unary(-19, -5);
        assert_eq!(ctx.join(empty, blob, 0), blob);
        let opt_null = ctx.unary(-18, -1);
        let nested = ctx.join(opt_null, opt, 0);
        assert_eq!(ctx.child(nested), opt);
        rejects(|| {
            ctx.join(-3, -8, 0);
        });
    }

    #[test]
    fn records_lift_missing_fields_and_variants_union_selected_cases() {
        let mut ctx = Context::new();
        let a = compound(&mut ctx, -20, &[(0, -15)]);
        let opt = ctx.unary(-18, -15);
        let b = compound(&mut ctx, -20, &[(0, -15), (9, opt)]);
        assert_eq!(ctx.join(a, b, 0), b);
        assert_eq!(ctx.join(b, a, 0), b);
        let c = compound(&mut ctx, -20, &[(9, -15)]);
        let joined = ctx.join(a, c, 0);
        assert_eq!(ctx.field_type(joined, 0), opt);
        assert_eq!(ctx.field_type(joined, 9), opt);
        assert_eq!(ctx.field_type(joined, 10), MISSING);
        let a = compound(&mut ctx, -21, &[(91, -1)]);
        let b = compound(&mut ctx, -21, &[(3, -2)]);
        let joined = ctx.join(a, b, 0);
        assert_eq!(ctx.case_index(joined, 3), 0);
        assert_eq!(ctx.case_index(joined, 91), 1);
        assert_eq!(ctx.field_type(joined, 91), -1);
        assert_eq!(ctx.join(joined, a, 0), joined);
    }

    fn balanced(ctx: &mut Context, shapes: &[i64]) -> i64 {
        ctx.join_begin();
        for shape in shapes {
            ctx.join_push(*shape);
        }
        ctx.join_end()
    }

    #[test]
    fn balanced_joins_match_sequential_joins_of_nested_optional_shapes() {
        let mut ctx = Context::new();
        let nat = ctx.unary(-18, -3);
        let present_absent = ctx.unary(-18, -1);
        let present_present = ctx.unary(-18, nat);
        let mut shapes = vec![];
        for value in 0..27 {
            let mut states = value;
            ctx.begin(-20);
            for id in 0..3 {
                match states % 3 {
                    0 => (),
                    1 => ctx.field(id, present_absent),
                    2 => ctx.field(id, present_present),
                    _ => unreachable!(),
                }
                states /= 3;
            }
            shapes.push(ctx.end());
        }
        // All triples exercise reassociation through every combination of
        // omitted fields and the two nested option levels, not just pairs.
        for a in &shapes {
            for b in &shapes {
                for c in &shapes {
                    let left = ctx.join(*a, *b, 0);
                    let expected = ctx.join(left, *c, 0);
                    assert_eq!(balanced(&mut ctx, &[*a, *b, *c]), expected);
                    let right = ctx.join(*b, *c, 0);
                    assert_eq!(ctx.join(*a, right, 0), expected);
                }
            }
        }
        let empty = ctx.unary(-19, -17);
        let mut cases = vec![compound(&mut ctx, -21, &[(200, empty)])];
        for record in shapes {
            for id in [3, 91] {
                let vector = ctx.unary(-19, record);
                cases.push(compound(&mut ctx, -21, &[(id, vector)]));
            }
        }
        for length in 0..128 {
            let mut sequence = vec![];
            let mut expected = -17;
            for i in 0..length {
                let shape = cases[(i * 17 + length * 11) % cases.len()];
                sequence.push(shape);
                expected = ctx.join(expected, shape, 0);
            }
            assert_eq!(balanced(&mut ctx, &sequence), expected);
        }
    }

    #[test]
    fn balanced_joins_reuse_scratch_and_keep_nested_accumulators_separate() {
        let mut ctx = Context::new();
        let a = compound(&mut ctx, -21, &[(0, -1)]);
        let b = compound(&mut ctx, -21, &[(1, -1)]);
        let wanted = ctx.join(a, b, 0);
        ctx.join_begin();
        ctx.join_push(a);
        let scratch = ctx.scratch.used;
        for _ in 0..10_000 {
            ctx.join_push(a);
        }
        assert_eq!(ctx.scratch.used, scratch);
        let inner = balanced(&mut ctx, &[a, a, b, b, a]);
        assert_eq!(inner, wanted);
        ctx.join_push(inner);
        assert_eq!(ctx.join_end(), wanted);
        let scratch = ctx.scratch.used;
        for _ in 0..10 {
            assert_eq!(balanced(&mut ctx, &[a, a, b, b, a]), wanted);
        }
        assert_eq!(ctx.scratch.used, scratch);
        assert_eq!(balanced(&mut ctx, &[]), -17);
    }

    #[test]
    fn wide_variant_vectors_stay_below_the_scratch_limit() {
        let mut ctx = Context::new();
        ctx.join_begin();
        for id in 0..6000 {
            let shape = compound(&mut ctx, -21, &[(id, -1)]);
            ctx.join_push(shape);
        }
        let variant = ctx.join_end();
        for id in 0..6000 {
            assert_eq!(ctx.case_index(variant, id), id);
            assert_eq!(ctx.field_type(variant, id), -1);
        }
        let vector = ctx.unary(-19, variant);
        ctx.root(vector);
        let header = bytes(&mut ctx);
        assert_eq!(ctx.table.len(), 2);
        // The sequential reduction exceeds the 256 MiB hard limit here,
        // despite this vector's type header requiring less than 18 KiB.
        assert!(header.len() < 18_000);
        assert!(ctx.scratch.used < 8 * 1024 * 1024);
    }

    #[test]
    fn unfinished_and_missing_shape_accumulators_reject() {
        let mut ctx = Context::new();
        rejects(|| {
            ctx.join_push(-1);
        });
        rejects(|| {
            ctx.join_end();
        });
        ctx.join_begin();
        rejects(|| {
            ctx.types_size();
        });
        ctx.join_end();
        ctx.types_size();
        rejects(|| {
            ctx.join_begin();
        });
    }

    #[test]
    fn structurally_equal_builds_and_repeated_joins_reuse_scratch() {
        let mut ctx = Context::new();
        let a = compound(&mut ctx, -20, &[(0, -3), (1, -24)]);
        let b = compound(&mut ctx, -20, &[(0, -3)]);
        let joined = ctx.join(a, b, 0);
        let allocated = ctx.scratch.used;
        for _ in 0..1024 {
            assert_eq!(compound(&mut ctx, -20, &[(0, -3), (1, -24)]), a);
            assert_eq!(compound(&mut ctx, -20, &[(0, -3)]), b);
            assert_eq!(ctx.join(a, b, 0), joined);
            assert_eq!(ctx.join(joined, b, 0), joined);
        }
        assert_eq!(ctx.scratch.used, allocated);
        assert_eq!(ctx.nodes.len(), 4);
    }

    #[test]
    fn all_optional_presence_pairs_match_their_independent_expected_union() {
        let mut ctx = Context::new();
        let inner = ctx.unary(-18, -3);
        let present_absent = ctx.unary(-18, -1);
        let present_present = ctx.unary(-18, inner);
        let mut records = Vec::new();
        let mut vectors = Vec::new();
        for value in 0..27 {
            let mut states = value;
            ctx.begin(-20);
            for id in 0..3 {
                match states % 3 {
                    0 => (),
                    1 => ctx.field(id, present_absent),
                    2 => ctx.field(id, present_present),
                    _ => unreachable!(),
                }
                states /= 3;
            }
            let record = ctx.end();
            records.push(record);
            vectors.push(ctx.unary(-19, record));
        }
        for a in 0..27 {
            for b in 0..27 {
                let mut left = a;
                let mut right = b;
                let mut expected = 0;
                let mut place = 1;
                for _ in 0..3 {
                    expected += (left % 3).max(right % 3) * place;
                    left /= 3;
                    right /= 3;
                    place *= 3;
                }
                assert_eq!(ctx.join(records[a], records[b], 0), records[expected]);
                assert_eq!(ctx.join(vectors[a], vectors[b], 0), vectors[expected]);
                let left = compound(&mut ctx, -21, &[(3, records[a])]);
                let right = compound(&mut ctx, -21, &[(3, records[b])]);
                let wanted = compound(&mut ctx, -21, &[(3, records[expected])]);
                assert_eq!(ctx.join(left, right, 0), wanted);
                let right = compound(&mut ctx, -21, &[(91, records[b])]);
                let wanted = compound(&mut ctx, -21, &[(3, records[a]), (91, records[b])]);
                assert_eq!(ctx.join(left, right, 0), wanted);
            }
        }
    }

    #[test]
    fn only_reachable_types_count_toward_the_final_table_limit() {
        let mut ctx = Context::new();
        for id in 0..1500 {
            compound(&mut ctx, -20, &[(id, -1)]);
        }
        ctx.root(0);
        ctx.types_size();
        assert_eq!(ctx.table.len(), 1);
        let mut ctx = Context::new();
        for id in 0..1025 {
            let record = compound(&mut ctx, -20, &[(id, -1)]);
            ctx.root(record);
        }
        rejects(|| {
            ctx.types_size();
        });
    }

    #[test]
    fn bounds_include_old_growth_buffers_and_all_source_visits() {
        let mut scratch = Scratch {
            used: 0,
            limit: MAX_SCRATCH,
        };
        let mut values = Vec::<i64>::new();
        scratch.reserve(&mut values, 1);
        let first = scratch.used;
        values.push(1);
        scratch.reserve(&mut values, 2);
        assert_eq!(scratch.used, first + allocation_size::<i64>(2));
        scratch.limit = scratch.used;
        rejects(|| {
            scratch.reserve(&mut values, 3);
        });
        let mut ctx = Context::new();
        ctx.visit(256, 2_000_000);
        rejects(|| {
            ctx.visit(256, 1);
        });
        rejects(|| {
            Context::new().visit(257, 0);
        });
        let mut ctx = Context::new();
        let mut t = ctx.unary(-19, -5);
        for _ in 0..256 {
            t = ctx.unary(-18, t);
        }
        rejects(|| {
            ctx.unary(-18, t);
        });
        ctx.root(t);
        ctx.types_size();
        assert_eq!(ctx.table.len(), 257);
        let mut out = Header {
            ptr: None,
            position: MAX_BYTES - 1,
        };
        out.byte(0);
        assert_eq!(out.position, MAX_BYTES);
        rejects(|| {
            out.byte(0);
        });
    }

    #[test]
    fn unsigned_labels_and_signed_type_references_are_minimal() {
        let mut ctx = Context::new();
        let record = compound(&mut ctx, -20, &[(u32::MAX, -1)]);
        ctx.root(record);
        assert_eq!(
            bytes(&mut ctx),
            b"DIDL\x01\x6c\x01\xff\xff\xff\xff\x0f\x7f\x01\x00"
        );
        let mut output = [0; 2];
        let mut out = Header::new(Some(output.as_mut_ptr() as usize));
        out.signed(64);
        assert_eq!(out.position, 2);
        assert_eq!(output, [0xc0, 0]);
    }

    #[test]
    fn malformed_or_unfinished_builders_and_unsupported_shapes_reject() {
        let mut ctx = Context::new();
        rejects(|| {
            ctx.root(-22);
        });
        rejects(|| {
            ctx.unary(-20, -1);
        });
        ctx.begin(-20);
        ctx.field(2, -1);
        rejects(|| {
            ctx.field(1, -1);
        });
        rejects(|| {
            ctx.types_size();
        });
        ctx.end();
        ctx.types_size();
        rejects(|| {
            ctx.root(-1);
        });
    }

    #[test]
    fn abi_writes_into_the_caller_buffer_and_keeps_internal_refs() {
        unsafe {
            let ctx = candid_canonical_new();
            candid_canonical_visit(ctx, 0, 2);
            let opt = candid_canonical_unary(ctx, -18, -3);
            candid_canonical_begin(ctx, -20);
            candid_canonical_field(ctx, 0, opt);
            let record = candid_canonical_end(ctx);
            candid_canonical_root(ctx, record);
            assert_eq!(candid_canonical_root_type(ctx, 0), record);
            assert_eq!(candid_canonical_child(ctx, opt), -3);
            assert_eq!(candid_canonical_field_type(ctx, record, 0), opt);
            let size = candid_canonical_types_size(ctx) as usize;
            let mut buffer = vec![0; size];
            assert_eq!(
                candid_canonical_write_types(ctx, buffer.as_mut_ptr() as usize),
                size as u64
            );
            assert_eq!(buffer, b"DIDL\x02\x6c\x01\x00\x01\x6e\x7d\x01\x00");
            candid_canonical_done(ctx);
        }
    }
}
