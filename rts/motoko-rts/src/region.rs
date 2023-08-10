use crate::barriers::{allocation_barrier, init_with_barrier, write_with_barrier};
use crate::memory::{alloc_blob, Memory};
use crate::rts_trap_with;
use crate::trap_with_prefix;
use crate::types::{size_of, Blob, Bytes, Region, Value, TAG_REGION};

use motoko_rts_macros::ic_mem_fn;

unsafe fn region_trap_with(msg: &str) -> ! {
    trap_with_prefix("Region error: ", msg)
}

unsafe fn stable_memory_trap_with(msg: &str) -> ! {
    trap_with_prefix("StableMemory ", msg)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BlockId(pub u16);

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RegionId(pub u16);

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RegionSizeInPages(pub u64);

// Note: Use this type only in local variables, as it contains raw pointers
// that are not handled by the GCs.
#[derive(Clone)]
pub struct AccessVector(pub *mut Blob);

// Note: Use this type only in local variables, as it contains raw pointers
// that are not handled by the GCs.
#[derive(Clone)]
pub struct RegionObject(pub *mut Region);

const NIL_REGION_ID: u16 = 0;

// Mirrored field from stable memory, for handling upgrade logic.
pub(crate) static mut REGION_TOTAL_ALLOCATED_BLOCKS: u16 = 0;

// Scalar sentinel value recognized in the GC as "no root", i.e. (!`is_ptr()`).
// Same design like `continuation_table::TABLE`.
pub(crate) const NO_REGION: Value = Value::from_scalar(0);

// Region 0 -- classic API for stable memory, as a dedicated region.
pub(crate) static mut REGION_0: Value = NO_REGION;

// This impl encapsulates encoding of optional region IDs within a u16.
// Used by block-region table to encode the (optional) region ID of a block.
impl RegionId {
    pub fn id_is_nil(id: u16) -> bool {
        id == NIL_REGION_ID
    }
    pub fn from_id(id: u16) -> Self {
        RegionId(id)
    }
    pub fn from_u16(id: u16) -> Option<Self> {
        if Self::id_is_nil(id) {
            None
        } else {
            Some(RegionId(id - 1))
        }
    }
    pub fn into_u16(opreg: Option<RegionId>) -> u16 {
        match opreg {
            None => 0,
            Some(id) => {
                assert!(id.0 <= meta_data::max::REGIONS);
                id.0 + 1
            }
        }
    }
}

// This impl encapsulates encoding of optional region (sizes) within a u64.
// Used by region table to encode sizes of allocated regions (the size of which are Some(s)),
// and which regions are available to allocate (the size of which are None).
impl RegionSizeInPages {
    pub fn from_size_in_pages(s: u64) -> Self {
        RegionSizeInPages(s)
    }
    pub fn u64_is_nil(u: u64) -> bool {
        u == 0
    }
    pub fn from_u64(u: u64) -> Option<Self> {
        if Self::u64_is_nil(u) {
            None
        } else {
            Some(RegionSizeInPages(u - 1))
        }
    }
    pub fn into_u64(opreg: Option<RegionSizeInPages>) -> u64 {
        match opreg {
            None => 0,
            Some(s) => s.0 + 1,
        }
    }
}

impl AccessVector {
    pub unsafe fn from_value(v: &Value) -> Self {
        AccessVector(v.as_blob_mut())
    }

    pub unsafe fn set_ith_block_id(&self, i: u32, block_id: &BlockId) {
        let block_id_upper: u8 = (block_id.0 >> 8) as u8;
        let block_id_lower: u8 = block_id.0 as u8;

        // Update heap memory with new association. Little endian.
        // (write u16 to slot i in new_pages.)
        self.0.set(i * 2, block_id_lower);
        self.0.set(i * 2 + 1, block_id_upper);
    }

    pub unsafe fn get_ith_block_id(&self, i: u32) -> BlockId {
        let lower: u16 = self.0.get(i * 2) as u16;
        let upper: u16 = self.0.get(i * 2 + 1) as u16;
        BlockId(upper << 8 | lower)
    }
}

impl RegionObject {
    pub unsafe fn from_value(v: &Value) -> Self {
        RegionObject(v.as_region())
    }

    pub unsafe fn id(&self) -> RegionId {
        RegionId((*self.0).id)
    }

    pub unsafe fn trap_with(&self, msg: &str) -> ! {
        if (*self.0).id == 0 {
            stable_memory_trap_with(msg)
        } else {
            region_trap_with(msg)
        };
    }

    // Check both offset and [offset,.., offset + len) within bounds *)
    // c.f. StableMem.guard_range in compile.ml *)
    // TODO: simplify and specialize on len
    pub unsafe fn check_relative_range(&self, offset: u64, len: u64) {
        if len <= 1 {
            if offset >= ((*self.0).page_count as u64) * meta_data::size::PAGE_IN_BYTES {
                self.trap_with("offset out of bounds");
            }
        } else {
            if u64::MAX - len < offset {
                self.trap_with("range overflow")
            };
            if offset + len > (((*self.0).page_count as u64) * meta_data::size::PAGE_IN_BYTES) {
                self.trap_with("range out of bounds");
            };
        }
    }

    // Computes absolute offset, BlockId, and remaining length (of the
    // given block) for a relative offset.
    pub unsafe fn relative_into_absolute_info(&self, offset: u64) -> (u64, BlockId, u64) {
        let av = AccessVector::from_value(&(*self.0).vec_pages);

        // Which block (rank relative to this region)?
        let block_rank = offset / meta_data::size::BLOCK_IN_BYTES;

        // Where in that block?
        let intra_block_index = offset % meta_data::size::BLOCK_IN_BYTES;

        // Where is that block located in stable memory (global rank)?
        let block_id = av.get_ith_block_id(block_rank as u32);

        // address of the byte to load from stable memory:
        let offset = meta_data::offset::BLOCK_ZERO
            + block_id.0 as u64 * meta_data::size::BLOCK_IN_BYTES
            + intra_block_index;
        (
            offset,
            block_id,
            meta_data::size::BLOCK_IN_BYTES - intra_block_index,
        )
    }

    // compute the absolute offset, begin block, remaining length of
    // begin block, and end block for a relative offset and length.
    //
    // NB: BlockIds can be used to do case analysis (same or diff
    // block?) when planning successive reads/writes.
    pub unsafe fn relative_into_absolute_span(
        &self,
        offset: u64,
        len: u64,
    ) -> (u64, BlockId, u64, BlockId) {
        let (off, b1, b1_len) = self.relative_into_absolute_info(offset);
        if len == 0 {
            return (off, b1.clone(), b1_len, b1);
        };
        let final_offset = offset + len - 1;
        let (_, b2, _) = self.relative_into_absolute_info(final_offset);
        (off, b1, b1_len, b2)
    }
}

// Mutable meta data stored in stable memory header (See motoko/design/StableRegions.md)
mod meta_data {

    pub mod version {
        pub const MAGIC: &[u8; 8] = b"MOREGION";
        pub const VERSION: u32 = 2;
    }

    /// Maximum number of entities.
    pub mod max {
        pub const BLOCKS: u16 = 32 * 1024;
        pub const REGIONS: u16 = 32 * 1024 - 1;
    }

    /// Sizes of table entries, and tables.
    pub mod size {
        // Region table entry:
        // - 8 bytes for the size.
        // - 4 bytes for future use (including GC visit-marking).
        pub const REGION_TABLE_ENTRY: u16 = 12;

        pub const REGION_TABLE: u64 = super::max::REGIONS as u64 * REGION_TABLE_ENTRY as u64;

        pub const BLOCK_REGION_TABLE_ENTRY: u16 = 4;

        pub const BLOCK_REGION_TABLE: u64 =
            super::max::BLOCKS as u64 * BLOCK_REGION_TABLE_ENTRY as u64;

        pub const PAGES_IN_BLOCK: u32 = 128;
        pub const PAGE_IN_BYTES: u64 = 1 << 16;
        pub const BLOCK_IN_BYTES: u64 = PAGE_IN_BYTES * (PAGES_IN_BLOCK as u64);

        // Static memory footprint, ignoring any dynamically-allocated pages.
        pub const STATIC_MEM_IN_PAGES: u64 = PAGES_IN_BLOCK as u64; /* One block for meta data, plus future use TBD. */

        pub unsafe fn total_required_pages(total_allocated_blocks: u64) -> u64 {
            STATIC_MEM_IN_PAGES + (total_allocated_blocks * (PAGES_IN_BLOCK as u64))
        }
    }

    /// Offsets into stable memory for statically-sized fields and tables.
    pub mod offset {

        pub const MAGIC: u64 = 0;

        pub const VERSION: u64 = MAGIC + 8;

        pub const TOTAL_ALLOCATED_BLOCKS: u64 = VERSION + 4;

        pub const TOTAL_ALLOCATED_REGIONS: u64 = TOTAL_ALLOCATED_BLOCKS + 2;

        pub const BLOCK_REGION_TABLE: u64 = TOTAL_ALLOCATED_REGIONS + 2;

        pub const REGION_TABLE: u64 = BLOCK_REGION_TABLE + super::size::BLOCK_REGION_TABLE;

        pub const FREE: u64 = REGION_TABLE + super::size::REGION_TABLE;

        /* One block for meta data, plus future use TBD. */
        pub const BLOCK_ZERO: u64 = super::size::BLOCK_IN_BYTES;
    }

    pub mod total_allocated_blocks {
        use super::offset;
        use crate::ic0_stable::nicer::{read_u16, write_u16};

        use crate::region::REGION_TOTAL_ALLOCATED_BLOCKS;

        pub fn get() -> u64 {
            read_u16(offset::TOTAL_ALLOCATED_BLOCKS) as u64
        }
        pub fn set(n: u64) {
            // Here we keep these copies of the total in sync.
            //
            // NB. The non-stable one is used when the stable one is
            // unavailable (temp relocated by stable variable
            // serialization/deserialization).
            unsafe {
                REGION_TOTAL_ALLOCATED_BLOCKS = n as u16;
            };
            write_u16(offset::TOTAL_ALLOCATED_BLOCKS, n as u16)
        }
    }

    pub mod total_allocated_regions {
        use super::offset;
        use crate::ic0_stable::nicer::{read_u16, write_u16};

        pub fn get() -> u64 {
            read_u16(offset::TOTAL_ALLOCATED_REGIONS) as u64
        }
        pub fn set(n: u64) {
            write_u16(offset::TOTAL_ALLOCATED_REGIONS, n as u16)
        }
    }

    pub mod block_region_table {
        // invariant: all blocks whose IDs are below the total_allocated_blocks are valid.

        use super::{offset, size};
        use crate::ic0_stable::nicer::{read_u16, write_u16};
        use crate::region::{BlockId, RegionId};
        use core::mem::size_of;

        // Compute an offset in stable memory for a particular block ID (based zero).
        fn index(b: &BlockId) -> u64 {
            offset::BLOCK_REGION_TABLE + (b.0 as u64 * size::BLOCK_REGION_TABLE_ENTRY as u64)
        }

        /// Some(r, j) means that the block is in use by region r, at slot j.
        /// None means that the block is available for (re-)allocation.
        pub fn get(b: BlockId) -> Option<(RegionId, u16)> {
            let offset = index(&b);
            let raw = read_u16(offset);
            let rid = RegionId::from_u16(raw);
            rid.map(|r| (r, read_u16(offset + size_of::<u16>() as u64)))
        }

        /// Some(r, j) means that the block is in use by region r, at slot j.
        /// None means that the block is available for (re-)allocation.
        pub fn set(b: BlockId, r: Option<(RegionId, u16)>) {
            match r {
                None => write_u16(index(&b), RegionId::into_u16(None)),
                Some((r, j)) => {
                    let offset = index(&b);
                    write_u16(offset, RegionId::into_u16(Some(r)));
                    write_u16(offset + size_of::<u16>() as u64, j)
                }
            }
        }
    }

    pub mod region_table {
        use crate::region::{RegionId, RegionSizeInPages};

        // invariant (for now, pre-GC integration):
        //  all regions whose IDs are below the total_allocated_regions are valid.
        use super::{offset, size};
        use crate::ic0_stable::nicer::{read_u64, write_u64};

        fn index(r: &RegionId) -> u64 {
            offset::REGION_TABLE + r.0 as u64 * size::REGION_TABLE_ENTRY as u64
        }

        /// Some(_) gives the size in pages.
        /// None means that the region is not in use.
        pub fn get(r: &RegionId) -> Option<RegionSizeInPages> {
            RegionSizeInPages::from_u64(read_u64(index(r)))
        }

        pub fn set(r: &RegionId, s: Option<RegionSizeInPages>) {
            write_u64(index(r), RegionSizeInPages::into_u64(s))
        }
    }
}

fn write_magic() {
    use crate::ic0_stable::nicer::{size, write, write_u32};
    assert!(size() > 0);
    write(meta_data::offset::MAGIC, meta_data::version::MAGIC);
    write_u32(meta_data::offset::VERSION, meta_data::version::VERSION);
}

unsafe fn alloc_region<M: Memory>(
    mem: &mut M,
    id: u16,
    page_count: u32,
    vec_pages: Value,
) -> Value {
    let r_ptr = mem.alloc_words(size_of::<Region>());

    // NB. cannot use as_region() here as we didn't write the header yet
    let region = r_ptr.get_ptr() as *mut Region;
    (*region).header.tag = TAG_REGION;
    (*region).header.init_forward(r_ptr);
    (*region).id = id;
    // The padding must be initialized with zero because it is read by the compiler-generated code.
    (*region).zero_padding = 0;
    (*region).page_count = page_count;
    init_with_barrier(mem, &mut (*region).vec_pages, vec_pages);

    allocation_barrier(r_ptr);
    r_ptr
}

#[ic_mem_fn]
pub unsafe fn region_id<M: Memory>(_mem: &mut M, r: Value) -> u32 {
    let r = r.as_region();
    (*r).id.into()
}

// Helper for commmon logic that reserves low-valued RegionIds in a certain span for future use.
// When first is some, we are actually reserving.  When first is none, we are checking that the reservation has occured.
unsafe fn region_reserve_id_span<M: Memory>(_mem: &mut M, first: Option<RegionId>, last: RegionId) {
    if let Some(first) = first {
        let next_id = meta_data::total_allocated_regions::get() as u16;
        assert_eq!(first.0, next_id);
        assert!(first.0 <= last.0);
        meta_data::total_allocated_regions::set((last.0 + 1) as u64);
    } else {
        let next_id = meta_data::total_allocated_regions::get() as u16;
        assert!(last.0 < next_id);
    }
}

#[ic_mem_fn]
pub unsafe fn region_new<M: Memory>(mem: &mut M) -> Value {
    let next_id = meta_data::total_allocated_regions::get() as u16;

    if next_id == meta_data::max::REGIONS {
        region_trap_with("out of regions")
    };

    meta_data::total_allocated_regions::set(next_id as u64 + 1);

    let vec_pages = alloc_blob(mem, Bytes(0));
    allocation_barrier(vec_pages);
    let r_ptr = alloc_region(mem, next_id, 0, vec_pages);

    // Update Region table.
    {
        let r_id = RegionId::from_id(next_id);
        let c = meta_data::region_table::get(&r_id);
        // sanity check: Region table says that this region is available.
        assert_eq!(c, None);
        meta_data::region_table::set(&r_id, Some(RegionSizeInPages(0)));
    }
    r_ptr
}

pub unsafe fn region_recover<M: Memory>(mem: &mut M, rid: &RegionId) -> Value {
    use meta_data::size::PAGES_IN_BLOCK;
    let c = meta_data::region_table::get(&rid);
    let page_count = match c {
        Some(RegionSizeInPages(pc)) => pc,
        _ => {
            rts_trap_with("region_recover failed: Expected Some(RegionSizeInPages(_))");
        }
    };

    let block_count = (page_count as u32 + PAGES_IN_BLOCK - 1) / PAGES_IN_BLOCK;
    let vec_pages = alloc_blob(mem, Bytes(block_count * 2));

    let tb = meta_data::total_allocated_blocks::get();
    let av = AccessVector(vec_pages.as_blob_mut());
    let mut recovered_blocks = 0;
    let mut block_id = 0;
    while recovered_blocks < block_count && block_id < tb {
        match meta_data::block_region_table::get(BlockId(block_id as u16)) {
            None => {}
            Some((rid_, rank)) => {
                if &rid_ == rid {
                    av.set_ith_block_id(rank.into(), &BlockId(block_id as u16));
                    recovered_blocks += 1;
                }
            }
        }
        block_id += 1;
    }
    assert_eq!(recovered_blocks, block_count);
    allocation_barrier(vec_pages);

    let r_ptr = alloc_region(mem, rid.0, page_count as u32, vec_pages);
    r_ptr
}

pub(crate) unsafe fn region_migration_from_v0_into_v2<M: Memory>(mem: &mut M) {
    use crate::ic0_stable::nicer::{grow, size, write};
    use meta_data::size::{PAGES_IN_BLOCK, PAGE_IN_BYTES};

    assert!(size() == 0);

    // pages required for meta_data (9/ 960KiB), much less than PAGES_IN_BLOCK (128/ 8MB) for a full block
    let meta_data_pages =
        (meta_data::offset::FREE - 1 + (PAGE_IN_BYTES as u64 - 1)) / PAGE_IN_BYTES as u64;

    assert!(meta_data_pages <= PAGES_IN_BLOCK as u64);

    // initially, only allocate meta_data_pages, not a full block, to reduce overhead for
    // canisters that don't allocate regions
    let prev_pages = grow(meta_data_pages);

    if prev_pages == u64::MAX {
        region_trap_with("migration failure (insufficient pages)");
    };

    debug_assert!(prev_pages == 0);

    // Zero metadata region, for good measure.
    let zero_page: [u8; PAGE_IN_BYTES as usize] = [0; PAGE_IN_BYTES as usize];
    for i in 0..meta_data_pages {
        write((i as u64) * (PAGE_IN_BYTES as u64), &zero_page);
    }

    // Write magic header
    write_magic();

    // Region 0 -- classic API for stable memory, as a dedicated region.
    REGION_0 = region_new(mem);

    // Regions 1 through 15, reserved for future use by future Motoko compiler-RTS features.
    region_reserve_id_span(mem, Some(RegionId(1)), RegionId(15));
}

//
// region manager migration/initialization, with pre-existing stable data.
// Case: Version 1 into version 2.
//
pub(crate) unsafe fn region_migration_from_v1_into_v2<M: Memory>(mem: &mut M) {
    // Grow region0 to nearest block boundary, and add a block to fit a region manager meta data.
    //
    // Existing stable data becomes region 0, with first block relocated for region manager meta data.
    //
    // - allocate a block-sized blob on the heap (8MB).
    // - copy the head block of data into that blob, using a stable memory read of a blob.
    // - copy the head block of data from temp blob into new "final block" (logically still first) for region 0.
    // - initialize the meta data for the region system in vacated initial block.

    use crate::ic0_stable::nicer::{grow, read, size, write};

    let header_len = meta_data::size::BLOCK_IN_BYTES as u32;

    let region0_pages = size() as u32; // TODO check for truncation

    let region0_blocks =
        (region0_pages + (meta_data::size::PAGES_IN_BLOCK - 1)) / (meta_data::size::PAGES_IN_BLOCK);
    assert!(region0_blocks > 0);

    let prev_pages = grow(
        (meta_data::size::PAGES_IN_BLOCK + // <-- For new region manager
	 /* Bump out region0 to nearest block boundary: */
	 region0_blocks * meta_data::size::PAGES_IN_BLOCK
            - region0_pages)
            .into(),
    );

    if prev_pages == u64::MAX {
        region_trap_with("migration failure (insufficient pages)")
    };

    debug_assert!(prev_pages == region0_pages.into());

    // Temp for the head block, which we move to be physically last.
    // NB: no allocation_barrier is required: header_val is temporary and can be reclaimed by the next GC increment/run.
    // TODO: instead of allocating an 8MB blob, just stack-allocate a tmp page and zero page, and transfer/zero-init via the stack, using a loop.
    let header_val = crate::memory::alloc_blob(mem, crate::types::Bytes(header_len));
    let header_blob = header_val.as_blob_mut();
    let header_bytes =
        core::slice::from_raw_parts_mut(header_blob.payload_addr(), header_len as usize);

    // Move it:
    read(0, header_bytes); // Save first block as "head block".
    write(
        (region0_blocks * (meta_data::size::BLOCK_IN_BYTES as u32)).into(),
        header_bytes,
    );

    // Zero-out the temp blob.
    for i in 0..header_bytes.len() {
        header_bytes[i] = 0;
    }
    write(0, header_bytes); // Zero out first block, for region manager meta data.

    // Write magic header
    write_magic();

    /* Initialize meta data as if there is only region 0. */
    meta_data::total_allocated_blocks::set(region0_blocks.into());
    meta_data::total_allocated_regions::set(1);

    /*
     * Initialize region0's table entries, for "recovery".
     */

    /* head block is physically last, but logically first. */
    let head_block_id = region0_blocks - 1; /* invariant: region0_blocks is non-zero. */

    meta_data::block_region_table::set(BlockId(head_block_id as u16), Some((RegionId(0), 0)));
    meta_data::region_table::set(&RegionId(0), Some(RegionSizeInPages(region0_pages as u64)));

    /* Any other blocks that follow head block are numbered [0,...,head_block_id) */
    /* They're logical placements are [1,...,) -- one more than their new identity, as a number. */
    for i in 0..head_block_id {
        meta_data::block_region_table::set(
            BlockId((i) as u16),
            Some((RegionId(0), (i + 1) as u16)),
        );
    }
    /* "Recover" the region data into a heap object. */
    REGION_0 = region_recover(mem, &RegionId(0));

    // Ensure that regions 2 through 15 are already reserved for
    // future use by future Motoko compiler-RTS features.
    region_reserve_id_span(mem, Some(RegionId(1)), RegionId(15));
}

//
// region manager migration/initialization, with pre-existing stable data.
// Case: Version 2 into version 2 ("Trivial migration" case).
//
pub(crate) unsafe fn region_migration_from_v2plus_into_v2<M: Memory>(mem: &mut M) {
    use crate::ic0_stable::nicer::{read, read_u32, size};

    // Check if the magic in the memory corresponds to this object.
    assert!(size() > 1);
    let mut magic_bytes: [u8; 8] = [0; 8];
    read(meta_data::offset::MAGIC, &mut magic_bytes);
    if &magic_bytes != meta_data::version::MAGIC {
        region_trap_with("migration failure (bad magic bytes)")
    };
    let version = read_u32(meta_data::offset::VERSION);
    if version > meta_data::version::VERSION {
        region_trap_with("migration failure (unexpected higher version)")
    };

    REGION_0 = region_recover(mem, &RegionId(0));

    // Ensure that regions 2 through 15 are already reserved for
    // future use by future Motoko compiler-RTS features.
    region_reserve_id_span(mem, None, RegionId(15));
}

//
// region manager migration/initialization, with pre-existing stable data.
//
// Valid cases:
//  - from version 1 into version 2.
//  - from version 2 into version 2.
//
#[ic_mem_fn]
pub(crate) unsafe fn region_init<M: Memory>(mem: &mut M, from_version: i32) {
    debug_assert!(meta_data::offset::FREE < meta_data::offset::BLOCK_ZERO);
    match from_version {
        0 => region_migration_from_v0_into_v2(mem),
        1 => region_migration_from_v1_into_v2(mem),
        2 => region_migration_from_v2plus_into_v2(mem),
        _ => unreachable!(),
    }
}

#[ic_mem_fn]
pub unsafe fn region_size<M: Memory>(_mem: &mut M, r: Value) -> u64 {
    let r = r.as_region();
    (*r).page_count.into()
}

#[ic_mem_fn]
pub unsafe fn region_grow<M: Memory>(mem: &mut M, r: Value, new_pages: u64) -> u64 {
    use meta_data::size::{total_required_pages, PAGES_IN_BLOCK};
    let r = r.as_region();
    let new_pages_ = new_pages as u32;
    let old_page_count = (*r).page_count;
    let old_block_count = (old_page_count + (PAGES_IN_BLOCK - 1)) / PAGES_IN_BLOCK;
    let new_block_count = (old_page_count + new_pages_ + (PAGES_IN_BLOCK - 1)) / PAGES_IN_BLOCK;
    let inc_block_count = new_block_count - old_block_count;

    // Determine the required total number of allocated blocks,
    let old_total_blocks = meta_data::total_allocated_blocks::get();
    let new_total_blocks = old_total_blocks + inc_block_count as u64;

    // Actually grow stable memory with more pages as required,
    // while respecting the global maximum limit on pages.
    {
        let have = crate::ic0_stable::nicer::size();
        let need = total_required_pages(new_total_blocks);
        if have < need {
            let diff = need - have;
            if crate::ic0_stable::nicer::grow(diff) == u64::MAX {
                return u64::MAX; // Propagate error
            }
        }
    }

    // Commit the allocation
    meta_data::total_allocated_blocks::set(new_total_blocks);

    // Update this region's page count, in both places where we record it (heap object, region table).
    {
        let r_id = RegionId::from_id((*r).id);
        let c = meta_data::region_table::get(&r_id);

        // Region table agrees with heap object's field.
        assert_eq!(c, Some(RegionSizeInPages(old_page_count.into())));

        // Increase both:
        (*r).page_count += new_pages_;
        meta_data::region_table::set(&r_id, Some(RegionSizeInPages((*r).page_count.into())));
    }

    let new_vec_pages = alloc_blob(mem, Bytes(new_block_count * 2));
    let old_vec_byte_count = old_block_count * 2;

    let new_pages = AccessVector::from_value(&new_vec_pages);
    let old_pages = AccessVector::from_value(&(*r).vec_pages);

    // Copy old region-block associations into new heap object.
    for i in 0..old_vec_byte_count {
        new_pages.0.set(i, old_pages.0.get(i));
    }

    // Record new associations, between the region and each new block:
    // - in block_region_table (stable memory, for persistence).
    // - in region representation (heap memory, for fast access operations).
    for i in old_block_count..new_block_count {
        // rel_i starts at zero.
        let rel_i: u16 = (i - old_block_count as u32) as u16;

        // (to do -- handle case where allocating this way has run out.)
        let block_id: u16 = (old_total_blocks + rel_i as u64) as u16;

        // Update stable memory with new association.
        let assoc = Some((RegionId::from_id((*r).id), i as u16));
        meta_data::block_region_table::set(BlockId(block_id), assoc);

        new_pages.set_ith_block_id(i, &BlockId(block_id));
    }

    allocation_barrier(new_vec_pages);
    write_with_barrier(mem, &mut (*r).vec_pages, new_vec_pages);
    old_page_count.into()
}

pub(crate) unsafe fn region_load<M: Memory>(_mem: &mut M, r: Value, offset: u64, dst: &mut [u8]) {
    use crate::ic0_stable::nicer::read;
    use meta_data::size::BLOCK_IN_BYTES;

    let r = RegionObject::from_value(&r);

    r.check_relative_range(offset, dst.len() as u64);

    if dst.len() == 0 {
        return;
    };

    let (b1_off, b1, b1_len, b2) = r.relative_into_absolute_span(offset, dst.len() as u64);
    if b1 == b2 {
        // Case: only uses a single block, and thus only requires one read.
        read(b1_off, dst);
    } else {
        // Case: Staged reads, one per block that holds requested data.

        let mut i = 0; // invariant: i = # of bytes loaded.
        let mut s = b1_off; // source of bytes, as absolute index.
        let mut d = dst.as_mut_ptr(); // dest for bytes.

        // do initial read (a special case, generally not full block length).
        read(s, core::slice::from_raw_parts_mut(d, b1_len as usize));

        // Advance input and output positions (i, d and s respectively).
        i += b1_len;
        d = d.offset(b1_len as isize);

        // Do rest of block-sized reads.
        // (invariant: they always occur at the start of a block).
        loop {
            let (s_, _, b_len) = r.relative_into_absolute_info(offset + i);
            s = s_;
            if i + b_len > dst.len() as u64 {
                // case: last (generally partial) block.
                if dst.len() as u64 > i {
                    read(
                        s,
                        core::slice::from_raw_parts_mut(d, dst.len() - i as usize),
                    );
                }
                break;
            } else {
                // case: internal (full) block.
                read(
                    s,
                    core::slice::from_raw_parts_mut(d, BLOCK_IN_BYTES as usize),
                );
                d = d.offset(BLOCK_IN_BYTES as isize);
                i += BLOCK_IN_BYTES;
            }
        }
    }
}

pub(crate) unsafe fn region_store<M: Memory>(_mem: &mut M, r: Value, offset: u64, src: &[u8]) {
    use crate::ic0_stable::nicer::write;
    use meta_data::size::BLOCK_IN_BYTES;

    let r = RegionObject::from_value(&r);

    r.check_relative_range(offset, src.len() as u64);

    if src.len() == 0 {
        return;
    };

    let (b1_off, b1, b1_len, b2) = r.relative_into_absolute_span(offset, src.len() as u64);
    if b1 == b2 {
        write(b1_off, src);
    } else {
        // Case: Staged writes, one per block that holds requested data.

        let mut i = 0; // invariant: i = # of bytes stored.
        let mut s = src.as_ptr(); // source for bytes.
        let mut d = b1_off; // dest of bytes, as absolute index.o

        // do initial write (a special case, generally not full block length).
        write(d, core::slice::from_raw_parts(s, b1_len as usize));

        // Advance input and output positions (i, s and d respectively).
        i += b1_len;
        s = s.offset(b1_len as isize);

        // Do rest of block-sized writes.
        // (invariant: they always occur at the start of a block).
        loop {
            let (d_, _, b_len) = r.relative_into_absolute_info(offset + i);
            d = d_;
            if i + b_len > src.len() as u64 {
                // case: last (generally partial) block.
                if src.len() as u64 > i {
                    write(d, core::slice::from_raw_parts(s, src.len() - i as usize));
                }
                break;
            } else {
                // case: internal (full) block.
                write(d, core::slice::from_raw_parts(s, BLOCK_IN_BYTES as usize));
                s = s.offset(BLOCK_IN_BYTES as isize);
                i += BLOCK_IN_BYTES;
            }
        }
    }
}

// -- Region load operations.

#[ic_mem_fn]
pub unsafe fn region_load_word8<M: Memory>(mem: &mut M, r: Value, offset: u64) -> u32 {
    let mut bytes: [u8; 1] = [0];
    region_load(mem, r, offset, &mut bytes);
    core::primitive::u8::from_le_bytes(bytes).into()
}

#[ic_mem_fn]
pub unsafe fn region_load_word16<M: Memory>(mem: &mut M, r: Value, offset: u64) -> u32 {
    let mut bytes: [u8; 2] = [0; 2];
    region_load(mem, r, offset, &mut bytes);
    core::primitive::u16::from_le_bytes(bytes).into()
}

#[ic_mem_fn]
pub unsafe fn region_load_word32<M: Memory>(mem: &mut M, r: Value, offset: u64) -> u32 {
    let mut bytes: [u8; 4] = [0; 4];
    region_load(mem, r, offset, &mut bytes);
    core::primitive::u32::from_le_bytes(bytes).into()
}

#[ic_mem_fn]
pub unsafe fn region_load_word64<M: Memory>(mem: &mut M, r: Value, offset: u64) -> u64 {
    let mut bytes: [u8; 8] = [0; 8];
    region_load(mem, r, offset, &mut bytes);
    core::primitive::u64::from_le_bytes(bytes).into()
}

#[ic_mem_fn]
pub unsafe fn region_load_float64<M: Memory>(mem: &mut M, r: Value, offset: u64) -> f64 {
    let mut bytes: [u8; 8] = [0; 8];
    region_load(mem, r, offset, &mut bytes);
    core::primitive::f64::from_le_bytes(bytes).into()
}

#[ic_mem_fn]
pub(crate) unsafe fn region_load_blob<M: Memory>(
    mem: &mut M,
    r: Value,
    offset: u64,
    len: u32,
) -> Value {
    let blob_val = crate::memory::alloc_blob(mem, crate::types::Bytes(len));
    let blob = blob_val.as_blob_mut();

    if len < (isize::MAX as u32) {
        let bytes: &mut [u8] = core::slice::from_raw_parts_mut(blob.payload_addr(), len as usize);
        region_load(mem, r, offset, bytes);
    } else {
        assert!((len / 2) < isize::MAX as u32);
        let bytes_low: &mut [u8] =
            core::slice::from_raw_parts_mut(blob.payload_addr(), (len / 2) as usize);
        region_load(mem, r, offset, bytes_low);
        let bytes_high: &mut [u8] = core::slice::from_raw_parts_mut(
            blob.payload_addr().add((len / 2) as usize),
            (len - len / 2) as usize,
        );
        region_load(mem, r, offset + (len / 2) as u64, bytes_high);
    }
    allocation_barrier(blob_val);
    blob_val
}

// -- Region store operations.

#[ic_mem_fn]
pub unsafe fn region_store_word8<M: Memory>(mem: &mut M, r: Value, offset: u64, val: u32) {
    region_store(mem, r, offset, &core::primitive::u8::to_le_bytes(val as u8))
}

#[ic_mem_fn]
pub unsafe fn region_store_word16<M: Memory>(mem: &mut M, r: Value, offset: u64, val: u32) {
    region_store(
        mem,
        r,
        offset,
        &core::primitive::u16::to_le_bytes(val as u16),
    )
}

#[ic_mem_fn]
pub unsafe fn region_store_word32<M: Memory>(mem: &mut M, r: Value, offset: u64, val: u32) {
    region_store(
        mem,
        r,
        offset,
        &core::primitive::u32::to_le_bytes(val as u32),
    )
}

#[ic_mem_fn]
pub unsafe fn region_store_word64<M: Memory>(mem: &mut M, r: Value, offset: u64, val: u64) {
    region_store(mem, r, offset, &core::primitive::u64::to_le_bytes(val))
}

#[ic_mem_fn]
pub unsafe fn region_store_float64<M: Memory>(mem: &mut M, r: Value, offset: u64, val: f64) {
    region_store(mem, r, offset, &core::primitive::f64::to_le_bytes(val))
}

#[ic_mem_fn]
pub(crate) unsafe fn region_store_blob<M: Memory>(mem: &mut M, r: Value, offset: u64, blob: Value) {
    let blob = blob.as_blob();
    let len = blob.len().0;
    let bytes = blob.payload_const();
    if len < (isize::MAX as u32) {
        let bytes: &[u8] = core::slice::from_raw_parts(bytes, len as usize);
        region_store(mem, r, offset, bytes);
    } else {
        assert!((len / 2) < isize::MAX as u32);
        let bytes_low: &[u8] = core::slice::from_raw_parts(bytes, (len / 2) as usize);
        region_store(mem, r, offset, bytes_low);
        let bytes_high: &[u8] =
            core::slice::from_raw_parts(bytes.add((len / 2) as usize), (len - len / 2) as usize);
        region_store(mem, r, offset + (len / 2) as u64, bytes_high);
    }
}
