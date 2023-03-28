use crate::memory::{alloc_blob, ic::NEXT_REGION_LOG_ID, Memory};
use crate::rts_trap_with;
use crate::types::{size_of, Blob, Bytes, Region, Value, TAG_REGION};

use motoko_rts_macros::ic_mem_fn;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BlockId(pub u16);

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RegionId(pub u16);

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RegionSizeInPages(pub u64);

#[derive(Clone)]
pub struct AccessVector(pub *mut Blob);

#[derive(Clone)]
pub struct RegionObject(pub *mut Region);

const NIL_REGION_ID: u16 = 0;

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
            Some(id) => id.0 + 1,
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
    pub fn from_value(v: &Value) -> Self {
        AccessVector(v.get_ptr() as *mut Blob)
    }

    pub unsafe fn set_ith_block_id(&self, i: u32, block_id: &BlockId) {
        let block_id_upper: u8 = ((block_id.0 & 0xff00) >> 8) as u8;
        let block_id_lower: u8 = ((block_id.0 & 0x00ff) >> 0) as u8;

        // Update heap memory with new association. Little endian.
        // (write u16 to slot i in new_pages.)
        self.0.set(i * 2 + 0, block_id_lower);
        self.0.set(i * 2 + 1, block_id_upper);
    }

    pub unsafe fn get_ith_block_id(&self, i: u32) -> BlockId {
        let lower: u16 = self.0.get(i * 2 + 0) as u16;
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

    pub unsafe fn check_relative_into_absolute_offset(&self, offset: u64) {
        if !((offset / meta_data::size::PAGE_IN_BYTES) < (*self.0).page_count.into()) {
            rts_trap_with("region access out of bounds.");
        };
    }

    // computes absolute offset, BlockId, and remaining length (of the
    // given block) for a relative offset.
    //
    // assumes but does not run check_relative_into_absolute_offset for
    // this offset, or some higher one.
    pub unsafe fn relative_into_absolute_info(&self, offset: u64) -> (u64, BlockId, u64) {
        let av = AccessVector::from_value(&(*self.0).vec_pages);

        // Which block (rank relative to this region)?
        let block_rank = offset / meta_data::size::BLOCK_IN_BYTES;

        // Where in that block?
        let intra_block_index = offset % meta_data::size::BLOCK_IN_BYTES;

        // Where is that block located in stable memory (global rank)?
        let block_id = av.get_ith_block_id(block_rank as u32);

        //println!(80, "intra-block index is {} (block is {:?})", intra_block_index, block_id);

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
        let (_, b2, _) = self.relative_into_absolute_info(offset + len - 1);
        (off, b1, b1_len, b2)
    }
}

// Mutable meta data stored in stable memory header (See motoko/design/StableRegions.md)
mod meta_data {

    /// Maximum number of entities.
    pub mod max {
        pub const BLOCKS: u16 = 32768;

        pub const REGIONS: u16 = 32767;
    }

    /// Sizes of table entries, and tables.
    pub mod size {
        pub const REGION_TABLE_ENTRY: u16 = 8;

        pub const BLOCK_REGION_TABLE_ENTRY: u16 = 4;

        pub const BLOCK_REGION_TABLE: u64 =
            super::max::BLOCKS as u64 * BLOCK_REGION_TABLE_ENTRY as u64;

        pub const REGION_TABLE: u64 = super::max::REGIONS as u64 * REGION_TABLE_ENTRY as u64;

        pub const PAGES_IN_BLOCK: u32 = 128;
        pub const PAGE_IN_BYTES: u64 = 1 << 16;
        pub const BLOCK_IN_BYTES: u64 = PAGE_IN_BYTES * (PAGES_IN_BLOCK as u64);

        // Static memory footprint, ignoring any dynamically-allocated pages.
        pub const STATIC_MEM_IN_PAGES: u64 =
            (super::offset::BLOCK_ZERO + PAGE_IN_BYTES - 1) / PAGE_IN_BYTES;

        pub unsafe fn required_pages() -> u64 {
            STATIC_MEM_IN_PAGES
                + (super::total_allocated_blocks::get() as u64) * (PAGES_IN_BLOCK as u64)
        }
    }

    /// Offsets into stable memory for statically-sized fields and tables.
    pub mod offset {
        pub const TOTAL_ALLOCATED_BLOCKS: u64 = 0;

        pub const TOTAL_ALLOCATED_REGIONS: u64 = 2;

        pub const BLOCK_REGION_TABLE: u64 = 4;

        pub const REGION_TABLE: u64 = BLOCK_REGION_TABLE + super::size::BLOCK_REGION_TABLE;

        pub const BLOCK_ZERO: u64 = REGION_TABLE + super::size::REGION_TABLE;
    }

    pub mod total_allocated_blocks {
        use super::offset;
        use crate::ic0_stable::nicer::{read_u16, write_u16};

        use crate::memory::ic::{REGION_SET_MEM_SIZE, REGION_TOTAL_ALLOCATED_BLOCKS};

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

                // Invalidate stale number (No longer use the code-gen
                // provided number that we need to use, temporarily,
                // for destabilization after an upgrade).
                REGION_SET_MEM_SIZE = None;
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

        // Compute an offset in stable memory for a particular block ID (based zero).
        fn index(b: &BlockId) -> u64 {
            offset::BLOCK_REGION_TABLE + (b.0 as u64 * size::BLOCK_REGION_TABLE_ENTRY as u64)
        }

        /// Some means that the block is in use.
        /// None means that the block is available for (re-)allocation.
        pub fn get(b: BlockId) -> Option<(RegionId, u16)> {
            let raw = read_u16(index(&b));
            let rid = RegionId::from_u16(raw);
            rid.map(|r| (r, read_u16(index(&b) + 2)))
        }

        /// Some means that the block is in use.
        /// None means that the block is available for (re-)allocation.
        pub fn set(b: BlockId, r: Option<(RegionId, u16)>) {
            match r {
                None => write_u16(index(&b), RegionId::into_u16(None)),
                Some((r, j)) => {
                    write_u16(index(&b) + 0, RegionId::into_u16(Some(r)));
                    write_u16(index(&b) + 2, j)
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

#[ic_mem_fn]
pub unsafe fn region_next_id<M: Memory>(_mem: &mut M) -> Value {
    Value::from_scalar(meta_data::total_allocated_regions::get() as u32)
}

// Region manager's total memory size in stable memory, in _pages_.
#[ic_mem_fn]
pub unsafe fn region_get_mem_size<M: Memory>(_mem: &mut M) -> u64 {
    let size = {
        if let Some(s) = crate::memory::ic::REGION_SET_MEM_SIZE {
            return s;
        };
        if crate::memory::ic::REGION_MEM_SIZE_INIT {
            meta_data::size::STATIC_MEM_IN_PAGES as u64
                + crate::memory::ic::REGION_TOTAL_ALLOCATED_BLOCKS as u64
                    * (meta_data::size::PAGES_IN_BLOCK as u64)
        } else {
            // Before we initialization of anything, give back zero.
            0
        }
    };
    if false {
        println!(80, "region_get_mem_size() => {}", size);
    }
    size
}

// Region manager's total memory size in stable memory, in _pages_.
#[ic_mem_fn]
pub unsafe fn region_set_mem_size<M: Memory>(_mem: &mut M, size: u64) {
    if false {
        println!(80, "region_set_mem_size({})", size);
    }
    crate::memory::ic::REGION_SET_MEM_SIZE = Some(size);
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
    let r_ptr = mem.alloc_words(size_of::<Region>());

    let next_id = meta_data::total_allocated_regions::get() as u16;
    meta_data::total_allocated_regions::set(next_id as u64 + 1);

    // NB. cannot use as_region() here as we didn't write the header yet
    let region = r_ptr.get_ptr() as *mut Region;
    (*region).header.tag = TAG_REGION;
    (*region).id = next_id;
    (*region).page_count = 0;
    (*region).vec_pages = alloc_blob(mem, Bytes(0));

    // Update Region table.
    {
        let r_id = RegionId::from_id((*region).id);
        let c = meta_data::region_table::get(&r_id);
        // sanity check: Region table says that this region is available.
        assert_eq!(c, None);
        meta_data::region_table::set(&r_id, Some(RegionSizeInPages(0)));
    }
    Value::from_ptr(region as usize)
}

#[ic_mem_fn]
pub unsafe fn region_recover<M: Memory>(mem: &mut M, rid: &RegionId) -> Value {
    use meta_data::size::PAGES_IN_BLOCK;
    let c = meta_data::region_table::get(&rid);
    let page_count = match c {
        Some(RegionSizeInPages(pc)) => {
            if false {
                println!(
                    80,
                    "region_recover {:?} => found region record {:?}", rid, c
                );
            }
            pc
        }
        _ => {
            rts_trap_with("region_recover failed: Expected Some(RegionSizeInPages(_))");
        }
    };
    let r_ptr = mem.alloc_words(size_of::<Region>());

    // NB. cannot use as_region() here as we didn't write the header yet
    let region = r_ptr.get_ptr() as *mut Region;
    (*region).header.tag = TAG_REGION;
    (*region).id = rid.0;
    (*region).page_count = page_count as u32;

    let block_count = (page_count as u32 + PAGES_IN_BLOCK - 1) / PAGES_IN_BLOCK;
    (*region).vec_pages = alloc_blob(mem, Bytes(block_count * 2));
    let tb = meta_data::total_allocated_blocks::get();
    let av = AccessVector((*region).vec_pages.as_blob_mut());
    let mut recovered_blocks = 0;
    for block_id in 0..tb {
        match meta_data::block_region_table::get(BlockId(block_id as u16)) {
            None => {}
            Some((rid_, rank)) => {
                if &rid_ == rid {
                    av.set_ith_block_id(rank.into(), &BlockId(block_id as u16));
                    recovered_blocks += 1;
                }
            }
        }
    }
    assert_eq!(recovered_blocks, block_count);
    Value::from_ptr(region as usize)
}

#[ic_mem_fn]
pub(crate) unsafe fn region_init_<M: Memory>(mem: &mut M) {
    // detect if we are being called in after upgrade --
    if crate::ic0_stable::nicer::size() == 0 {
        if false {
            println!(80, "region_init -- first time.");
        }
        let _ = crate::ic0_stable::nicer::grow(meta_data::size::STATIC_MEM_IN_PAGES as u64);

        // Region 0 -- classic API for stable memory, as a dedicated region.
        crate::memory::ic::REGION_0 = crate::region::region_new(mem);

        // Region 1 -- reserved for reclaimed regions' blocks (to do).
        crate::memory::ic::REGION_1 = crate::region::region_new(mem);

        // Regions 2 through 15, reserved for future use by future Motoko compiler-RTS features.
        region_reserve_id_span(mem, Some(RegionId(2)), RegionId(15));

        // Recall that we've done this later, without asking ic0_stable::size.
        assert_eq!(crate::memory::ic::REGION_MEM_SIZE_INIT, false);
        crate::memory::ic::REGION_MEM_SIZE_INIT = true;
    } else {
        if false {
            println!(80, "region_init -- upgrade time.");
        }

        // Recall that we've done this later, without asking ic0_stable::size.
        assert_eq!(crate::memory::ic::REGION_MEM_SIZE_INIT, false);
        crate::memory::ic::REGION_MEM_SIZE_INIT = true;

        if false {
            println!(80, "region_init -- recover regions 0 and 1.");
        }
        crate::memory::ic::REGION_0 = crate::region::region_recover(mem, &RegionId(0));
        crate::memory::ic::REGION_1 = crate::region::region_recover(mem, &RegionId(1));

        // Ensure that regions 2 through 15 are already reserved for
        // future use by future Motoko compiler-RTS features.
        region_reserve_id_span(mem, None, RegionId(15));
    }
}

// Utility for logging global region manager state (in stable memory).
// For sanity-checking during testing and for future trouble-shooting.
// (Perhaps we can keep this here, and just not commit to it when exposing final API?)
#[ic_mem_fn]
pub unsafe fn region_meta_loglines<M: Memory>(_mem: &mut M) {
    let log_id = NEXT_REGION_LOG_ID;
    NEXT_REGION_LOG_ID += 1;
    println!(50, "# regionMetaLogLines {{");
    println!(50, " log_id = {};", log_id);
    println!(
        50,
        " total_allocated_blocks = {};",
        meta_data::total_allocated_blocks::get()
    );
    println!(
        50,
        " total_allocated_regions = {};",
        meta_data::total_allocated_regions::get()
    );
    println!(50, "}}");
}

#[ic_mem_fn]
pub unsafe fn region_id<M: Memory>(_mem: &mut M, r: Value) -> u32 {
    let r = r.as_region();
    (*r).id.into()
}

#[ic_mem_fn]
pub unsafe fn region_size<M: Memory>(_mem: &mut M, r: Value) -> u64 {
    if false {
        println!(80, "region_size({:?})", r);
    }
    let r = r.as_region();
    (*r).page_count.into()
}

#[ic_mem_fn]
pub unsafe fn region_grow<M: Memory>(mem: &mut M, r: Value, new_pages: u64) -> u64 {
    use meta_data::size::{required_pages, PAGES_IN_BLOCK};
    let r = r.as_region();
    let new_pages_ = new_pages as u32;
    let old_page_count = (*r).page_count;
    let old_block_count = (old_page_count + (PAGES_IN_BLOCK - 1)) / PAGES_IN_BLOCK;
    let new_block_count = (old_page_count + new_pages_ + (PAGES_IN_BLOCK - 1)) / PAGES_IN_BLOCK;
    let inc_block_count = new_block_count - old_block_count;

    if false {
        println!(
            80,
            "begin region_grow id={} page_count={} new_pages={}",
            (*r).id,
            old_page_count,
            new_pages
        );
    }

    // Update the total number of allocated blocks.
    let old_total_blocks = {
        let c = meta_data::total_allocated_blocks::get();
        let c_ = c + inc_block_count as u64;
        // to do -- assert c_ is less than meta_data::max::BLOCKS
        meta_data::total_allocated_blocks::set(c_);
        c
    };

    // Actually grow stable memory with more pages
    // (but only if needed, by first checking if someone else did it already).
    {
        let have = crate::ic0_stable::nicer::size();
        let need = required_pages();
        if have < need {
            let diff = need - have;
            crate::ic0_stable::nicer::grow(diff);
        }
    }

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

    if false {
        println!(
            80,
            " region_grow id={} (old_block_count, new_block_count) = ({}, {})",
            (*r).id,
            old_block_count,
            new_block_count
        );
    }

    // Record new associations, between the region and each new block:
    // - in block_region_table (stable memory, for persistence).
    // - in region representation (heap memory, for fast access operations).
    for i in old_block_count..new_block_count {
        // rel_i starts at zero.
        let rel_i: u16 = (i - old_block_count as u32) as u16;

        // (to do -- handle case where allocating this way has run out.)
        let block_id: u16 = (old_total_blocks + rel_i as u64) as u16;

        if false {
            println!(
                50,
                "  region_grow id={} (slot index) i={} block_id={}",
                (*r).id,
                i,
                block_id
            );
        }

        // Update stable memory with new association.
        let assoc = Some((RegionId::from_id((*r).id), i as u16));
        meta_data::block_region_table::set(BlockId(block_id), assoc.clone());

        if true {
            // temp sanity testing: read back the data we just wrote.
            let assoc_ = meta_data::block_region_table::get(BlockId(block_id));
            assert_eq!(assoc, assoc_)
        }

        new_pages.set_ith_block_id(i, &BlockId(block_id));

        if true {
            // temp sanity testing: read back the data we just wrote.
            let block_id_ = new_pages.get_ith_block_id(i);
            assert_eq!(BlockId(block_id), block_id_);
        }
    }

    if false {
        println!(80, " region_grow id={} done.", (*r).id,);
    }

    (*r).vec_pages = new_vec_pages;
    old_page_count.into()
}

pub(crate) unsafe fn region_load<M: Memory>(_mem: &mut M, r: Value, offset: u64, dst: &mut [u8]) {
    use crate::ic0_stable::nicer::read;
    use meta_data::size::BLOCK_IN_BYTES;

    let r = RegionObject::from_value(&r);
    if dst.len() > 1 {
        r.check_relative_into_absolute_offset(offset + dst.len() as u64 - 1);
    } else {
        r.check_relative_into_absolute_offset(offset);
    }
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
            if false {
                println!(80, "load r={:?} s={} i={}", r.id(), s, i);
            }
            let (s_, _, b_len) = r.relative_into_absolute_info(offset + i);
            s = s_;
            if i + b_len > dst.len() as u64 {
                // case: last (generally partial) block.
                if i - dst.len() as u64 > 0 {
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
    if src.len() > 1 {
        r.check_relative_into_absolute_offset(offset + src.len() as u64 - 1);
    } else {
        r.check_relative_into_absolute_offset(offset);
    }
    let (b1_off, b1, b1_len, b2) = r.relative_into_absolute_span(offset, src.len() as u64);
    if b1 == b2 {
        write(b1_off, src);
    } else {
        // Case: Staged reads, one per block that holds requested data.

        let mut i = 0; // invariant: i = # of bytes stored.
        let mut s = src.as_ptr(); // source for bytes.
        let mut d = b1_off; // dest of bytes, as absolute index.o

        // do initial read (a special case, generally not full block length).
        write(d, core::slice::from_raw_parts(s, b1_len as usize));

        // Advance input and output positions (i, s and d respectively).
        i += b1_len;
        s = s.offset(b1_len as isize);

        // Do rest of block-sized reads.
        // (invariant: they always occur at the start of a block).
        loop {
            let (d_, _, b_len) = r.relative_into_absolute_info(offset + i);
            d = d_;
            if i + b_len > src.len() as u64 {
                // case: last (generally partial) block.
                if i - src.len() as u64 > 0 {
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
    let mut byte: [u8; 1] = [0];
    region_load(mem, r, offset, &mut byte);
    byte[0] as u32
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
pub unsafe fn region_load_blob<M: Memory>(mem: &mut M, r: Value, offset: u64, len: u32) -> Value {
    let blob_val = crate::memory::alloc_blob(mem, crate::types::Bytes(len));
    let blob = blob_val.as_blob_mut();
    let bytes: &mut [u8] = core::slice::from_raw_parts_mut(blob.payload_addr(), len as usize);
    region_load(mem, r, offset, bytes);
    blob_val
}

// -- Region store operations.

#[ic_mem_fn]
pub unsafe fn region_store_word8<M: Memory>(mem: &mut M, r: Value, offset: u64, byte: u32) {
    let mut byte: [u8; 1] = [byte as u8];
    region_store(mem, r, offset, &mut byte);
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
pub unsafe fn region_store_blob<M: Memory>(mem: &mut M, r: Value, offset: u64, blob: Value) {
    let blob: *const Blob = blob.as_blob();
    let len = blob.len();
    let bytes = blob.payload_const();
    let bytes: &[u8] = core::slice::from_raw_parts(bytes, len.0 as usize);
    region_store(mem, r, offset, bytes)
}
