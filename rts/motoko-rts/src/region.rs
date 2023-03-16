use crate::memory::{ic::NEXT_REGION_LOG_ID, Memory, alloc_blob};
use crate::types::{size_of, Region, Value, Blob, Bytes, TAG_REGION};
use crate::rts_trap_with;

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

const NIL_REGION_ID : u16 = 0;

// This impl encapsulates encoding of optional region IDs within a u16.
// Used by block-region table to encode the (optional) region ID of a block.
impl RegionId {
    pub fn id_is_nil(id : u16) -> bool {
	id == NIL_REGION_ID
    }
    pub fn from_id(id: u16) -> Self {
	RegionId(id)
    }
    pub fn from_u16(id: u16) -> Option<Self> {
	if Self::id_is_nil(id) { None } else { Some(RegionId(id - 1)) }
    }
    pub fn into_u16(opreg: Option<RegionId>) -> u16 {
	match opreg {
	    None => 0,
	    Some(id) => id.0 + 1
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
    pub fn u64_is_nil(u : u64) -> bool {
	u == 0
    }
    pub fn from_u64(u: u64) -> Option<Self> {
	if Self::u64_is_nil(u) { None } else { Some(RegionSizeInPages(u - 1)) }
    }
    pub fn into_u64(opreg: Option<RegionSizeInPages>) -> u64 {
	match opreg {
	    None => 0,
	    Some(s) => s.0 + 1
	}
    }
}

impl AccessVector {

    pub fn from_value(v: &Value) -> Self {
	AccessVector(v.get_ptr() as *mut Blob)
    }

    pub unsafe fn set_ith_block_id(&self, i: u32, block_id: &BlockId) {
	let block_id_upper : u8 = ((block_id.0 & 0xff00) >> 8) as u8;
	let block_id_lower : u8 = ((block_id.0 & 0x00ff) >> 0) as u8;

	// Update heap memory with new association. Little endian.
	// (write u16 to slot i in new_pages.)
	self.0.set(i * 2 + 0, block_id_lower);
	self.0.set(i * 2 + 1, block_id_upper);
    }

    pub unsafe fn get_ith_block_id(&self, i: u32) -> BlockId {
	let lower : u16 = self.0.get(i * 2 + 0) as u16;
	let upper : u16 = self.0.get(i * 2 + 1) as u16;
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

    pub unsafe fn relative_into_absolute_offset(&self, offset: u64) -> u64 {
	let av = AccessVector::from_value(&(*self.0).vec_pages);

	// assert that offset is currently allocated
	// (to do -- this should be a certain kind of trap when it fails.)
	assert!(offset / meta_data::size::PAGE_IN_BYTES < (*self.0).page_count.into());

	// Which block (rank relative to this region)?
	let block_rank  = offset / meta_data::size::BLOCK_IN_BYTES;

	// Where in that block?
	let intra_block_index = offset % meta_data::size::BLOCK_IN_BYTES;

	// Where is that block located in stable memory (global rank)?
	let block_id = av.get_ith_block_id(block_rank as u32);

	// address of the byte to load from stable memory:
	let offset =
	    meta_data::offset::BLOCK_ZERO + block_id.0 as u64 * meta_data::size::BLOCK_IN_BYTES
	    + intra_block_index;
	offset
    }

}

// Mutable meta data stored in stable memory header (See motoko/design/StableRegions.md)
mod meta_data {

    /// Maximum number of entities.
    pub mod max {
	pub const BLOCKS : u16 = 32768;

	pub const REGIONS : u16 = 32767;
    }

    /// Sizes of table entries, and tables.
    pub mod size {
	pub const REGION_TABLE_ENTRY : u16 = 8;

	pub const BLOCK_REGION_TABLE_ENTRY : u16 = 4;

	pub const BLOCK_REGION_TABLE : u64 =
	    super::max::BLOCKS as u64 * BLOCK_REGION_TABLE_ENTRY as u64;

	pub const REGION_TABLE : u64 =
	    super::max::REGIONS as u64 * REGION_TABLE_ENTRY as u64;

	pub const PAGES_IN_BLOCK : u32 = 128;
	pub const PAGE_IN_BYTES : u64 = 1 << 16;
	pub const BLOCK_IN_BYTES : u64 = PAGE_IN_BYTES * (PAGES_IN_BLOCK as u64);

	pub unsafe fn required_pages() -> u64 {
	    use super::offset::BLOCK_ZERO;
	    let meta_data_pages = (BLOCK_ZERO + PAGE_IN_BYTES - 1) / PAGE_IN_BYTES;
	    meta_data_pages + (super::total_allocated_blocks::get() * (PAGES_IN_BLOCK as u64))
	}
    }

    /// Offsets into stable memory for statically-sized fields and tables.
    pub mod offset {
	pub const TOTAL_ALLOCATED_BLOCKS : u64 = 0;

	pub const TOTAL_ALLOCATED_REGIONS : u64 = 2;

	pub const BLOCK_REGION_TABLE : u64 = 4;

	pub const REGION_TABLE : u64 =
	    BLOCK_REGION_TABLE +
	    super::size::BLOCK_REGION_TABLE;

	pub const BLOCK_ZERO : u64 =
	    REGION_TABLE +
	    super::size::REGION_TABLE;
    }

    pub mod total_allocated_blocks {
	use crate::ic0_stable::nicer::{read_u16, write_u16};
	use super::offset;

	pub fn get() -> u64 {
	    read_u16(offset::TOTAL_ALLOCATED_BLOCKS) as u64
	}
	pub fn set(n: u64) {
	    write_u16(offset::TOTAL_ALLOCATED_BLOCKS, n as u16)
	}
    }

    pub mod total_allocated_regions {
	use crate::ic0_stable::nicer::{read_u16, write_u16};
	use super::offset;

	pub fn get() -> u64 {
	    read_u16(offset::TOTAL_ALLOCATED_REGIONS) as u64
	}
	pub fn set(n: u64) {
	    write_u16(offset::TOTAL_ALLOCATED_REGIONS, n as u16)
	}
    }

    pub mod block_region_table {
	// invariant: all blocks whose IDs are below the total_allocated_blocks are valid.

	use crate::ic0_stable::nicer::{read_u16, write_u16};
	use super::{offset, size};
	use crate::region::{BlockId, RegionId};

	// Compute an offset in stable memory for a particular block ID (based zero).
	fn index(b : &BlockId) -> u64 {
	    offset::BLOCK_REGION_TABLE + (b.0 as u64 * size::BLOCK_REGION_TABLE_ENTRY as u64)
	}

	/// Some means that the block is in use.
	/// None means that the block is available for (re-)allocation.
	pub fn get(b:BlockId) -> Option<(RegionId, u16)> {
	    let raw = read_u16(index(&b));
	    let rid = RegionId::from_u16(raw);
	    rid.map(|r| {
		(r, read_u16(index(&b) + 2))
	    })
	}

	/// Some means that the block is in use.
	/// None means that the block is available for (re-)allocation.
	pub fn set(b:BlockId, r:Option<(RegionId, u16)>) {
	    match r {
		None => {
		    write_u16(index(&b), RegionId::into_u16(None))
		},
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

	fn index(r : &RegionId) -> u64 {
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
pub unsafe fn region0_load<M: Memory>(_mem: &mut M, offset:u64, dst: &mut [u8]) {
    let r = RegionObject(crate::memory::ic::REGION_0);
    let abs_off = r.relative_into_absolute_offset(offset);
    // second bounds check on region:
    if dst.len() > 1 {
	let _ = r.relative_into_absolute_offset(offset + dst.len() as u64 - 1);
    };
    crate::ic0_stable::nicer::read(abs_off, dst);
}

#[ic_mem_fn]
pub unsafe fn region0_store<M: Memory>(_mem: &mut M, offset:u64, src: &[u8]) {
    let r = RegionObject(crate::memory::ic::REGION_0);
    let abs_off = r.relative_into_absolute_offset(offset);
    // second bounds check on region:
    if src.len() > 1 {
	let _ = r.relative_into_absolute_offset(offset + src.len() as u64 - 1);
    };
    crate::ic0_stable::nicer::write(abs_off, src);
}

#[ic_mem_fn]
pub unsafe fn region0_grow<M: Memory>(mem: &mut M, new_pages: u64) -> u64 {
    region_grow(mem, Value::from_ptr(crate::memory::ic::REGION_0 as usize), new_pages)
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
pub(crate) unsafe fn region_init_<M: Memory>(mem: &mut M) {
    use meta_data::{size, offset};
    let min_pages = (offset::BLOCK_ZERO + size::PAGE_IN_BYTES - 1) / size::PAGE_IN_BYTES;
    let _ = crate::ic0_stable::nicer::grow(min_pages);
    // Region 0 -- classic API for stable memory, as a dedicated region.
    crate::memory::ic::REGION_0 = crate::region::region_new(mem).as_region();
    // Region 1 -- reserved for reclaimed regions' blocks (to do).
    crate::memory::ic::REGION_1 = crate::region::region_new(mem).as_region();
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
    println!(50, " total_allocated_blocks = {};", meta_data::total_allocated_blocks::get());
    println!(50, " total_allocated_regions = {};", meta_data::total_allocated_regions::get());
    println!(50, "}}");

}

#[ic_mem_fn]
pub unsafe fn region_id<M: Memory>(_mem: &mut M, r: Value) -> u32 {
    let r = r.as_region();
    (*r).id.into()
}

#[ic_mem_fn]
pub unsafe fn region_size<M: Memory>(_mem: &mut M, r: Value) -> u64 {
    let r = r.as_region();
    (*r).page_count.into()
}

#[ic_mem_fn]
pub unsafe fn region_grow<M: Memory>(mem: &mut M, r: Value, new_pages: u64) -> u64 {
    use meta_data::size::{PAGES_IN_BLOCK, required_pages};
    let r = r.as_region();
    let new_pages_ = new_pages as u32;
    let old_page_count = (*r).page_count;
    let old_block_count = (old_page_count + (PAGES_IN_BLOCK - 1)) / PAGES_IN_BLOCK;
    let new_block_count = (old_page_count + new_pages_ + (PAGES_IN_BLOCK - 1)) / PAGES_IN_BLOCK;
    let inc_block_count = new_block_count - old_block_count;

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

    println!(80, "region_grow id={} (old_block_count, new_block_count) = ({}, {})", (*r).id, old_block_count, new_block_count);

    // Record new associations, between the region and each new block:
    // - in block_region_table (stable memory, for persistence).
    // - in region representation (heap memory, for fast access operations).
    for i in old_block_count..new_block_count {
	// rel_i starts at zero.
	let rel_i : u16 = (i - old_block_count as u32) as u16;

	// (to do -- handle case where allocating this way has run out.)
	let block_id : u16 = (old_total_blocks + rel_i as u64) as u16;

	println!(50, "  region_grow id={} (slot index) i={} block_id={}", (*r).id, i, block_id);

	// Update stable memory with new association.
	let assoc =  Some((RegionId::from_id((*r).id), i as u16));
	meta_data::block_region_table::set(BlockId(block_id), assoc.clone());

	if true { // temp sanity testing: read back the data we just wrote.
	    let assoc_ = meta_data::block_region_table::get(BlockId(block_id));
	    assert_eq!(assoc, assoc_)
	}

	new_pages.set_ith_block_id(i, &BlockId(block_id));

	if true { // temp sanity testing: read back the data we just wrote.
	    let block_id_ = new_pages.get_ith_block_id(i);
	    assert_eq!(BlockId(block_id), block_id_);
	}
    }

    (*r).vec_pages = new_vec_pages;
    old_page_count.into()
}

#[ic_mem_fn]
pub unsafe fn region_load_byte<M: Memory>(_mem: &mut M, r: Value, offset: u64) -> u8 {
    let r = RegionObject::from_value(&r);
    let abs_off = r.relative_into_absolute_offset(offset);
    let mut dst : [u8; 1] = [0];
    crate::ic0_stable::nicer::read(abs_off, &mut dst);
    println!(80, "region_load_byte({:?}, {} ~> {}) ==> {}", r.id(), offset, abs_off, dst[0]);
    dst[0]
}

#[ic_mem_fn]
pub unsafe fn region_store_byte<M: Memory>(_mem: &mut M, r: Value, offset: u64, byte: u8) {
    let r = RegionObject::from_value(&r);
    let abs_off = r.relative_into_absolute_offset(offset);
    let src : [u8; 1] = [byte];
    println!(80, "region_store_byte({:?}, {} ~> {}, {})", r.id(), offset, abs_off, byte);
    crate::ic0_stable::nicer::write(abs_off, &src);
}

#[ic_mem_fn]
pub unsafe fn region_load_blob<M: Memory>(_mem: &mut M, _r: Value, _start: Value, _len: Value) -> Value {
    rts_trap_with("TODO region_load_blob");
}

#[ic_mem_fn]
pub unsafe fn region_store_blob<M: Memory>(_mem: &mut M, _r: Value, _start: Value, _blob: Value) {
    rts_trap_with("TODO region_store_blob");
}


#[ic_mem_fn]
pub unsafe fn region_next_id<M: Memory>(_mem: &mut M) -> Value {
    Value::from_scalar(meta_data::total_allocated_regions::get() as u32)
}
