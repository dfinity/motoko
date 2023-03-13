use crate::memory::{ic::NEXT_REGION_ID, ic::NEXT_REGION_LOG_ID, Memory, alloc_blob};
use crate::types::{size_of, Region, Value, Words, Blob, Bytes, TAG_REGION};
use crate::rts_trap_with;

use motoko_rts_macros::ic_mem_fn;

pub struct BlockId(pub u16);
pub struct RegionId(pub u16);

pub const NIL_REGION_ID : u16 = 0;

impl RegionId {
    pub fn id_is_nil(id : u16) -> bool {
	id == NIL_REGION_ID
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

// Mutable meta data stored in stable memory header (See motoko/design/StableRegions.md)
mod meta_data {

    /// Maximum number of entities.
    pub mod max {
	pub const BLOCKS : u16 = 32768;

	pub const REGIONS : u16 = 32767;
    }

    /// Sizes of table entries.
    pub mod size {
	pub const REGION_TABLE_ENTRY : u16 = 8;

	pub const BLOCK_REGION_TABLE_ENTRY : u16 = 4;

	pub const BLOCK_REGION_TABLE : u64 =
	    super::max::BLOCKS as u64 * BLOCK_REGION_TABLE_ENTRY as u64;
    }

    /// Offsets into stable memory for statically-sized fields and tables.
    pub mod offset {
	pub const TOTAL_ALLOCATED_BLOCKS : u64 = 0;

	pub const TOTAL_ALLOCATED_REGIONS : u64 = 2;

	pub const BLOCK_REGION_TABLE : u64 = 4;

	pub const REGION_TABLE : u64 =
	    BLOCK_REGION_TABLE +
	    super::size::BLOCK_REGION_TABLE;
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

	// Compute an offset in stable memory for a particular region ID.
	fn index(id : u16) -> u64 {
	    offset::BLOCK_REGION_TABLE + (id as u64 * size::BLOCK_REGION_TABLE_ENTRY as u64)
	}

	pub fn get(b:BlockId) -> Option<(RegionId, u16)> {
	    let raw = read_u16(index(b.0));
	    let rid = RegionId::from_u16(raw);
	    rid.map(|r| {
		let r_offset = r.0;
		(r, read_u16(index(r_offset) + 2))
	    })
	}
	pub fn set(b:BlockId, r:Option<RegionId>) {
	    write_u16(index(b.0), RegionId::into_u16(r))
	}
    }

    pub mod region_table {
	// invariant (for now, pre-GC integration):
	//  all regions whose IDs are below the total_allocated_regions are valid.
	use super::{offset, size};

	fn index(id : u16) -> u64 {
	    offset::REGION_TABLE + id as u64 * size::REGION_TABLE_ENTRY as u64
	}

	// to do:
	// - get_region_size
	// - set_region_size


    }
}

#[ic_mem_fn]
pub unsafe fn region_new<M: Memory>(mem: &mut M) -> Value {
    let r_ptr = mem.alloc_words(size_of::<Region>() + Words(1));
    // NB. cannot use as_region() here as we didn't write the header yet
    let region = r_ptr.get_ptr() as *mut Region;
    (*region).header.tag = TAG_REGION;
    (*region).id = NEXT_REGION_ID;
    NEXT_REGION_ID += 1;
    (*region).page_count = 0;
    (*region).vec_pages = alloc_blob(mem, Bytes(0));
    if true { // to do -- use this to eventually replace NEXT_REGION_ID
	let c = meta_data::total_allocated_regions::get();
	meta_data::total_allocated_regions::set(c + 1);
    }
    Value::from_ptr(region as usize)
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
    let r = r.as_region();
    let new_pages_ = new_pages as u32;
    let old_page_count = (*r).page_count;
    let old_block_count = (old_page_count + 127) / 128;
    let new_block_count = (old_page_count + new_pages_ + 127) / 128;
    let inc_block_count = new_block_count - old_block_count;
    if true { // Update the total allocated blocks:
	let c = meta_data::total_allocated_blocks::get();
	meta_data::total_allocated_blocks::set(c + inc_block_count as u64);
    }
    (*r).page_count += new_pages_;
    let new_vec_pages = alloc_blob(mem, Bytes(new_block_count * 2));
    let old_vec_byte_count = old_block_count * 2;
    let new_vec_byte_count = new_block_count * 2;
    for i in 0..old_vec_byte_count {
	let new_pages = new_vec_pages.get_ptr() as *mut Blob;
	let old_pages = (*r).vec_pages.get_ptr() as *mut Blob;
	new_pages.set(i, old_pages.get(i));
    }
    //  ## choose and record new block IDs:
    //    - call meta_data::block_region_table::set_block_region(r.id, block_id)
    //             for each block_id in old_total_blocks..new_total_blocks-1
    //  ## save new block IDs into new_vec_pages
    //    - call new_vec_pages.set(byte_offset)
    //            for byte_offset in old_vec_byte_count..new_vec_byte_count
    //              if the byte_offset is even, vs odd, ...

    (*r).vec_pages = new_vec_pages;
    old_page_count.into()
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
    Value::from_scalar(NEXT_REGION_ID as u32)
}
