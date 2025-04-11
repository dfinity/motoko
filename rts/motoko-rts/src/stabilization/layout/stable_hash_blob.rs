use crate::{
    stabilization::{
        layout::StableObjectKind, serialization::stable_memory_stream::StableMemoryStream,
    },
    types::{count_fields_in_hash_blob, size_of, FwdPtr, Tag, Value, TAG_FWD_PTR},
};

use super::{stable_blob::StableBlob, StableTag};

#[repr(C)]
pub struct HashBlob {
    tag: StableTag,
    header: StableBlob,
}

/// Resolve number of fields in object and closure during serialization.
/// This requires a look up in the hash blob, which may however already have been
/// serialized to stable memory.
pub unsafe fn size_by_hash_blob(
    stable_memory: &StableMemoryStream,
    main_hash_blob: Value,
) -> usize {
    // Do not call tag as it resolves the forwarding pointer.
    unsafe {
        let main_tag = *(main_hash_blob.get_ptr() as *const Tag);
        if main_tag == TAG_FWD_PTR {
            // The hash blob has already been moved to stable memory.
            let target_location = (*(main_hash_blob.get_ptr() as *mut FwdPtr)).fwd;
            let stable_offset = target_location.get_ptr() as u64;
            let stable_hash_blob = stable_memory.read_preceding::<HashBlob>(stable_offset);
            assert!(stable_hash_blob.tag.decode() == StableObjectKind::BlobBytes);
            let hash_blob_length = stable_hash_blob.header.byte_length() as usize;
            let hash_entry_length = size_of::<u64>().to_bytes().as_usize();
            debug_assert_eq!(hash_blob_length % hash_entry_length, 0);
            hash_blob_length / hash_entry_length
        } else {
            count_fields_in_hash_blob(main_hash_blob)
        }
    }
}
