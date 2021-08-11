use crate::page_alloc::{Page, PageAlloc};
use crate::rts_trap_with;
use crate::types::{size_of, skew, Array, Blob, Bytes, SkewedPtr, Words, TAG_ARRAY, TAG_BLOB};

/// A space is an allocation area, consists of a linked list of pages.
pub struct Space<P: PageAlloc> {
    /// Page allocator
    page_alloc: P,

    /// First page of this space. Scan through the space with `Page::next()`.
    first_page: P::Page,

    /// `hp` is in this page and allocation is done here. `Page::prev()` gives us previous (filled)
    /// page of this space.
    current_page: P::Page,

    /// Allocation pointer in the current page
    hp: usize,

    /// Total amount allocated in this space so far. Note that when we allocate a new space because
    /// an allocation request is too much for the current space, rest of the current space will be
    /// accounted as allocated. (TODO: not sure about this part)
    total_alloc: usize,
}

impl<P: PageAlloc> Space<P> {
    pub unsafe fn new(mut page_alloc: P) -> Space<P> {
        let page = page_alloc.alloc();

        Space {
            page_alloc,
            first_page: page,
            current_page: page,
            hp: page.contents_start(),
            total_alloc: 0,
        }
    }

    /// Get the first page of this space. Can be used to scan the space from start to end.
    /// (e.g. when scavenging to-space in gc)
    pub fn first_page(&self) -> P::Page {
        self.first_page
    }

    pub fn total_alloc(&self) -> usize {
        self.total_alloc
    }

    pub unsafe fn alloc_words(&mut self, n: Words<u32>) -> SkewedPtr {
        let bytes = n.to_bytes().as_usize();

        let alloc = if self.hp + bytes >= self.current_page.end() {
            let new_page = self.page_alloc.alloc();

            // Rest of the page is considered allocated
            self.total_alloc += self.current_page.end() - self.hp;

            new_page.set_prev(Some(self.current_page));
            self.current_page.set_next(Some(new_page));

            let alloc = new_page.start();
            self.hp = alloc + bytes;
            alloc
        } else {
            self.hp + bytes
        };

        self.total_alloc += bytes;

        skew(alloc)
    }

    /// Free all pages of the space. The space itself should not be used afterwards.
    pub unsafe fn free(&mut self) {
        let mut next = Some(self.first_page);
        while let Some(page) = next {
            next = page.next();
            self.page_alloc.free(page);
        }
    }

    pub unsafe fn alloc_array(&mut self, len: u32) -> SkewedPtr {
        // Array payload should not be larger than half of the memory
        if len > 1 << (32 - 2 - 1) {
            // 2 for word size, 1 to divide by two
            rts_trap_with("Array allocation too large");
        }

        let skewed_ptr = self.alloc_words(size_of::<Array>() + Words(len));

        let ptr: *mut Array = skewed_ptr.unskew() as *mut Array;
        (*ptr).header.tag = TAG_ARRAY;
        (*ptr).len = len;

        skewed_ptr
    }

    pub unsafe fn alloc_blob(&mut self, size: Bytes<u32>) -> SkewedPtr {
        let ptr = self.alloc_words(size_of::<Blob>() + size.to_words());
        let blob = ptr.unskew() as *mut Blob;
        (*blob).header.tag = TAG_BLOB;
        (*blob).len = size;
        ptr
    }

    /// Get page of an object in this space. May panic or return incorrect results if the object is
    /// not in the space.
    pub unsafe fn get_address_page(&self, obj: usize) -> P::Page {
        self.page_alloc.get_address_page(obj)
    }
}
