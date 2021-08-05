use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;

use core::arch::wasm32;
use core::convert::TryFrom;

// Initially the first page is free (TODO: won't work if static data is multiple pages).
// `Page::start` accounts for static data. Pages linked with the `next` field. `prev` is not used.
static mut FREE_PAGES: Option<Page> = Some(Page { wasm_page_num: 0 });

/// A chunk of memory that can be used for allocation. Pages can be linked together.
// NB. currently this is a single Wasm page, but this may change in the future. When compiling for
// IC first page is special: the contents start at heap base instead of 0.
#[derive(Debug, Clone, Copy)]
pub struct Page {
    // Wasm page number for this page
    wasm_page_num: u16,
}

/// Header of a page. `page.page_contents_start()` will point to this struct. Rest of the page is
/// available for allocation.
struct PageHeader {
    next: Option<Page>,
    prev: Option<Page>,
}

impl Page {
    /// Allocate a new page
    pub unsafe fn alloc() -> Page {
        match FREE_PAGES.take() {
            None => {
                let wasm_page_num = wasm32::memory_grow(0, 1);

                // First page should be in use
                debug_assert_ne!(wasm_page_num, 0);

                if wasm_page_num == usize::MAX {
                    rts_trap_with("Cannot grow memory");
                }

                Page {
                    wasm_page_num: u16::try_from(wasm_page_num).unwrap(),
                }
            }
            Some(free_page) => {
                FREE_PAGES = free_page.take_next();
                free_page
            }
        }
    }

    /// Free the page
    pub unsafe fn free(self) {
        // No need to set prev
        self.set_next(FREE_PAGES);
        FREE_PAGES = Some(self);
    }

    unsafe fn page_contents_start(&self) -> *mut PageHeader {
        // First page is special: it contains static data
        // TODO: This will break if static data is multiple pages
        if self.wasm_page_num == 0 {
            // Skip static data
            crate::memory::ic::get_heap_base() as *mut PageHeader
        } else {
            (usize::from(self.wasm_page_num) * WASM_PAGE_SIZE.as_usize()) as *mut PageHeader
        }
    }

    /// Get the beginning of allocation area of this page
    pub unsafe fn start(&self) -> usize {
        // Skip page header
        self.page_contents_start().add(1) as usize
    }

    /// Get the end of allocation of this page
    pub fn end(&self) -> usize {
        (usize::from(self.wasm_page_num) + 1) * WASM_PAGE_SIZE.as_usize()
    }

    pub unsafe fn prev(&self) -> Option<Page> {
        self.page_contents_start().prev()
    }

    pub unsafe fn set_prev(&self, next: Option<Page>) {
        self.page_contents_start().set_prev(next)
    }

    pub unsafe fn take_prev(&self) -> Option<Page> {
        self.page_contents_start().take_prev()
    }

    pub unsafe fn next(&self) -> Option<Page> {
        self.page_contents_start().next()
    }

    pub unsafe fn set_next(&self, next: Option<Page>) {
        self.page_contents_start().set_next(next)
    }

    pub unsafe fn take_next(&self) -> Option<Page> {
        self.page_contents_start().take_next()
    }
}

impl PageHeader {
    unsafe fn prev(self: *mut Self) -> Option<Page> {
        (*self).prev
    }

    unsafe fn set_prev(self: *mut Self, prev: Option<Page>) {
        (*self).prev = prev;
    }

    unsafe fn take_prev(self: *mut Self) -> Option<Page> {
        (*self).prev.take()
    }

    unsafe fn next(self: *mut Self) -> Option<Page> {
        (*self).next
    }

    unsafe fn set_next(self: *mut Self, next: Option<Page>) {
        (*self).next = next;
    }

    unsafe fn take_next(self: *mut Self) -> Option<Page> {
        (*self).next.take()
    }
}
