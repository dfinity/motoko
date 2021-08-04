use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;

use core::arch::wasm32;
use core::convert::TryFrom;

// Initially the first page is free (TODO: won't work if static data is multiple pages).
// `Page::start` accounts for static data.
static mut FREE_PAGES: Option<Page> = Some(Page { wasm_page_num: 0 });

/// A chunk of memory that can be used for allocation. Pages can be linked together.
// NB. currently this is a single Wasm page, but this may change in the future. When compiling for
// IC first page is special: the contents start at heap base instead of 0.
#[derive(Debug, Clone, Copy)]
pub struct Page {
    // Wasm page number for this page
    wasm_page_num: u16,
}

/// Header of a page. `page.start` will point to this struct. Rest of the page is available for
/// allocation.
struct PageHeader {
    next: Option<Page>,
}

impl Page {
    unsafe fn page_contents_start(&self) -> usize {
        // First page is special: it contains static data
        // TODO: This will break if static data is multiple pages
        if self.wasm_page_num == 0 {
            // Skip static data
            crate::memory::ic::get_heap_base() as usize
        } else {
            usize::from(self.wasm_page_num) * WASM_PAGE_SIZE.as_usize()
        }
    }

    /// Get the beginning of allocation area of this page
    pub unsafe fn start(&self) -> usize {
        // Skip page header
        (self.page_contents_start() as *const PageHeader).add(1) as usize
    }

    /// Get the end of allocation of this page
    pub fn end(&self) -> usize {
        (usize::from(self.wasm_page_num) + 1) * WASM_PAGE_SIZE.as_usize()
    }

    pub unsafe fn next(&self) -> Option<Page> {
        let header = self.page_contents_start() as *const PageHeader;
        (*header).next
    }

    pub unsafe fn set_next(&self, next: Option<Page>) {
        let header = self.page_contents_start() as *mut PageHeader;
        (*header).next = next;
    }

    pub unsafe fn take_next(&self) -> Option<Page> {
        let header = self.page_contents_start() as *mut PageHeader;
        (*header).next.take()
    }
}

pub unsafe fn alloc_page() -> Page {
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

pub unsafe fn free_page(page: Page) {
    page.set_next(FREE_PAGES);
    FREE_PAGES = Some(page);
}
