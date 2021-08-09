use super::{Page, PageAlloc, PageHeader};
use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;

use core::arch::wasm32;
use core::convert::TryFrom;

/// A `PageAlloc` implementation for the IC
pub struct IcPageAlloc {}

/// A `Page` implementation for the IC. Currently maps to a single Wasm page, but we may want to
/// chage that in the future.
// NB. first page is special: the contents start at heap base instead of 0.
#[derive(Debug, Clone, Copy)]
pub struct IcPage {
    // Wasm page number for this page
    wasm_page_num: u16,
}

/// Free list for `IcPageAlloc`. It's important that this is static data rather than a
/// `IcPageAlloc` field, otherwise `IcPageAlloc` won't disappear in the generated code.
///
/// Initially the first page is free (TODO: won't work if static data is multiple pages).
///
/// `IcPage::start` accounts for static data. Pages linked with the `next` field. `prev` is not
/// used in this list.
static mut FREE_PAGES: Option<IcPage> = Some(IcPage { wasm_page_num: 0 });

impl PageAlloc for IcPageAlloc {
    type Page = IcPage;

    unsafe fn alloc(&mut self) -> IcPage {
        match FREE_PAGES.take() {
            None => {
                let wasm_page_num = wasm32::memory_grow(0, 1);

                // First page should be in use
                debug_assert_ne!(wasm_page_num, 0);

                if wasm_page_num == usize::MAX {
                    rts_trap_with("Cannot grow memory");
                }

                IcPage {
                    wasm_page_num: u16::try_from(wasm_page_num).unwrap(),
                }
            }
            Some(free_page) => {
                FREE_PAGES = free_page.take_next();
                free_page
            }
        }
    }

    unsafe fn free(&mut self, page: IcPage) {
        // No need to set prev
        page.set_next(FREE_PAGES);
        FREE_PAGES = Some(page);
    }
}

impl Page for IcPage {
    unsafe fn start(&self) -> usize {
        // First page is special: it contains static data
        // TODO: This will break if static data is multiple pages
        if self.wasm_page_num == 0 {
            // Skip static data
            crate::get_heap_base() as usize
        } else {
            (usize::from(self.wasm_page_num) * WASM_PAGE_SIZE.as_usize()) as usize
        }
    }

    unsafe fn contents_start(&self) -> usize {
        (self.start() as *const PageHeader<IcPage>).add(1) as usize
    }

    unsafe fn end(&self) -> usize {
        (usize::from(self.wasm_page_num) + 1) * WASM_PAGE_SIZE.as_usize()
    }

    unsafe fn prev(&self) -> Option<IcPage> {
        (self.start() as *mut PageHeader<IcPage>).prev()
    }

    unsafe fn set_prev(&self, prev: Option<IcPage>) {
        (self.start() as *mut PageHeader<IcPage>).set_prev(prev)
    }

    unsafe fn next(&self) -> Option<IcPage> {
        (self.start() as *mut PageHeader<IcPage>).next()
    }

    unsafe fn set_next(&self, next: Option<IcPage>) {
        (self.start() as *mut PageHeader<IcPage>).set_next(next)
    }
}
