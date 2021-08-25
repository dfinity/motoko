use super::{Page, PageAlloc, PageHeader};
use crate::bitmap::Bitmap;
use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;

use alloc::vec::Vec;
use core::arch::wasm32;
use core::convert::TryFrom;

/// A `PageAlloc` implementation for the IC
#[derive(Clone)]
pub struct IcPageAlloc {}

/// A `Page` implementation for the IC. Currently maps to a single Wasm page, but that may change
/// in the future.
#[derive(Debug, Clone, Copy)]
pub struct IcPage {
    // Wasm page number for this page
    wasm_page_num: u16,
}

/// Free list for `IcPageAlloc`. It's important that this is static data rather than a
/// `IcPageAlloc` field, otherwise `IcPageAlloc` will need to be passed around in RTS functions and
/// to the RTS by the mutator.
static mut FREE_PAGES: Vec<IcPage> = Vec::new();

impl PageAlloc for IcPageAlloc {
    type Page = IcPage;

    unsafe fn alloc(&self) -> IcPage {
        FREE_PAGES.pop().unwrap_or_else(|| {
            let wasm_page_num = wasm32::memory_grow(0, 1);

            // First page should be in use
            debug_assert_ne!(wasm_page_num, 0);

            if wasm_page_num == usize::MAX {
                rts_trap_with("Cannot grow memory");
            }

            // TODO: Should be safe to use `as u16` here
            IcPage {
                wasm_page_num: u16::try_from(wasm_page_num).unwrap(),
            }
        })
    }

    unsafe fn free(&self, page: IcPage) {
        FREE_PAGES.push(page)
    }

    unsafe fn get_address_page(&self, addr: usize) -> IcPage {
        IcPage {
            wasm_page_num: (addr / WASM_PAGE_SIZE.as_usize()) as u16,
        }
    }

    unsafe fn in_static_heap(&self, addr: usize) -> bool {
        addr < crate::get_heap_base() as usize
    }
}

impl Page for IcPage {
    unsafe fn start(&self) -> usize {
        usize::from(self.wasm_page_num) * WASM_PAGE_SIZE.as_usize()
    }

    unsafe fn contents_start(&self) -> usize {
        (self.start() as *const PageHeader).add(1) as usize
    }

    unsafe fn end(&self) -> usize {
        (usize::from(self.wasm_page_num) + 1) * WASM_PAGE_SIZE.as_usize()
    }

    unsafe fn get_bitmap(&self) -> Option<*mut Bitmap> {
        (self.start() as *mut PageHeader).get_bitmap()
    }

    unsafe fn set_bitmap(&self, bitmap: Option<Bitmap>) {
        (self.start() as *mut PageHeader).set_bitmap(bitmap)
    }

    unsafe fn take_bitmap(&self) -> Option<Bitmap> {
        (self.start() as *mut PageHeader).take_bitmap()
    }
}
