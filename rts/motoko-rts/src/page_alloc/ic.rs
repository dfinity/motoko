use super::free_lists::{self, WasmPage};
use super::{LargePageHeader, PageAlloc};
use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;

use core::arch::wasm32;

/// A `PageAlloc` implementation for the IC
#[derive(Clone)]
pub struct IcPageAlloc {}

fn alloc_wasm_pages(n_pages: u16) -> u16 {
    unsafe {
        let page_num = wasm32::memory_grow(0, usize::from(n_pages));

        if page_num == usize::MAX {
            rts_trap_with("Cannot grow memory");
        }
        page_num as u16
    }
}

impl PageAlloc for IcPageAlloc {
    type Page = WasmPage;

    unsafe fn alloc(&self) -> WasmPage {
        free_lists::alloc(alloc_wasm_pages)
    }

    unsafe fn alloc_pages(&self, n_pages: u16) -> WasmPage {
        free_lists::alloc_pages(alloc_wasm_pages, n_pages)
    }

    unsafe fn free(&self, page: WasmPage) {
        free_lists::free(page)
    }

    unsafe fn free_large(&self, header: *const LargePageHeader) {
        let page = WasmPage {
            page_num: (header as usize / WASM_PAGE_SIZE.as_usize()) as u16,
            n_pages: (*header).n_pages,
        };

        self.free(page);
    }

    unsafe fn get_address_page_start(&self, addr: usize) -> usize {
        // TODO: Implement debug mode checks:
        // - `addr` is not in a "large" page
        // - `addr` is within heap

        // Mask least significant bits to get Wasm page start
        addr & !(WASM_PAGE_SIZE.as_usize() - 1)
    }
}
