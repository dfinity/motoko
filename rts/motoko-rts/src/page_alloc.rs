mod free_lists;

use crate::bitmap::Bitmap;
use crate::constants::WASM_PAGE_SIZE;
use crate::types::Bytes;

#[cfg(feature = "ic")]
pub mod ic;

pub const PAGE_SIZE: Bytes<u32> = WASM_PAGE_SIZE;

// Cannot use Div and Mul implementations of Bytes in const context, so using WASM_PAGE_SIZE.
// LARGE_OBJECT_THRESHOLD = (PAGE_SIZE / 3) * 4
pub const LARGE_OBJECT_THRESHOLD: Bytes<u32> = Bytes((WASM_PAGE_SIZE.0 / 4) * 3);

#[derive(Debug, Clone, Copy)]
pub struct WasmPage {
    /// Wasm page number, i.e. value returned by `memory_grow` instruction
    pub page_num: u16,

    /// Number of pages
    pub n_pages: u16,
}

/// Trait for page allocators. A page is a unit of allocation from the underlying systme (Wasm, OS,
/// some kind of mock or simulation in tests etc.).
pub trait PageAlloc: Clone {
    /// Allocate single page
    unsafe fn alloc(&self) -> WasmPage;

    /// Allocate given number of consecutive pages
    unsafe fn alloc_pages(&self, n_pages: u16) -> WasmPage;

    /// Free a page
    unsafe fn free(&self, page: WasmPage);

    /// Get the start address of the page that contains the given address. Only works on
    /// single-page pages! Can be used to get page header.
    unsafe fn get_address_page_start(&self, addr: usize) -> usize;
}

impl WasmPage {
    /// Get the start of this page
    pub unsafe fn start(&self) -> usize {
        usize::from(self.page_num) * WASM_PAGE_SIZE.as_usize()
    }

    /// Get the start of this page after the page header (e.g. linked list fields)
    pub unsafe fn contents_start(&self) -> usize {
        (self.start() as *const PageHeader).add(1) as usize
    }

    /// Get the end (exclusive) of this page
    pub unsafe fn end(&self) -> usize {
        (usize::from(self.page_num) + 1) * WASM_PAGE_SIZE.as_usize()
    }

    /// Size of the page, excluding any headers
    pub unsafe fn size(&self) -> usize {
        self.end() - self.contents_start()
    }

    pub unsafe fn get_bitmap(&self) -> Option<*mut Bitmap> {
        (self.start() as *mut PageHeader).get_bitmap()
    }

    pub unsafe fn set_bitmap(&self, bitmap: Option<Bitmap>) {
        (self.start() as *mut PageHeader).set_bitmap(bitmap)
    }

    /// `Option::take` for the bitmap field
    pub unsafe fn take_bitmap(&self) -> Option<Bitmap> {
        (self.start() as *mut PageHeader).take_bitmap()
    }
}

pub struct PageHeader {
    // Bitmap used when marking the page for garbage collection
    pub bitmap: Option<Bitmap>,
}

impl PageHeader {
    pub unsafe fn get_bitmap(self: *mut Self) -> Option<*mut Bitmap> {
        (*self).bitmap.as_mut().map(|r| r as *mut Bitmap)
    }

    pub unsafe fn set_bitmap(self: *mut Self, bitmap: Option<Bitmap>) {
        (*self).bitmap = bitmap;
    }

    pub unsafe fn take_bitmap(self: *mut Self) -> Option<Bitmap> {
        (*self).bitmap.take()
    }
}

pub struct LargePageHeader {
    pub prev: *mut LargePageHeader,
    pub next: *mut LargePageHeader,
}
