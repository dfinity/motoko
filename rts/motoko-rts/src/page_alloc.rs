// Public for testing
pub mod free_lists;

use crate::bitmap::Bitmap;
use crate::constants::WASM_PAGE_SIZE;
use crate::types::Bytes;

#[cfg(feature = "ic")]
pub mod ic;

pub const PAGE_SIZE: Bytes<u32> = WASM_PAGE_SIZE;

// Cannot use Div and Mul implementations of Bytes in const context, so using WASM_PAGE_SIZE.
// LARGE_OBJECT_THRESHOLD = (PAGE_SIZE / 3) * 4
pub const LARGE_OBJECT_THRESHOLD: Bytes<u32> = Bytes((WASM_PAGE_SIZE.0 / 4) * 3);

/// Trait for page allocators. A page is a unit of allocation from the underlying systme (Wasm, OS,
/// some kind of mock or simulation in tests etc.).
pub trait PageAlloc: Clone {
    type Page: Page;

    /// Allocate single page
    unsafe fn alloc(&self) -> Self::Page;

    /// Allocate given number of consecutive pages
    unsafe fn alloc_pages(&self, n_pages: u16) -> Self::Page;

    /// Free a page
    unsafe fn free(&self, page: Self::Page);

    /// Free a large page (i.e. multiple consecutive pages)
    unsafe fn free_large(&self, header: *const LargePageHeader);

    /// Get the start address of the page that contains the given address. Only works on
    /// single-page pages! Can be used to get page header.
    unsafe fn get_address_page_start(&self, addr: usize) -> usize;
}

/// Trait for allocation units from the underlying system (Wasm, OS, ...). Page state can be held
/// withing the pages, or externally (e.g. in an array indexed by a page), or a combination of
/// both.
pub trait Page: Clone {
    /// Get the start of this page
    unsafe fn start(&self) -> usize;

    /// Get the start of this page after the page header (e.g. linked list fields)
    unsafe fn contents_start(&self) -> usize;

    /// Get the end (exclusive) of this page
    unsafe fn end(&self) -> usize;

    /// Size of the page, excluding any headers
    unsafe fn size(&self) -> usize {
        self.end() - self.contents_start()
    }

    unsafe fn get_bitmap(&self) -> Option<*mut Bitmap>;

    unsafe fn set_bitmap(&self, bitmap: Option<Bitmap>);

    /// `Option::take` for the bitmap field
    unsafe fn take_bitmap(&self) -> Option<Bitmap>;
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
    pub n_pages: u16,
    _unused: u16, // TODO: This is to make sure page contents start at a word boundary
}
