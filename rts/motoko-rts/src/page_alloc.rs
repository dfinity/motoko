use crate::bitmap::Bitmap;

#[cfg(feature = "ic")]
pub mod ic;

/// Trait for page allocators. A page is a unit of allocation from the underlying systme (Wasm, OS,
/// some kind of mock or simulation in tests etc.).
pub trait PageAlloc: Clone {
    type Page: Page;

    unsafe fn alloc(&self) -> Self::Page;

    unsafe fn free(&self, page: Self::Page);

    /// Get the page of a given address. May panic if address does not belong to a page for this
    /// allocator.
    unsafe fn get_address_page(&self, addr: usize) -> Self::Page;
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

#[repr(packed)]
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
