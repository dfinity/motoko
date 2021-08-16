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

    /// Get the previous page in the list
    unsafe fn prev(&self) -> Option<Self>;

    /// Get the next page in the list
    unsafe fn next(&self) -> Option<Self>;

    /// Set the previous page in the list
    unsafe fn set_prev(&self, prev: Option<Self>);

    /// Set the next page in the list
    unsafe fn set_next(&self, next: Option<Self>);

    /// `Option::take` for the prev field
    unsafe fn take_prev(&self) -> Option<Self> {
        let prev = self.prev();
        self.set_prev(None);
        prev
    }

    /// `Option::take` for the next field
    unsafe fn take_next(&self) -> Option<Self> {
        let next = self.next();
        self.set_next(None);
        next
    }

    unsafe fn get_bitmap(&self) -> Option<*mut Bitmap>;

    unsafe fn set_bitmap(&self, bitmap: Option<Bitmap>);

    /// `Option::take` for the bitmap field
    unsafe fn take_bitmap(&self) -> Option<Bitmap>;
}

#[repr(packed)]
pub struct PageHeader<P: Clone> {
    pub next: Option<P>,
    pub prev: Option<P>,

    // Bitmap used when marking the page for garbage collection
    pub bitmap: Option<Bitmap>,
}

impl<P: Clone> PageHeader<P> {
    pub unsafe fn prev(self: *mut Self) -> Option<P> {
        (*self).prev.clone()
    }

    pub unsafe fn set_prev(self: *mut Self, prev: Option<P>) {
        (*self).prev = prev;
    }

    pub unsafe fn take_prev(self: *mut Self) -> Option<P> {
        (*self).prev.take()
    }

    pub unsafe fn next(self: *mut Self) -> Option<P> {
        (*self).next.clone()
    }

    pub unsafe fn set_next(self: *mut Self, next: Option<P>) {
        (*self).next = next;
    }

    pub unsafe fn take_next(self: *mut Self) -> Option<P> {
        (*self).next.take()
    }

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
