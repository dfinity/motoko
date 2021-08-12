use crate::bitmap::Bitmap;

#[cfg(feature = "ic")]
pub mod ic;

/// Trait for page allocators. A page is a unit of allocation from the underlying systme (Wasm, OS,
/// some kind of mock or simulation in tests etc.).
pub trait PageAlloc: Clone {
    type Page: Page;

    unsafe fn alloc(&mut self) -> Self::Page;

    unsafe fn free(&mut self, page: Self::Page);

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

    unsafe fn get_bitmap(&self) -> Option<&Bitmap>;

    unsafe fn set_bitmap(&self, bitmap: Option<Bitmap>);

    /// `Option::take` for the bitmap field
    unsafe fn take_bitmap(&self) -> Option<Bitmap>;
}

#[repr(packed)]
pub struct PageHeader<P: Copy + Sized> {
    pub next: Option<P>,
    pub prev: Option<P>,

    // Bitmap used when marking the page for garbage collection
    pub bitmap: Option<Bitmap>,
}

impl<P: Copy + Sized> PageHeader<P> {
    unsafe fn prev(self: *mut Self) -> Option<P> {
        (*self).prev
    }

    unsafe fn set_prev(self: *mut Self, prev: Option<P>) {
        (*self).prev = prev;
    }

    unsafe fn take_prev(self: *mut Self) -> Option<P> {
        (*self).prev.take()
    }

    unsafe fn next(self: *mut Self) -> Option<P> {
        (*self).next
    }

    unsafe fn set_next(self: *mut Self, next: Option<P>) {
        (*self).next = next;
    }

    unsafe fn take_next(self: *mut Self) -> Option<P> {
        (*self).next.take()
    }

    unsafe fn get_bitmap<'a>(self: *mut Self) -> Option<&'a Bitmap> {
        (*self).bitmap.as_ref()
    }

    unsafe fn set_bitmap(self: *mut Self, bitmap: Option<Bitmap>) {
        (*self).bitmap = bitmap;
    }

    unsafe fn take_bitmap(self: *mut Self) -> Option<Bitmap> {
        (*self).bitmap.take()
    }
}
