#[cfg(feature = "ic")]
pub mod ic;

use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;

use core::arch::wasm32;
use core::convert::TryFrom;

/// Trait for page allocators. A page is a unit of allocation from the underlying systme (Wasm, OS,
/// some kind of mock or simulation in tests etc.).
pub trait PageAlloc {
    type Page: Page;

    unsafe fn alloc(&mut self) -> Self::Page;

    unsafe fn free(&mut self, page: Self::Page);
}

/// Trait for allocation units from the underlying system (Wasm, OS, ...). Page state can be held
/// withing the pages, or externally (e.g. in an array indexed by a page), or a combination of
/// both.

// TODO: `Copy` is convenient but not sure if really necessary
pub trait Page: Copy + Sized {
    /// Get the start of this page
    fn start(&self) -> usize;

    /// Get the start of this page after the page header (e.g. linked list fields)
    fn contents_start(&self) -> usize;

    /// Get the end (exclusive) of this page
    fn end(&self) -> usize;

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
}

/// A header type for pages that implements the doubly linked list intrusively
#[repr(packed)]
pub struct PageHeader<P: Copy + Sized> {
    pub next: Option<P>,
    pub prev: Option<P>,
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
}
