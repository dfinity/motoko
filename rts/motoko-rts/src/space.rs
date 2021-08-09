use crate::page_alloc::{Page, PageAlloc};
use crate::types::{skew, SkewedPtr, Words};

/// A space is an allocation area, consists of a linked list of pages.
pub struct Space<P: PageAlloc> {
    /// First page of this space. Scan through the space with `Page::next()`.
    first_page: P::Page,

    /// `hp` is in this page and allocation is done here. `Page::prev()` gives us previous (filled)
    /// page of this space.
    current_page: P::Page,

    /// Allocation pointer in the current page
    hp: usize,
}

impl<P: PageAlloc> Space<P> {
    pub unsafe fn new(page_alloc: &mut P) -> Space<P> {
        let page = page_alloc.alloc();

        Space {
            first_page: page,
            current_page: page,
            hp: page.contents_start(),
        }
    }

    /// Get the first page of this space. Can be used to scan the space from start to end.
    /// (e.g. when scavenging to-space in gc)
    pub fn first_page(&self) -> P::Page {
        self.first_page
    }

    pub unsafe fn alloc_words(&mut self, n: Words<u32>) -> SkewedPtr {
        let bytes = n.to_bytes().as_usize();

        let alloc = if self.hp + bytes >= self.current_page.end() {
            let new_page = P::alloc();

            new_page.set_prev(Some(self.current_page));
            self.current_page.set_next(Some(new_page));

            let alloc = new_page.start();
            self.hp = alloc + bytes;
            alloc
        } else {
            self.hp + bytes
        };

        skew(alloc)
    }

    /// Free all pages of the space. The space itself should not be used afterwards.
    pub unsafe fn free(self) {
        let mut next = Some(self.first_page);
        while let Some(page) = next {
            next = page.next();
            page.free();
        }
    }
}
