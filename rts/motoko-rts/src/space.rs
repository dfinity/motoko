use crate::page_alloc::{alloc_page, free_page, Page};
use crate::types::{skew, SkewedPtr, Words};

/// A space is basically an allocation area, consists of a linked list of pages.
pub struct Space {
    /// Current page. `next` pointer gives us previous (filled) pages of this space.
    page: Page,

    /// Allocation pointer in the current page
    hp: usize,
}

impl Space {
    pub unsafe fn alloc_words(&mut self, n: Words<u32>) -> SkewedPtr {
        let bytes = n.to_bytes().as_usize();

        let alloc = if self.hp + bytes >= self.page.end() {
            let new_page = alloc_page();
            let alloc = alloc_page().start();
            self.hp = alloc + bytes;
            alloc
        } else {
            self.hp + bytes
        };

        skew(alloc)
    }

    pub unsafe fn free(self) {
        let mut page = Some(self.page);
        while let Some(page_) = page.take() {
            page = *(page_.start() as *const Option<Page>);
            free_page(page_);
        }
    }
}
