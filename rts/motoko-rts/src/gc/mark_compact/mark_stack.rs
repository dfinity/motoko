//! A mark stack implementation for garbage collection.

use crate::constants::WORD_SIZE;
use crate::page_alloc::{Page, PageAlloc};
use crate::types::Tag;

use alloc::vec;
use alloc::vec::Vec;
use core::convert::TryFrom;

pub struct MarkStack<P: PageAlloc> {
    /// Pages allocated so far. Never empty. Always push to and pop from the last page.
    pages: Vec<P::Page>,

    /// Page allocator used to allocate new mark stack pages
    page_alloc: P,

    /// Location in `current_page` for the next allocation
    hp: usize,
}

impl<P: PageAlloc> MarkStack<P> {
    pub unsafe fn new(page_alloc: P) -> Self {
        let page = page_alloc.alloc();
        let hp = page.contents_start();
        MarkStack {
            pages: vec![page],
            page_alloc,
            hp,
        }
    }

    pub unsafe fn free(self) {
        let page_alloc = self.page_alloc;
        for page in self.pages {
            page_alloc.free(page);
        }
    }

    fn current_page(&self) -> &P::Page {
        self.pages.last().unwrap()
    }

    pub unsafe fn push(&mut self, obj: usize, obj_tag: Tag) {
        let new_hp = self.hp + (WORD_SIZE as usize) * 2;
        let current_page = self.current_page();
        let page_end = current_page.end();

        if new_hp >= page_end {
            if new_hp > page_end {
                // Fill unused word with 0. We allocate 2 words at a time so the free space can be
                // at most one word.
                *(self.hp as *mut u32) = 0;
            }

            let new_page = self.page_alloc.alloc();
            let hp = new_page.contents_start();

            self.pages.push(new_page);

            *(hp as *mut usize) = obj;
            // try_from disappears on Wasm
            *(hp as *mut usize).add(1) = usize::try_from(obj_tag).unwrap();

            self.hp = hp + (WORD_SIZE as usize) * 2;
        } else {
            *(self.hp as *mut usize) = obj;
            // try_from disappears on Wasm
            *(self.hp as *mut usize).add(1) = usize::try_from(obj_tag).unwrap();

            self.hp += (WORD_SIZE as usize) * 2;
        }
    }

    pub unsafe fn pop(&mut self) -> Option<(usize, Tag)> {
        let current_page = self.current_page();
        if self.hp == current_page.contents_start() {
            if self.pages.len() == 1 {
                // Stack empty
                return None;
            }

            // Free the current page, pop from previous page
            // TODO: Use unwrap_unchecked, we check the length above
            let empty_page = self.pages.pop().unwrap();
            self.page_alloc.free(empty_page);

            self.hp = self.current_page().end();
        }

        if *((self.hp - (WORD_SIZE as usize)) as *const u32) == 0 {
            self.hp -= (WORD_SIZE as usize) * 3;
        } else {
            self.hp -= WORD_SIZE as usize * 2;
        }

        let p = *(self.hp as *const usize);
        let tag = *(self.hp as *const usize).add(1) as u32;

        Some((p, tag))
    }
}
