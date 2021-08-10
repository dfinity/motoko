//! A mark stack implementation for garbage collection.

use crate::constants::WORD_SIZE;
use crate::page_alloc::{Page, PageAlloc};
use crate::space::Space;
use crate::types::{Blob, Tag, Words};

use core::convert::TryFrom;
use core::ptr::null_mut;

pub struct MarkStack<P: PageAlloc> {
    /// Current page for pushing new objects. Follow `prev` links when popping and freeing the mark
    /// stack.
    current_page: P::Page,

    /// Page allocator used to allocate new mark stack pages
    page_alloc: P,

    /// Location in `current_page` for the next allocation
    hp: usize,
}

impl<P: PageAlloc> MarkStack<P> {
    pub unsafe fn new(mut page_alloc: P) -> Self {
        let current_page = page_alloc.alloc();
        let hp = current_page.contents_start();
        MarkStack {
            current_page,
            page_alloc,
            hp,
        }
    }

    pub unsafe fn free(mut self) {
        let mut page_alloc = self.page_alloc;
        let mut page = Some(self.current_page);
        while let Some(page_) = page {
            page = page_.prev();
            page_alloc.free(page_);
        }
    }

    pub unsafe fn push(&mut self, obj: usize, obj_tag: Tag) {
        let new_hp = self.hp + (WORD_SIZE as usize) * 2;
        let page_end = self.current_page.end();

        if new_hp >= page_end {
            if new_hp > page_end {
                // Fill unused word with 0. We allocate 2 words at a time so the free space can be
                // at most one word.
                *(self.hp as *mut u32) = 0;
            }

            let new_page = self.page_alloc.alloc();
            new_page.set_prev(Some(self.current_page));

            self.current_page = new_page;
            let hp = new_page.contents_start();

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
        if self.hp == self.current_page.contents_start() {
            let prev_page = match self.current_page.prev() {
                None => return None,
                Some(prev_page) => prev_page,
            };

            self.page_alloc.free(self.current_page);
            self.current_page = prev_page;
            self.hp = prev_page.end();
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
