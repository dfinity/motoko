use super::{Bitmap, Page, PageHeader, WASM_PAGE_SIZE};

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::vec::Vec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WasmPage {
    /// Wasm page number, i.e. value returned by `memory_grow` instruction
    pub page_num: u16,

    /// Number of pages
    pub n_pages: u16,
}

impl Page for WasmPage {
    fn page_idx(&self) -> u16 {
        self.page_num
    }

    unsafe fn start(&self) -> usize {
        usize::from(self.page_num) * WASM_PAGE_SIZE.as_usize()
    }

    unsafe fn contents_start(&self) -> usize {
        (self.start() as *const PageHeader).add(1) as usize
    }

    unsafe fn end(&self) -> usize {
        (usize::from(self.page_num) + 1) * WASM_PAGE_SIZE.as_usize()
    }

    unsafe fn get_bitmap(&self) -> Option<*mut Bitmap> {
        (self.start() as *mut PageHeader).get_bitmap()
    }

    unsafe fn set_bitmap(&self, bitmap: Option<Bitmap>) {
        (self.start() as *mut PageHeader).set_bitmap(bitmap)
    }

    unsafe fn take_bitmap(&self) -> Option<Bitmap> {
        (self.start() as *mut PageHeader).take_bitmap()
    }
}

// Public for testing
#[derive(Debug, PartialEq, Eq)]
pub struct SizeClass {
    pub n_pages: u16,
    // Set of Wasm pages
    pub pages: BTreeSet<u16>,
}

impl SizeClass {
    fn new(n_pages: u16) -> Self {
        SizeClass {
            n_pages,
            pages: BTreeSet::new(),
        }
    }

    fn insert(&mut self, wasm_page_num: u16) {
        let not_present = self.pages.insert(wasm_page_num);
        debug_assert!(not_present);
    }

    fn remove(&mut self, wasm_page_num: u16) {
        let present = self.pages.remove(&wasm_page_num);
        debug_assert!(present);
    }

    fn remove_first(&mut self) -> Option<u16> {
        self.pages.pop_first()
    }

    fn is_empty(&self) -> bool {
        self.pages.is_empty()
    }
}

/// Free pages sorted by start address (Wasm page num). Used to find pages to merge.
// page num -> num of pages
pub static mut FREE_PAGES_ADDR_SORTED: BTreeMap<u16, u16> = BTreeMap::new();

/// Free pages reverse-sorted by region size and start address (or Wasm page num), used to allocate.
pub static mut FREE_PAGES_SIZE_SORTED: Vec<SizeClass> = Vec::new();

unsafe fn get_size_class_idx(n_pages: u16) -> Result<usize, usize> {
    FREE_PAGES_SIZE_SORTED.binary_search_by_key(&n_pages, |size_class| size_class.n_pages)
}

unsafe fn add_free_page_size_sorted(wasm_page_num: u16, n_pages: u16) {
    match get_size_class_idx(n_pages) {
        Ok(size_class_idx) => {
            FREE_PAGES_SIZE_SORTED[size_class_idx].insert(wasm_page_num);
        }
        Err(new_size_class_idx) => {
            let mut size_class = SizeClass::new(n_pages);
            size_class.insert(wasm_page_num);
            FREE_PAGES_SIZE_SORTED.insert(new_size_class_idx, size_class);
        }
    }
}

unsafe fn remove_free_page_size_sorted(wasm_page_num: u16, n_pages: u16) {
    let size_class_idx = get_size_class_idx(n_pages).unwrap_or_else(|_| {
        panic!(
            "remove_free_size_sorted: No size class for {} pages",
            n_pages
        )
    });
    let size_class = &mut FREE_PAGES_SIZE_SORTED[size_class_idx];
    size_class.remove(wasm_page_num);
    if size_class.is_empty() {
        FREE_PAGES_SIZE_SORTED.remove(size_class_idx);
    }
}

// For testing: clears free lists.
pub unsafe fn clear() {
    FREE_PAGES_SIZE_SORTED.clear();
    FREE_PAGES_ADDR_SORTED.clear();
}

/// Allocate single page
pub unsafe fn alloc<GrowMemory>(grow_memory: GrowMemory) -> WasmPage
where
    GrowMemory: FnMut(u16) -> u16, // Wasm memory.grow
{
    // TODO: For single page allocs we always use the first free list, not need to binary search
    alloc_pages(grow_memory, 1)
}

/// Allocate multiple pages
pub unsafe fn alloc_pages<GrowMemory>(mut grow_memory: GrowMemory, n_pages: u16) -> WasmPage
where
    GrowMemory: FnMut(u16) -> u16, // Wasm memory.grow
{
    // Get the size class with n_pages or the smallest size class larger than n_pages
    match get_size_class_idx(n_pages) {
        Ok(size_class_idx) => {
            // TODO: Improve err msg
            let size_class = &mut FREE_PAGES_SIZE_SORTED[size_class_idx];
            let page_num = size_class.remove_first().unwrap();

            if size_class.is_empty() {
                FREE_PAGES_SIZE_SORTED.remove(size_class_idx);
            }

            let page = WasmPage { page_num, n_pages };

            let old = FREE_PAGES_ADDR_SORTED.remove(&page_num);
            debug_assert!(old.is_some());

            page
        }
        Err(size_class_idx) => {
            if size_class_idx == FREE_PAGES_SIZE_SORTED.len() {
                // No size class available for `n_pages`, allocate Wasm pages
                let page_num = grow_memory(n_pages);
                WasmPage { page_num, n_pages }
            } else {
                // We have a size class larger than requested. Get a page, free unused parts.
                let size_class = &mut FREE_PAGES_SIZE_SORTED[size_class_idx];

                // TODO: Improve err msg
                let page = size_class.remove_first().unwrap();

                let old = FREE_PAGES_ADDR_SORTED.remove(&page);
                debug_assert!(old.is_some());

                let free_page = WasmPage {
                    page_num: page + n_pages,
                    n_pages: size_class.n_pages - n_pages,
                };

                // Remove the size class if it's empty
                //
                // NOTE: We can't mutate `FREE_PAGES_SIZE_SORTED` while `size_class` is in use as
                // `size_class` borrows from it. It turns out references borrowing from `static
                // mut` are not properly borrow checked. Demo:
                // https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=a4aa8358ff4a7e24b20aa6f9c01d4acd
                //
                // TODO: Maybe we should stop using `static mut` and pass parameters?
                if size_class.is_empty() {
                    FREE_PAGES_SIZE_SORTED.remove(size_class_idx);
                }

                add_free_page_size_sorted(free_page.page_num, free_page.n_pages);
                FREE_PAGES_ADDR_SORTED.insert(free_page.page_num, free_page.n_pages);

                WasmPage {
                    page_num: page,
                    n_pages,
                }
            }
        }
    }
}

/// Free given page(s)
pub unsafe fn free(page: WasmPage) {
    let coalesce_left: Option<WasmPage> = FREE_PAGES_ADDR_SORTED
        .range(..page.page_num)
        .next_back()
        .map(|(page_num, n_pages)| WasmPage {
            page_num: *page_num,
            n_pages: *n_pages,
        });

    let coalesce_right: Option<WasmPage> = FREE_PAGES_ADDR_SORTED
        .range(page.page_num..)
        .next()
        .map(|(page_num, n_pages)| WasmPage {
            page_num: *page_num,
            n_pages: *n_pages,
        });

    match (coalesce_left, coalesce_right) {
        (None, None) => {
            // Coalescing not possible
            FREE_PAGES_ADDR_SORTED.insert(page.page_num, page.n_pages);
            add_free_page_size_sorted(page.page_num, page.n_pages);
        }
        (Some(coalesce_left), None) => {
            if coalesce_left.page_num + coalesce_left.n_pages == page.page_num {
                let old = FREE_PAGES_ADDR_SORTED.remove(&coalesce_left.page_num);
                debug_assert!(old.is_some());
                remove_free_page_size_sorted(coalesce_left.page_num, coalesce_left.n_pages);

                FREE_PAGES_ADDR_SORTED
                    .insert(coalesce_left.page_num, coalesce_left.n_pages + page.n_pages);
                add_free_page_size_sorted(
                    coalesce_left.page_num,
                    coalesce_left.n_pages + page.n_pages,
                );
            } else {
                // Coalescing not possible
                FREE_PAGES_ADDR_SORTED.insert(page.page_num, page.n_pages);
                add_free_page_size_sorted(page.page_num, page.n_pages);
            }
        }
        (None, Some(coalesce_right)) => {
            if page.page_num + page.n_pages == coalesce_right.page_num {
                let old = FREE_PAGES_ADDR_SORTED.remove(&coalesce_right.page_num);
                debug_assert!(old.is_some());
                remove_free_page_size_sorted(coalesce_right.page_num, coalesce_right.n_pages);

                FREE_PAGES_ADDR_SORTED.insert(page.page_num, page.n_pages + coalesce_right.n_pages);
                add_free_page_size_sorted(page.page_num, page.n_pages + coalesce_right.n_pages);
            } else {
                // Coalescing not possible
                FREE_PAGES_ADDR_SORTED.insert(page.page_num, page.n_pages);
                add_free_page_size_sorted(page.page_num, page.n_pages);
            }
        }
        (Some(coalesce_left), Some(coalesce_right)) => {
            let coalesce_left_ = coalesce_left.page_num + coalesce_left.n_pages == page.page_num;
            let coalesce_right_ = page.page_num + page.n_pages == coalesce_right.page_num;

            if coalesce_left_ {
                let old = FREE_PAGES_ADDR_SORTED.remove(&coalesce_left.page_num);
                debug_assert!(old.is_some());
                remove_free_page_size_sorted(coalesce_left.page_num, coalesce_left.n_pages);
            }

            if coalesce_right_ {
                let old = FREE_PAGES_ADDR_SORTED.remove(&coalesce_right.page_num);
                debug_assert!(old.is_some());
                remove_free_page_size_sorted(coalesce_right.page_num, coalesce_right.n_pages);
            }

            let new_page = match (coalesce_left_, coalesce_right_) {
                (true, true) => WasmPage {
                    page_num: coalesce_left.page_num,
                    n_pages: coalesce_left.n_pages + page.n_pages + coalesce_right.n_pages,
                },
                (true, false) => WasmPage {
                    page_num: coalesce_left.page_num,
                    n_pages: coalesce_left.n_pages + page.n_pages,
                },
                (false, true) => WasmPage {
                    page_num: page.page_num,
                    n_pages: page.n_pages + coalesce_right.n_pages,
                },
                (false, false) => page,
            };

            FREE_PAGES_ADDR_SORTED.insert(new_page.page_num, new_page.n_pages);
            add_free_page_size_sorted(new_page.page_num, new_page.n_pages);
        }
    }
}
