use super::{Bitmap, Page, PageHeader, WASM_PAGE_SIZE};

use alloc::collections::btree_map::Entry;
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
#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug)]
pub struct FreeLists {
    addr_sorted: BTreeMap<u16, u16>,
    size_sorted: BTreeMap<u16, SizeClass>,
}

impl FreeLists {
    pub const fn new() -> FreeLists {
        FreeLists {
            addr_sorted: BTreeMap::new(),
            size_sorted: BTreeMap::new(),
        }
    }

    fn add_free_page_size_sorted(&mut self, wasm_page_num: u16, n_pages: u16) {
        match self.size_sorted.entry(n_pages) {
            Entry::Vacant(entry) => entry.insert(SizeClass::new(n_pages)).insert(wasm_page_num),
            Entry::Occupied(mut entry) => entry.get_mut().insert(wasm_page_num),
        }
    }

    fn remove_free_page_size_sorted(&mut self, wasm_page_num: u16, n_pages: u16) {
        let size_class = self.size_sorted.get_mut(&n_pages).unwrap_or_else(|| {
            panic!(
                "remove_free_size_sorted: No size class for {} pages",
                n_pages
            )
        });

        size_class.remove(wasm_page_num);

        if size_class.is_empty() {
            self.size_sorted.remove(&n_pages);
        }
    }

    /// Allocate single page
    pub fn alloc<GrowMemory>(&mut self, grow_memory: GrowMemory) -> WasmPage
    where
        GrowMemory: FnMut(u16) -> u16, // Wasm memory.grow
    {
        // TODO: For single page allocs we always use the first free list, not need to binary search
        self.alloc_pages(grow_memory, 1)
    }

    /// Allocate multiple pages
    pub fn alloc_pages<GrowMemory>(&mut self, mut grow_memory: GrowMemory, n_pages: u16) -> WasmPage
    where
        GrowMemory: FnMut(u16) -> u16, // Wasm memory.grow
    {
        // Get the size class with n_pages or the smallest size class larger than n_pages
        match self.size_sorted.range_mut(n_pages..).next() {
            Some((n_pages_, size_class)) => {
                let n_pages_ = *n_pages_;

                if n_pages_ > n_pages {
                    // We have a size class larger than requested. Get a page, free unused parts.

                    // TODO: Improve err msg
                    let page = size_class.remove_first().unwrap();

                    let old = self.addr_sorted.remove(&page);
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
                        self.size_sorted.remove(&n_pages_);
                    }

                    self.add_free_page_size_sorted(free_page.page_num, free_page.n_pages);
                    self.addr_sorted
                        .insert(free_page.page_num, free_page.n_pages);

                    WasmPage {
                        page_num: page,
                        n_pages,
                    }
                } else {
                    debug_assert_eq!(n_pages_, n_pages);

                    // TODO: Improve err msg
                    let page_num = size_class.remove_first().unwrap();

                    if size_class.is_empty() {
                        self.size_sorted.remove(&n_pages_);
                    }

                    let page = WasmPage { page_num, n_pages };

                    let old = self.addr_sorted.remove(&page_num);
                    debug_assert!(old.is_some());

                    page
                }
            }
            None => {
                // No size class available for `n_pages`, allocate Wasm pages
                let page_num = grow_memory(n_pages);
                WasmPage { page_num, n_pages }
            }
        }
    }

    /// Free given page(s)
    pub fn free(&mut self, page: WasmPage) {
        let coalesce_left: Option<WasmPage> = self
            .addr_sorted
            .range(..page.page_num)
            .next_back()
            .map(|(page_num, n_pages)| WasmPage {
                page_num: *page_num,
                n_pages: *n_pages,
            });

        let coalesce_right: Option<WasmPage> =
            self.addr_sorted
                .range(page.page_num..)
                .next()
                .map(|(page_num, n_pages)| WasmPage {
                    page_num: *page_num,
                    n_pages: *n_pages,
                });

        match (coalesce_left, coalesce_right) {
            (None, None) => {
                // Coalescing not possible
                self.addr_sorted.insert(page.page_num, page.n_pages);
                self.add_free_page_size_sorted(page.page_num, page.n_pages);
            }
            (Some(coalesce_left), None) => {
                if coalesce_left.page_num + coalesce_left.n_pages == page.page_num {
                    let old = self.addr_sorted.remove(&coalesce_left.page_num);
                    debug_assert!(old.is_some());
                    self.remove_free_page_size_sorted(
                        coalesce_left.page_num,
                        coalesce_left.n_pages,
                    );

                    self.addr_sorted
                        .insert(coalesce_left.page_num, coalesce_left.n_pages + page.n_pages);
                    self.add_free_page_size_sorted(
                        coalesce_left.page_num,
                        coalesce_left.n_pages + page.n_pages,
                    );
                } else {
                    // Coalescing not possible
                    self.addr_sorted.insert(page.page_num, page.n_pages);
                    self.add_free_page_size_sorted(page.page_num, page.n_pages);
                }
            }
            (None, Some(coalesce_right)) => {
                if page.page_num + page.n_pages == coalesce_right.page_num {
                    let old = self.addr_sorted.remove(&coalesce_right.page_num);
                    debug_assert!(old.is_some());
                    self.remove_free_page_size_sorted(
                        coalesce_right.page_num,
                        coalesce_right.n_pages,
                    );

                    self.addr_sorted
                        .insert(page.page_num, page.n_pages + coalesce_right.n_pages);
                    self.add_free_page_size_sorted(
                        page.page_num,
                        page.n_pages + coalesce_right.n_pages,
                    );
                } else {
                    // Coalescing not possible
                    self.addr_sorted.insert(page.page_num, page.n_pages);
                    self.add_free_page_size_sorted(page.page_num, page.n_pages);
                }
            }
            (Some(coalesce_left), Some(coalesce_right)) => {
                let coalesce_left_ =
                    coalesce_left.page_num + coalesce_left.n_pages == page.page_num;
                let coalesce_right_ = page.page_num + page.n_pages == coalesce_right.page_num;

                if coalesce_left_ {
                    let old = self.addr_sorted.remove(&coalesce_left.page_num);
                    debug_assert!(old.is_some());
                    self.remove_free_page_size_sorted(
                        coalesce_left.page_num,
                        coalesce_left.n_pages,
                    );
                }

                if coalesce_right_ {
                    let old = self.addr_sorted.remove(&coalesce_right.page_num);
                    debug_assert!(old.is_some());
                    self.remove_free_page_size_sorted(
                        coalesce_right.page_num,
                        coalesce_right.n_pages,
                    );
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

                self.addr_sorted.insert(new_page.page_num, new_page.n_pages);
                self.add_free_page_size_sorted(new_page.page_num, new_page.n_pages);
            }
        }
    }

    /// For testing
    pub fn free_pages_addr_sorted(&self) -> Vec<WasmPage> {
        self.addr_sorted
            .iter()
            .map(|(page_num, n_pages)| WasmPage {
                page_num: *page_num,
                n_pages: *n_pages,
            })
            .collect()
    }

    /// For testing
    pub fn free_pages_size_sorted(&self) -> Vec<SizeClass> {
        self.size_sorted
            .iter()
            .map(|(_, size_class)| size_class.clone())
            .collect()
    }
}
