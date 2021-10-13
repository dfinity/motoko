use super::{Page, PageAlloc, PageHeader};
use crate::bitmap::Bitmap;
use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;

use alloc::collections::btree_set::BTreeSet;
use alloc::vec::Vec;
use core::arch::wasm32;

/// A `PageAlloc` implementation for the IC
#[derive(Clone)]
pub struct IcPageAlloc {}

/// A `Page` implementation for the IC. Currently maps to a single Wasm page, but that may change
/// in the future.
#[derive(Debug, Clone, Copy)]
pub struct IcPage {
    /// Wasm page number for this page
    wasm_page_num: u16,

    /// Number of Wasm pages in this page
    n_pages: u16,
}

#[derive(Debug)]
struct SizeClass {
    n_pages: u16,
    // Set of Wasm pages
    pages: BTreeSet<u16>,
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
}

//
// Free list operations
//

/// Free pages sorted by start address (Wasm page num). Used to find pages to merge.
// TODO: Make this a `BTreeSet` after updating rustc (currently BTreeSet::new is not const)
static mut FREE_PAGES_ADDR_SORTED: Vec<IcPage> = Vec::new();

unsafe fn get_page_num_idx(wasm_page_num: u16) -> Result<usize, usize> {
    FREE_PAGES_ADDR_SORTED.binary_search_by_key(&wasm_page_num, |page| page.wasm_page_num)
}

unsafe fn add_free_page_addr_sorted(page: IcPage) {
    match get_page_num_idx(page.wasm_page_num) {
        Ok(_) => panic!("add_free_page_addr_sorted: page already in free list"),
        Err(idx) => FREE_PAGES_ADDR_SORTED.insert(idx, page),
    }
}

unsafe fn remove_free_page_addr_sorted(wasm_page_num: u16) {
    match get_page_num_idx(wasm_page_num) {
        Ok(idx) => {
            FREE_PAGES_ADDR_SORTED.remove(idx);
        }
        Err(_) => panic!(
            "remove_free_page_addr: page {} is not in free list",
            wasm_page_num
        ),
    }
}

/// Free pages reverse-sorted by region size and start address (or Wasm page num), used to allocate.
// TODO: Make this a `BTreeSet` after updating rustc (currently BTreeSet::new is not const)
static mut FREE_PAGES_SIZE_SORTED: Vec<SizeClass> = Vec::new();

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

unsafe fn remove_free_size_sorted(wasm_page_num: u16, n_pages: u16) {
    match get_size_class_idx(n_pages) {
        Ok(size_class_idx) => {
            FREE_PAGES_SIZE_SORTED[size_class_idx].remove(wasm_page_num);
        }
        Err(_) => panic!(
            "remove_free_size_sorted: No size class for {} pages",
            n_pages
        ),
    }
}

//
// End of free list operations
//

unsafe fn alloc_wasm_pages(n_pages: u16) -> u16 {
    let wasm_page_num = wasm32::memory_grow(0, usize::from(n_pages));

    if wasm_page_num == usize::MAX {
        rts_trap_with("Cannot grow memory");
    }

    wasm_page_num as u16
}

impl PageAlloc for IcPageAlloc {
    type Page = IcPage;

    unsafe fn alloc(&self) -> IcPage {
        // Get the smallest page
        match FREE_PAGES_SIZE_SORTED.get_mut(0) {
            None => {
                let wasm_page_num = alloc_wasm_pages(1);
                IcPage {
                    wasm_page_num,
                    n_pages: 1,
                }
            }
            Some(size_class) => {
                // TODO: Improve error msg
                let page = size_class.remove_first().unwrap();

                // Page removed from size-sorted list, remove from addr-sorted list
                remove_free_page_addr_sorted(page);

                if size_class.n_pages > 1 {
                    // Page too big: split it, add the unused part to free lists
                    let free_page = IcPage {
                        wasm_page_num: page + 1,
                        n_pages: size_class.n_pages - 1,
                    };

                    add_free_page_size_sorted(free_page.wasm_page_num, free_page.n_pages);
                    add_free_page_addr_sorted(free_page);
                }

                IcPage {
                    wasm_page_num: page,
                    n_pages: 1,
                }
            }
        }
    }

    unsafe fn alloc_pages(&self, n_pages: u16) -> IcPage {
        // Get the size class with n_pages or the smallest size class larger than n_pages
        match get_size_class_idx(n_pages) {
            Ok(size_class_idx) => {
                // TODO: Improve err msg
                let wasm_page_num = FREE_PAGES_SIZE_SORTED[size_class_idx]
                    .remove_first()
                    .unwrap();

                let page = IcPage {
                    wasm_page_num,
                    n_pages,
                };

                remove_free_page_addr_sorted(wasm_page_num);

                page
            }
            Err(size_class_idx) => {
                if size_class_idx == FREE_PAGES_SIZE_SORTED.len() {
                    // No size class available for `n_pages`, allocate Wasm pages
                    let wasm_page_num = alloc_wasm_pages(n_pages);

                    IcPage {
                        wasm_page_num,
                        n_pages,
                    }
                } else {
                    // We have a size class larger than requested. Get a page, free unused parts.
                    let size_class = &mut FREE_PAGES_SIZE_SORTED[size_class_idx];

                    // TODO: Improve err msg
                    let page = size_class.remove_first().unwrap();

                    remove_free_page_addr_sorted(page);

                    let free_page = IcPage {
                        wasm_page_num: page + n_pages,
                        n_pages: size_class.n_pages - n_pages,
                    };

                    add_free_page_size_sorted(free_page.wasm_page_num, free_page.n_pages);
                    add_free_page_addr_sorted(free_page);

                    IcPage {
                        wasm_page_num: page,
                        n_pages,
                    }
                }
            }
        }
    }

    unsafe fn free(&self, page: IcPage) {
        // Check addr-sorted list first, coalesce with neighbors
        let free_list_idx = match get_page_num_idx(page.wasm_page_num) {
            Ok(_) => panic!(
                "free: Wasm page {} is already in a free list",
                page.wasm_page_num
            ),
            Err(idx) => idx,
        };

        let mut coalesce_start = free_list_idx;
        let mut coalesce_end = free_list_idx;

        while coalesce_start != 0 {
            let prev_page = FREE_PAGES_ADDR_SORTED[coalesce_start - 1];
            if prev_page.wasm_page_num + prev_page.n_pages == page.wasm_page_num {
                coalesce_start -= 1;
            } else {
                break;
            }
        }

        while coalesce_end != FREE_PAGES_ADDR_SORTED.len() {
            let next_page = FREE_PAGES_ADDR_SORTED[coalesce_end];
            if next_page.wasm_page_num == page.wasm_page_num + page.n_pages {
                coalesce_end += 1;
            }
            break;
        }

        if coalesce_start == coalesce_end {
            // Coalescing not possible
            add_free_page_addr_sorted(page);
            add_free_page_size_sorted(page.wasm_page_num, page.n_pages);
        } else {
            // Remove coalesced pages from free lists
            let mut total_pages = page.n_pages;
            let coalesced_wasm_page_num = FREE_PAGES_ADDR_SORTED[coalesce_start].wasm_page_num;

            let coalesced_pages: Vec<IcPage> = FREE_PAGES_ADDR_SORTED
                .drain(coalesce_start..coalesce_end)
                .collect();

            for coalesced_page in &coalesced_pages {
                // Page already removed from addr-sorted list above
                remove_free_size_sorted(coalesced_page.wasm_page_num, coalesced_page.n_pages);
                total_pages += coalesced_page.n_pages;
            }

            // Insert new page
            let new_page = IcPage {
                wasm_page_num: coalesced_wasm_page_num,
                n_pages: total_pages,
            };

            add_free_page_addr_sorted(new_page);
            add_free_page_size_sorted(coalesced_wasm_page_num, total_pages);
        }
    }

    unsafe fn get_address_page_start(&self, addr: usize) -> usize {
        // TODO: Implement debug mode checks:
        // - `addr` is not in a "large" page
        // - `addr` is within heap

        // Mask least significant bits to get Wasm page start
        addr & !(WASM_PAGE_SIZE.as_usize() - 1)
    }
}

impl Page for IcPage {
    unsafe fn start(&self) -> usize {
        usize::from(self.wasm_page_num) * WASM_PAGE_SIZE.as_usize()
    }

    unsafe fn contents_start(&self) -> usize {
        (self.start() as *const PageHeader).add(1) as usize
    }

    unsafe fn end(&self) -> usize {
        (usize::from(self.wasm_page_num) + 1) * WASM_PAGE_SIZE.as_usize()
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
