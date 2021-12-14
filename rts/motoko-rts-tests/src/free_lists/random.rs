use motoko_rts::page_alloc::free_lists::{FreeLists, SizeClass, WasmPage};

use oorandom::Rand32;

fn rand_bool(rng: &mut Rand32) -> bool {
    rng.rand_range(0..2) == 1
}

pub(super) unsafe fn random_free_list_ops(seed: u64, num_ops: u32) {
    let mut rng = Rand32::new(seed);

    let mut free_lists = FreeLists::new();

    // Number of Wasm pages allocated so far
    let mut n_wasm_pages = 0;

    // In-use pages
    let mut pages: Vec<WasmPage> = vec![];

    for _ in 0..num_ops {
        // If we have pages to free then consider freeing some of them. If not, allocate new pages
        if !pages.is_empty() && rand_bool(&mut rng) {
            // Index of the page to remove
            let page_idx = rng.rand_range(0..pages.len() as u32) as usize;
            let page = pages.remove(page_idx);
            free_lists.free(page);
        } else {
            // How many pages to allocate
            let n_pages = rng.rand_range(1..101) as u16;
            let page = free_lists.alloc_pages(
                |n_pages| {
                    let ret = n_wasm_pages;
                    n_wasm_pages += n_pages;
                    ret
                },
                n_pages,
            );
            pages.push(page);
        }
    }

    // Release all of the in-use pages
    for page in pages.into_iter() {
        free_lists.free(page);
    }

    // Free lists should have one entry
    assert_eq!(
        free_lists.free_pages_size_sorted(),
        vec![SizeClass {
            n_pages: n_wasm_pages,
            pages: [0].iter().copied().collect()
        }]
    );

    assert_eq!(
        free_lists.free_pages_addr_sorted(),
        vec![WasmPage {
            page_num: 0,
            n_pages: n_wasm_pages
        }]
    );
}
