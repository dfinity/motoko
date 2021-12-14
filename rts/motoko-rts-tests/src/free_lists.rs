mod random;

use motoko_rts::page_alloc::free_lists::{FreeLists, SizeClass, WasmPage};

pub unsafe fn test() {
    println!("Testing free list functions ...");

    simple();
    coalesce_left();
    coalesce_right();
    allocate_and_coalesce_multiple_pages();
    split_large_pages();

    println!("  Testing random free list operations...");

    let max_seed = 1000;
    for seed in 0..max_seed {
        print!("\r{}/{}", seed + 1, max_seed);
        std::io::Write::flush(&mut std::io::stdout()).unwrap();
        random::random_free_list_ops(seed, 1000);
    }
    print!("\r");
}

unsafe fn simple() {
    let mut free_lists = FreeLists::new();

    let page = free_lists.alloc(|n_pages| {
        assert_eq!(n_pages, 1);
        1
    });
    free_lists.free(page);

    assert_eq!(
        free_lists.free_pages_addr_sorted(),
        vec![WasmPage {
            page_num: 1,
            n_pages: 1,
        }]
    );

    assert_eq!(
        free_lists.free_pages_size_sorted(),
        vec![SizeClass {
            n_pages: 1,
            pages: [1u16].iter().copied().collect()
        }]
    );
}

unsafe fn coalesce_left() {
    let mut free_lists = FreeLists::new();

    let page1 = free_lists.alloc(|n_pages| {
        assert_eq!(n_pages, 1);
        1
    });

    let page2 = free_lists.alloc(|n_pages| {
        assert_eq!(n_pages, 1);
        2
    });

    free_lists.free(page2);
    free_lists.free(page1);

    assert_eq!(
        free_lists.free_pages_addr_sorted(),
        vec![WasmPage {
            page_num: 1,
            n_pages: 2,
        }]
    );

    assert_eq!(
        free_lists.free_pages_size_sorted(),
        vec![SizeClass {
            n_pages: 2,
            pages: [1u16].iter().copied().collect()
        }]
    );
}

unsafe fn coalesce_right() {
    let mut free_lists = FreeLists::new();

    let page1 = free_lists.alloc(|n_pages| {
        assert_eq!(n_pages, 1);
        1
    });

    let page2 = free_lists.alloc(|n_pages| {
        assert_eq!(n_pages, 1);
        2
    });

    free_lists.free(page1);

    assert_eq!(
        free_lists.free_pages_addr_sorted(),
        vec![WasmPage {
            page_num: 1,
            n_pages: 1,
        }]
    );

    assert_eq!(
        free_lists.free_pages_size_sorted(),
        vec![SizeClass {
            n_pages: 1,
            pages: [1u16].iter().copied().collect()
        }]
    );

    free_lists.free(page2);

    assert_eq!(
        free_lists.free_pages_addr_sorted(),
        vec![WasmPage {
            page_num: 1,
            n_pages: 2,
        }]
    );

    assert_eq!(
        free_lists.free_pages_size_sorted(),
        vec![SizeClass {
            n_pages: 2,
            pages: [1u16].iter().copied().collect()
        }]
    );
}

unsafe fn allocate_and_coalesce_multiple_pages() {
    let mut free_lists = FreeLists::new();

    let page1 = free_lists.alloc_pages(
        |n_pages| {
            assert_eq!(n_pages, 5);
            0
        },
        5,
    );

    free_lists.free(page1);

    assert_eq!(
        free_lists.free_pages_size_sorted(),
        vec![SizeClass {
            n_pages: 5,
            pages: [0].iter().copied().collect()
        }]
    );

    assert_eq!(
        free_lists.free_pages_addr_sorted(),
        vec![WasmPage {
            page_num: 0,
            n_pages: 5
        }]
    );

    // Second time we allocate same number of pages the old page should be returned
    let page2 = free_lists.alloc_pages(
        |n_pages| panic!("memory.grow called, n_pages={}", n_pages),
        5,
    );

    assert_eq!(page1, page2);

    assert_eq!(free_lists.free_pages_size_sorted(), vec![]);
    assert_eq!(free_lists.free_pages_addr_sorted(), vec![]);

    let page3 = free_lists.alloc(|_| 5);
    let page4 = free_lists.alloc(|_| 6);

    free_lists.free(page3);

    assert_eq!(
        free_lists.free_pages_size_sorted(),
        vec![SizeClass {
            n_pages: 1,
            pages: [5].iter().copied().collect()
        }]
    );

    assert_eq!(
        free_lists.free_pages_addr_sorted(),
        vec![WasmPage {
            page_num: 5,
            n_pages: 1
        }]
    );

    free_lists.free(page4);

    assert_eq!(
        free_lists.free_pages_size_sorted(),
        vec![SizeClass {
            n_pages: 2,
            pages: [5].iter().copied().collect()
        }]
    );

    assert_eq!(
        free_lists.free_pages_addr_sorted(),
        vec![WasmPage {
            page_num: 5,
            n_pages: 2
        }]
    );

    free_lists.free(page2);

    assert_eq!(
        free_lists.free_pages_size_sorted(),
        vec![SizeClass {
            n_pages: 7,
            pages: [0].iter().copied().collect()
        }]
    );

    assert_eq!(
        free_lists.free_pages_addr_sorted(),
        vec![WasmPage {
            page_num: 0,
            n_pages: 7
        }]
    );
}

unsafe fn split_large_pages() {
    let mut free_lists = FreeLists::new();

    // Populate free lists
    let page = free_lists.alloc_pages(|_| 0, 10);
    free_lists.free(page);

    // Allocate 10 small pages without calling memory.grow
    let mut pages = vec![];
    for page_num in 0..10 {
        let page = free_lists.alloc(|_| panic!("memory.grow called"));
        assert_eq!(
            page,
            WasmPage {
                page_num,
                n_pages: 1
            }
        );
        pages.push(page);
    }

    // Free small pages in some random order
    free_lists.free(pages[3]);
    free_lists.free(pages[4]);
    free_lists.free(pages[1]);
    free_lists.free(pages[2]);
    free_lists.free(pages[5]);
    free_lists.free(pages[9]);
    free_lists.free(pages[8]);
    free_lists.free(pages[7]);
    free_lists.free(pages[6]);
    free_lists.free(pages[0]);

    assert_eq!(
        free_lists.free_pages_size_sorted(),
        vec![SizeClass {
            n_pages: 10,
            pages: [0].iter().copied().collect()
        }]
    );

    assert_eq!(
        free_lists.free_pages_addr_sorted(),
        vec![WasmPage {
            page_num: 0,
            n_pages: 10
        }]
    );
}
