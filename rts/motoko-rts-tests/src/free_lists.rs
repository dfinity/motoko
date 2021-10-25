use motoko_rts::page_alloc::free_lists::*;

pub unsafe fn test() {
    println!("Testing free list functions ...");

    simple();
    simple_coalesce();
}

unsafe fn simple() {
    clear();
    let page = alloc(|n_pages| 1);
    free(page);

    assert_eq!(
        FREE_PAGES_ADDR_SORTED,
        vec![WasmPage {
            page_num: 1,
            n_pages: 1,
        }]
    );

    assert_eq!(
        FREE_PAGES_SIZE_SORTED,
        vec![SizeClass {
            n_pages: 1,
            pages: [1u16].iter().copied().collect()
        }]
    );
}

unsafe fn simple_coalesce() {
    clear();
    let page1 = alloc(|n_pages| 1);
    let page2 = alloc(|n_pages| 2);
    free(page1);
    free(page2);

    assert_eq!(
        FREE_PAGES_ADDR_SORTED,
        vec![WasmPage {
            page_num: 1,
            n_pages: 2,
        }]
    );

    assert_eq!(
        FREE_PAGES_SIZE_SORTED,
        vec![SizeClass {
            n_pages: 2,
            pages: [1u16].iter().copied().collect()
        }]
    );
}
