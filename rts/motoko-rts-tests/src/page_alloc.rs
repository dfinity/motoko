use motoko_rts::bitmap::Bitmap;
use motoko_rts::page_alloc::{LargePageHeader, Page, PageAlloc, PageHeader, PAGE_SIZE};

use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

#[derive(Clone)]
pub struct TestPageAlloc {
    inner: Rc<RefCell<TestPageAllocInner>>,
}

struct TestPageAllocInner {
    // TODO: Maybe use a vector with free slots, for lookup efficiency?
    pages: HashMap<TestPageRef, TestPage>,

    /// Start addresses of currently in-use pages. Used to implement `get_address_page` and
    /// `free_large`.
    // TODO: None of the binary search trees in Rust's std provide methods for finding previous
    // element of a given one, so using a sorted `Vec` which provides a binary search method.
    page_addrs: Vec<(usize, TestPageRef)>,

    /// Total pages allocated so far. Used to generate `TestPageRef`s. We don't reuse page refs to
    /// catch use-after-free issues.
    n_total_pages: usize,
}

#[derive(Clone)]
pub struct TestPageRef {
    page_idx: usize,
    page_alloc: TestPageAlloc,
}

impl PartialEq for TestPageRef {
    fn eq(&self, other: &Self) -> bool {
        self.page_idx.eq(&other.page_idx)
    }
}

impl Eq for TestPageRef {}

impl std::hash::Hash for TestPageRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.page_idx.hash(state)
    }
}

struct TestPage {
    contents: Box<[u8]>,
}

impl TestPageAlloc {
    pub fn new() -> TestPageAlloc {
        // Should have enough space in a page for the header + more (TODO)
        TestPageAlloc {
            inner: Rc::new(RefCell::new(TestPageAllocInner::new())),
        }
    }
}

impl TestPageAllocInner {
    fn new() -> TestPageAllocInner {
        TestPageAllocInner {
            pages: HashMap::new(),
            page_addrs: vec![],
            n_total_pages: 0,
        }
    }
}

impl PageAlloc for TestPageAlloc {
    type Page = TestPageRef;

    unsafe fn alloc(&self) -> Self::Page {
        let self_clone = self.clone();
        self.inner.borrow_mut().alloc(1, self_clone)
    }

    unsafe fn alloc_pages(&self, n_pages: u16) -> Self::Page {
        let self_clone = self.clone();
        self.inner.borrow_mut().alloc(n_pages, self_clone)
    }

    unsafe fn free(&self, page: Self::Page) {
        self.inner.borrow_mut().free(page)
    }

    unsafe fn free_large(&self, header: *const LargePageHeader) {
        self.inner.borrow_mut().free_large(header)
    }

    unsafe fn get_address_page_start(&self, addr: usize) -> usize {
        self.inner.borrow().get_address_page_start(addr)
    }
}

impl TestPageAllocInner {
    unsafe fn alloc(&mut self, n_pages: u16, page_alloc: TestPageAlloc) -> TestPageRef {
        let page = TestPage {
            contents: vec![0u8; PAGE_SIZE.as_usize() * usize::from(n_pages)].into_boxed_slice(),
        };

        let page_start = page.start();

        let page_idx = self.n_total_pages;
        self.n_total_pages += 1;

        let page_ref = TestPageRef {
            page_idx,
            page_alloc,
        };
        self.pages.insert(page_ref.clone(), page);

        match self
            .page_addrs
            .binary_search_by_key(&page_start, |(k, _)| *k)
        {
            Ok(_) => panic!("Page start address already in page_addrs"),
            Err(idx) => self.page_addrs.insert(idx, (page_start, page_ref.clone())),
        }

        page_ref
    }

    unsafe fn free(&mut self, page: TestPageRef) {
        let mut page = self.pages.remove(&page).unwrap();
        page.contents.fill(0);
        let page_start = page.start();
        match self
            .page_addrs
            .binary_search_by_key(&page_start, |(k, _)| *k)
        {
            Ok(idx) => {
                self.page_addrs.remove(idx);
            }
            Err(_) => panic!("Page start address not in page_addrs"),
        }
    }

    unsafe fn free_large(&mut self, header: *const LargePageHeader) {
        let page_start = header as usize;
        let page_idx = match self
            .page_addrs
            .binary_search_by_key(&page_start, |(k, _)| *k)
        {
            Ok(page_idx) => page_idx,
            Err(_) => panic!(
                "TestPageAlloc::free_large: page {:#x} is not in page_addrs, page_addrs={:?}",
                page_start,
                self.page_addrs
                    .iter()
                    .map(|(page_start_addr, _)| format!("{:#x}", page_start_addr))
                    .collect::<Vec<_>>(),
            ),
        };

        self.free(self.page_addrs[page_idx].1.clone());
    }

    unsafe fn get_address_page_start(&self, addr: usize) -> usize {
        let page_ref_idx = match self.page_addrs.binary_search_by_key(&addr, |(k, _)| *k) {
            Ok(idx) => idx,
            Err(0) => panic!("Page start address not in page_addrs"),
            Err(idx) => idx - 1,
        };

        let page_ref = self.page_addrs[page_ref_idx].1.clone();

        if addr > page_ref.end() {
            panic!("Page address not in allocated pages");
        }

        page_ref.start()
    }
}

impl Page for TestPageRef {
    fn page_idx(&self) -> u16 {
        u16::try_from(self.page_idx).unwrap()
    }

    unsafe fn start(&self) -> usize {
        self.page_alloc
            .inner
            .borrow()
            .pages
            .get(self)
            .expect("Page::start called on a freed page")
            .start()
    }

    unsafe fn contents_start(&self) -> usize {
        self.page_alloc
            .inner
            .borrow()
            .pages
            .get(self)
            .expect("Page::contents_start called on a freed page")
            .contents_start()
    }

    unsafe fn end(&self) -> usize {
        self.page_alloc
            .inner
            .borrow()
            .pages
            .get(self)
            .expect("Page::end called on a freed page")
            .end(&self.page_alloc)
    }

    unsafe fn get_bitmap(&self) -> Option<*mut Bitmap> {
        self.page_alloc
            .inner
            .borrow()
            .pages
            .get(self)
            .expect("Page::get_bitmap called on a freed page")
            .get_bitmap()
    }

    unsafe fn set_bitmap(&self, bitmap: Option<Bitmap>) {
        self.page_alloc
            .inner
            .borrow()
            .pages
            .get(self)
            .expect("Page::set_bitmap called on a freed page")
            .set_bitmap(bitmap)
    }

    unsafe fn take_bitmap(&self) -> Option<Bitmap> {
        self.page_alloc
            .inner
            .borrow()
            .pages
            .get(self)
            .expect("Page::take_bitmap called on a freed page")
            .take_bitmap()
    }
}

impl TestPage {
    unsafe fn start(&self) -> usize {
        self.contents.as_ptr() as usize
    }

    unsafe fn contents_start(&self) -> usize {
        (self.contents.as_ptr() as *const PageHeader).add(1) as usize
    }

    unsafe fn end(&self, page_alloc: &TestPageAlloc) -> usize {
        self.start() + PAGE_SIZE.as_usize()
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
