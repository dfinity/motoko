use motoko_rts::bitmap::Bitmap;
use motoko_rts::page_alloc::{Page, PageAlloc, PageHeader};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Clone)]
pub struct TestPageAlloc {
    inner: Rc<RefCell<TestPageAllocInner>>,
}

struct TestPageAllocInner {
    /// Size of a page, including headers.
    page_size_bytes: usize,

    // TODO: Maybe use a vector with free slots, for lookup efficiency?
    pages: HashMap<TestPageRef, TestPage>,

    /// Start addresses of currently in-use pages. Used to implement `get_address_page`.
    // TODO: None of the binary search trees in Rust's std provide methods for finding previous
    // element of a given one, so using a sorted `Vec` which provides a binary search method.
    page_addrs: Vec<(usize, TestPageRef)>,

    /// Total pages allocated so far. We don't reuse page refs to catch use-after-free issues.
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
    pub fn new(page_size_bytes: usize) -> TestPageAlloc {
        // Should have enough space in a page for the header + more (TODO)
        assert!(page_size_bytes > std::mem::size_of::<PageHeader>());
        TestPageAlloc {
            inner: Rc::new(RefCell::new(TestPageAllocInner::new(page_size_bytes))),
        }
    }
}

impl TestPageAllocInner {
    fn new(page_size_bytes: usize) -> TestPageAllocInner {
        TestPageAllocInner {
            page_size_bytes,
            pages: HashMap::new(),
            page_addrs: vec![],
            n_total_pages: 0,
        }
    }
}

impl PageAlloc for TestPageAlloc {
    type Page = TestPageRef;

    unsafe fn alloc(&self) -> Self::Page {
        let alloc_ref = self.clone();
        self.inner.borrow_mut().alloc(alloc_ref)
    }

    unsafe fn free(&self, page: Self::Page) {
        self.inner.borrow_mut().free(page)
    }

    unsafe fn get_address_page(&self, addr: usize) -> Self::Page {
        self.inner.borrow().get_address_page(addr)
    }
}

impl TestPageAllocInner {
    unsafe fn alloc(&mut self, page_alloc: TestPageAlloc) -> TestPageRef {
        let page = TestPage {
            contents: vec![0u8; self.page_size_bytes].into_boxed_slice(),
        };

        let page_start = page.contents_start();

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
        let page = self.pages.remove(&page).unwrap();
        let page_start = page.contents_start();
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

    unsafe fn get_address_page(&self, addr: usize) -> TestPageRef {
        let page_ref_idx = match self.page_addrs.binary_search_by_key(&addr, |(k, _)| *k) {
            Ok(idx) => idx,
            Err(0) => panic!("Page start address not in page_addrs"),
            Err(idx) => idx - 1,
        };

        let page_ref = self.page_addrs[page_ref_idx].1.clone();

        if addr > page_ref.end() {
            panic!("Page address not in allocated pages");
        }

        page_ref
    }
}

impl Page for TestPageRef {
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
        self.start() + page_alloc.inner.borrow().page_size_bytes
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
