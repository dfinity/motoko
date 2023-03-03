use crate::{
    gc::common::{Limits, Roots},
    memory::Memory,
};

pub struct YoungCollection<'a, M: Memory> {
    mem: &'a mut M,
    limits: Limits,
    roots: Roots,
}

impl<'a, M: Memory> YoungCollection<'a, M> {
    pub fn new(mem: &'a mut M, limits: Limits, roots: Roots) -> YoungCollection<'a, M> {
        YoungCollection { mem, limits, roots }
    }

    pub fn limits(&self) -> &Limits {
        &self.limits
    }

    pub unsafe fn run(&mut self) {
        println!(100, "INCREMENTAL GC");
    }
}
