//! Implements non-allocating printing utilities. All allocations are done on stack.

// Debug functions are not always used
#![allow(unused)]

use core::fmt;

extern "C" {
    // `print_ptr` is implemented by the code generator and has different implementations depending
    // on the target platform (WASI or IC).
    fn print_ptr(ptr: usize, len: u32);
}

/*
NB (osa): Implementation below uses const_generics and non-inlined function, which is probably
better for code size, but generates this warning:
    warning: the feature `const_generics` is incomplete and may not be safe to use and/or cause
    compiler crashes
which looks scary.
---
use crate::common::WriteBuf;
#[macro_export]
macro_rules! println {
    ($buf_size:tt, $($arg:tt)*) => ({
        $crate::print::print::<$buf_size>(::core::format_args_nl!($($arg)*));
    })
}
pub(crate) fn print<const BUF_SIZE: usize>(args: fmt::Arguments<'_>) {
    let mut buf = [0 as u8; BUF_SIZE];
    let mut fmt = WriteBuf::new(&mut buf);
    fmt.write_fmt(args).unwrap();
    fmt.print();
}
*/

#[macro_export]
macro_rules! println {
    ($buf_size:tt, $($arg:tt)*) => ({
        {
            use core::fmt::Write;
            let mut buf = [0 as u8; $buf_size];
            let mut fmt = $crate::print::WriteBuf::new(&mut buf);
            let _ = write!(&mut fmt, $($arg)*);
            $crate::print::print(&fmt);
        }
    })
}

#[macro_export]
macro_rules! format {
    ($buf_size:tt, $($arg:tt)*) => ({
        {
            use core::fmt::Write;
            let mut buf = [0 as u8; $buf_size];
            let mut fmt = $crate::print::WriteBuf::new(&mut buf);
            let _ = write!(&mut fmt, $($arg)*);
            buf
        }
    })
}

/// A buffer that implements `core::fmt::Write`. `Write` methods will write to the buffer until
/// it's filled and then ignore the rest, without failing.
pub(crate) struct WriteBuf<'a> {
    buf: &'a mut [u8],
    offset: usize,
}

impl<'a> WriteBuf<'a> {
    pub(crate) fn new(buf: &'a mut [u8]) -> Self {
        Self {
            buf: buf,
            offset: 0,
        }
    }

    pub(crate) fn reset(&mut self) {
        self.offset = 0;
    }

    pub(crate) unsafe fn print(&self) {
        print_ptr(self.buf.as_ptr() as usize, self.offset as u32)
    }
}

impl<'a> fmt::Write for WriteBuf<'a> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        // Amount of space left in the write buffer
        let buf_space_left = self.buf.len() - self.offset;
        let buf = &mut self.buf[self.offset..];
        // Copy the bytes to the buffer. Note that the buffer and the copied slice have to have the
        // same length otherwise `copy_from_slice` panics.
        let bytes = s.as_bytes();
        let copy_len = core::cmp::min(buf_space_left, bytes.len());
        (&mut buf[..copy_len]).copy_from_slice(&bytes[..copy_len]);
        // Update offset
        self.offset += copy_len;
        Ok(())
    }
}

pub(crate) unsafe fn print(buf: &WriteBuf) {
    buf.print()
}

// pub(crate) unsafe fn print_str(str: &str) {
//     print_ptr(str.as_ptr() as usize, str.len() as u32)
// }
