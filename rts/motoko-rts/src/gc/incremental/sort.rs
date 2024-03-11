use core::cmp::Ordering;

/// Simple in-place sort algorithm that does not allocate additional memory.
/// Based on quicksort. Average runtime costs of `O(n*log(n))`.
pub fn sort<Compare: Fn(usize, usize) -> Ordering>(array: &mut [usize], compare: &Compare) {
    if array.len() > 1 {
        quicksort(array, compare);
    }
}

/// Special version of quicksort by Niklaus Wirth: https://people.inf.ethz.ch/wirth/AD.pdf, pages 65/66.
fn quicksort<Compare: Fn(usize, usize) -> Ordering>(array: &mut [usize], compare: &Compare) {
    // Require at least two elements to avoid the unsigned integer underflow of `backward`.
    debug_assert!(array.len() > 1);
    // Take the middle element as pivot to optimize for the case of a sorted or nearly sorted array.
    let pivot = array[array.len() / 2];
    let mut forward = 0;
    let mut backward = array.len() - 1;
    loop {
        while compare(array[forward], pivot) == Ordering::Less {
            forward += 1;
        }
        while compare(array[backward], pivot) == Ordering::Greater {
            backward -= 1;
        }
        if forward <= backward {
            // Cannot use `array.swap()` since it imports `memmove` in debug build.
            let temporary = array[forward];
            array[forward] = array[backward];
            array[backward] = temporary;
            forward += 1;
            backward -= 1;
        }
        if forward > backward {
            break;
        }
    }
    // Invariant after partitioning: `forward > backward`. Therefore:
    // * The left partition ends at `backward`.
    // * The right partition starts at `forward`.
    if backward > 0 {
        quicksort(&mut array[..backward + 1], compare);
    }
    if forward < array.len() - 1 {
        quicksort(&mut array[forward..], compare);
    }
}
