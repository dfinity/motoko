use core::cmp::Ordering;

/// Simple in-place sort algorithm that does not allocate additional memory.
/// Based on quicksort. Average runtime costs of `O(n*log(n))`.
pub unsafe fn sort<Compare: Fn(usize, usize) -> Ordering>(
    array: *mut usize,
    length: usize,
    compare: &Compare,
) {
    if length > 1 {
        quicksort(array, 0, length - 1, compare);
    }
}

/// Special version of quicksort by Niklaus Wirth: https://people.inf.ethz.ch/wirth/AD.pdf, pages 65/66.
/// Sorting `array[start..end+1]`, i.e. `start` and `end` are inclusive indices.
unsafe fn quicksort<Compare: Fn(usize, usize) -> Ordering>(
    array: *mut usize,
    start: usize,
    end: usize,
    compare: &Compare,
) {
    // Require at least two elements to avoid the unsigned integer underflow of `backward`.
    debug_assert!(start < end);
    // Take the middle element as pivot to optimize for the case of a sorted or nearly sorted array.
    let pivot = *array.add((start + end + 1) / 2);
    let mut forward = start;
    let mut backward = end;
    loop {
        while compare(*array.add(forward), pivot) == Ordering::Less {
            forward += 1;
        }
        while compare(*array.add(backward), pivot) == Ordering::Greater {
            backward -= 1;
        }
        if forward <= backward {
            // Cannot use `array.swap()` since it imports `memmove` in debug build.
            let temporary = *array.add(forward);
            *array.add(forward) = *array.add(backward);
            *array.add(backward) = temporary;
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
    if backward > start {
        quicksort(array, start, backward, compare);
    }
    if forward < end {
        quicksort(array, forward, end, compare);
    }
}
