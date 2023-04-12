use core::cmp::Ordering;

/// Simple in-place sort algorithm that does not allocate additional memory.
/// Based on quicksort. Average runtime costs of `O(n*log(n))`.
pub fn sort<Compare: Fn(usize, usize) -> Ordering>(array: &mut [usize], compare: &Compare) {
    if array.len() > 1 {
        quicksort(array, compare);
    }
}

fn quicksort<Compare: Fn(usize, usize) -> Ordering>(array: &mut [usize], compare: &Compare) {
    debug_assert!(array.len() > 1);
    let pivot = array[array.len() / 2];
    let mut left = 0;
    let mut right = array.len() - 1;
    loop {
        while compare(array[left], pivot) == Ordering::Less {
            left += 1;
        }
        while compare(array[right], pivot) == Ordering::Greater {
            right -= 1;
        }
        if left <= right {
            // Cannot use `array.swap()` since it imports `memmove` in debug build.
            let temporary = array[left];
            array[left] = array[right];
            array[right] = temporary;
            left += 1;
            right -= 1;
        }
        if left > right {
            break;
        }
    }
    if right > 0 {
        quicksort(&mut array[..right + 1], compare);
    }
    if left < array.len() - 1 {
        quicksort(&mut array[left..], compare);
    }
}
