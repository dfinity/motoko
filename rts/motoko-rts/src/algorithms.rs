use core::cmp::PartialOrd;

pub trait SortedArray<T: PartialOrd> {
    /// Length of the array
    fn get_length(&self) -> usize;

    /// Element at index < length.
    fn value_at(&self, index: usize) -> T;

    // Binary search of the index matching the searched element.
    fn index_of(&self, searched: T) -> Option<usize> {
        let mut left = 0;
        let mut right = self.get_length();
        while left < right {
            let middle = (left + right) / 2;
            let item = self.value_at(middle);
            debug_assert!(self.value_at(left) <= item && item <= self.value_at(right - 1));
            if searched <= item {
                right = middle;
            } else {
                left = middle + 1;
            }
        }
        if left < self.get_length() {
            let item = self.value_at(left);
            if item == searched {
                return Some(left);
            }
        }
        None
    }
}
