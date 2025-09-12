use std::{array::from_fn, vec};

use motoko_rts::algorithms::SortedArray;

pub fn test() {
    println!("Testing algorithms ...");

    test_binary_search();
}

pub fn test_binary_search() {
    println!("  Testing binary search ...");

    assert_eq!(find(vec![], 1), None);
    assert_eq!(find(vec![1], 1), Some(1));
    assert_eq!(find(vec![1], 0), None);
    assert_eq!(find(vec![1], 2), None);

    let array: [usize; 1000] = from_fn(|index| index * 2 + 1);
    let vector = array.to_vec();
    for index in 0..array.len() {
        assert_eq!(find(vector.clone(), index * 2), None);
        assert_eq!(find(vector.clone(), index * 2 + 1), Some(index * 2 + 1));
    }
    assert_eq!(find(vector, array.len() * 2 + 2), None);
}

struct TestArrary {
    vector: Vec<usize>,
}

impl TestArrary {
    fn new(vector: Vec<usize>) -> Self {
        TestArrary { vector }
    }
}

impl SortedArray<usize> for TestArrary {
    fn get_length(&self) -> usize {
        self.vector.len()
    }

    fn value_at(&self, index: usize) -> usize {
        self.vector[index]
    }
}

fn find(vector: Vec<usize>, searched: usize) -> Option<usize> {
    let test_array = TestArrary::new(vector);
    match test_array.index_of(searched) {
        None => None,
        Some(index) => Some(test_array.vector[index]),
    }
}
