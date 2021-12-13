use std::collections::HashMap;

/// A generic trie relying on cloneable and comparable elements.
/// The empty Slice<T> is a valid element to find/delete/insert.
pub struct Trie<T: Eq + Clone, V> {
    children: Vec<(T, Trie<T, V>)>,
    terminal: Option<V>,
}

impl<T: Eq + Clone, V> Trie<T, V> {
    pub fn new() -> Trie<T, V> {
        Trie { children: vec![], terminal: None }
    }
    fn in_children(trie: &Trie<T, V>, val: &T) -> Option<usize> {
        for (i, (child_val, child_trie)) in trie.children.iter().enumerate() {
            if *child_val == *val {
                return Some(i);
            }
        }
        None
    }

    /// Add list<T> to the trie
    pub fn insert(&mut self, list: &[T], val: V) {
        let mut node = self;
        for element in list.iter() {
            if let Some(child_idx) =  Trie::in_children(node, element) {
                node = &mut node.children.get_mut(child_idx).expect("not possible idx returned from enumerate of this vec, please report a bug").1;
            } else {
                node.children.push((element.clone(), Trie::new()));
                node = &mut node.children.last_mut().expect("not possible just pushed, please report a bug").1;
            }
        }
        node.terminal = Some(val);
    }

    /// Is the list<T> in the trie?
    pub fn find(&self, list: &[T]) -> &Option<V> {
        let mut node = self;
        for element in list.iter() {
            if let Some(child_idx) = Trie::in_children(&node, element ) {
                node = &node.children.get(child_idx).expect("not possible idx returns from enum, please report a bug").1;
            } else {
                return &Option::<V>::None;
            }
        }
        &node.terminal
    }
}

#[test]
fn simple_test() {
    let mut trie: Trie<u8, u64> = Trie::new();
    trie.insert(&[1,2], 3);
    assert_eq!(trie.find(&[1]), &Option::<u64>::None);
    assert_eq!(trie.find(&[1,2]), &Option::<u64>::Some(3));
}

#[test]
fn test_trie() {
    let non = &Option::<u64>::None;
    let mut trie: Trie<u8, u64> = Trie::new();
    assert_eq!(trie.find(&[1,2,3]), non);
    assert_eq!(trie.find(&[1,1,1]), non);
    assert_eq!(trie.find(&[1,1,1,1,1,1,1,1,1,1,255]), non);
    assert_eq!(trie.find(&[]), non);
    trie.insert(&[1,2,3,4,5], 12345);
    assert_eq!(trie.find(&[1,2,3,4,5]), &Option::<u64>::Some(12345));
    assert_eq!(trie.find(&[1,2,3,4]), non);
    assert_eq!(trie.find(&[2,3,4,5]), non);
    trie.insert(&[1,2,3,3,3], 12333);
    trie.insert(&[1], 1);
    trie.insert(&[2], 2);
    trie.insert(&[3], 3);
    assert_eq!(trie.find(&[1,2,3,4,5]), &Option::<u64>::Some(12345));
    assert_eq!(trie.find(&[1,2,3,3,3]), &Option::<u64>::Some(12333));
    assert_eq!(trie.find(&[1,2,3,4]), non);
    assert_eq!(trie.find(&[2,3,4,5]), non);
    assert_eq!(trie.find(&[1]), &Option::<u64>::Some(1));
    assert_eq!(trie.find(&[2]), &Option::<u64>::Some(2));
    assert_eq!(trie.find(&[3]), &Option::<u64>::Some(3));
    assert_eq!(trie.find(&[4]), non);
}



#[test]
fn test_trie_rand() {

    use rand::Rng;
    use std::collections::HashSet;

    fn make_vec() -> Vec<usize> {
        let mut rng = rand::thread_rng();
        let length = rng.gen::<u8>();
        let mut vec: Vec<usize> = vec![0; length.into()];
        for idx in 0..length {
            vec[(idx as usize)] = rng.gen::<usize>();
        }
        vec
    }

    fn test_a_trie() {
        let mut rng = rand::thread_rng();

        let total_ops: u8 = rng.gen::<u8>();
        let mut trie: Trie<usize, u8> = Trie::new();

        let mut inserted: HashMap<Vec<usize>, u8> = HashMap::new();
        let mut inserted_vec: Vec<Vec<usize>> = vec![];

        for i in 0..total_ops {
            println!("Running {} times", total_ops);
            let action = rng.gen_range(0..2);
            if action == 1 {
                // do an insert
                let value = rng.gen::<u8>();
                let vec = make_vec();
                trie.insert(vec.as_slice(), value);
                inserted.insert(vec.clone(), value);
                inserted_vec.push(vec);
            } else if action == 2 {
                // do a find
                let find_inserted = rng.gen::<bool>() && inserted.len() > 0;
                if find_inserted {
                    // find a thing we inserted
                    let rand_idx = rng.gen_range(0..inserted_vec.len());
                    let check_me: &Vec<usize> = inserted_vec.get(rand_idx).unwrap();
                    let value = inserted[check_me];
                    assert_eq!(trie.find(check_me.as_slice()), &Some(value))
                } else {
                    // find something we didn't insert
                    let mut vec = make_vec();
                    while inserted.contains_key(&vec) {
                        vec = make_vec();
                    }
                    assert_eq!(trie.find(vec.as_slice()), &Option::<u8>::None);
                }
            }
        }
    }

    for _ in 0..100 {
        test_a_trie();
    }
}