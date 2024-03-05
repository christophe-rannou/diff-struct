use super::*;

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::path::PathBuf;
use std::rc::Rc;

fn identity_test<D: Diff + Debug + PartialEq>(s: D) {
    assert_eq!(D::identity().apply_new(&D::identity().diff(&s)), s);
}

fn generate_map<K: Eq + Hash, V>(parts: Vec<(K, V)>) -> HashMap<K, V> {
    parts.into_iter().collect::<HashMap<_, _>>()
}

fn generate_set<T: Eq + Hash>(parts: Vec<T>) -> HashSet<T> {
    parts.into_iter().collect::<HashSet<_, _>>()
}

#[test]
fn numeric_diffs() {
    identity_test(true);
    identity_test(42_u8);
    identity_test(42_i8);
    identity_test(42_u16);
    identity_test(42_i16);
    identity_test(42_u32);
    identity_test(42_i32);
    identity_test(42.0_f32);
    identity_test(42.0_f64);
}

#[test]
fn test_char_string() {
    identity_test('b');
    identity_test(String::from("42"));
    assert_eq!('b'.diff(&'c'), Change::Change('c'));
    assert_eq!('b'.diff(&'b'), Change::NoChange);
    assert_eq!(
        String::from("42").diff(&String::from("asdf")),
        Change::Change(String::from("asdf"))
    );
    assert_eq!(
        String::from("42").diff(&String::from("42")),
        Change::NoChange
    );
}

#[test]
fn test_cow() {
    // Cow<'_, str>
    assert_eq!(
        Cow::from("42").diff(&Cow::from("asdf")),
        Change::Change("asdf".into())
    );
    assert_eq!(Cow::from("42").diff(&Cow::from("42")), Change::NoChange);
}

#[test]
fn test_opt() {
    assert_eq!(
        Some(10).diff(&Some(15)),
        Change::Change(Some(Change::Change(5)))
    );
    assert_eq!(
        None.apply_new(&Change::Change(Some(Change::Change(5)))),
        Some(5)
    );
    assert_eq!(Some(100).apply_new(&Change::Change(None)), None);
    assert_eq!(Some(20).diff(&Some(20)), Change::NoChange);
    identity_test(Some(42))
}

// #[test]
// fn test_maps() {
//     let a = generate_map(vec![("a", 1), ("b", 2), ("x", 42)]);
//     let b = generate_map(vec![("b", 3), ("c", 4), ("x", 42)]);
//     let expected = HashMapDiff {
//         altered: generate_map(vec![("b", 1), ("c", 4)]),
//         removed: vec!["a"].into_iter().collect::<HashSet<_>>(),
//     };
//     assert_eq!(a.diff(&b), expected);
//     identity_test(a);
// }

// #[test]
// fn test_sets() {
//     let a = generate_set(vec![1, 2, 42]);
//     identity_test(a.clone());

//     let b = generate_set(vec![1, 4, 42]);
//     let diff = a.diff(&b);
//     let expected = HashSetDiff {
//         added: vec![4].into_iter().collect::<HashSet<_>>(),
//         removed: vec![2].into_iter().collect::<HashSet<_>>(),
//     };
//     assert_eq!(diff, expected);

//     let mut a_plus_diff = a;
//     a_plus_diff.apply(&diff);
//     assert_eq!(a_plus_diff, b);
// }

#[test]
fn test_path() {
    let a = PathBuf::from(r"/example/path/to/file.ext");
    let b = PathBuf::from(r"/different/path");
    identity_test(a.clone());
    assert_eq!(a.diff(&b), Change::Change(b.clone()));
}

#[derive(Debug, PartialEq, Diff)]
#[diff(attr(
    #[derive(Debug, PartialEq)]
))]
#[diff(name(TestDiff))]
#[diff(path(crate))]
struct TestStruct {
    a: bool,
    b: u32,
}

#[test]
fn test_derive() {
    let a = TestStruct { a: false, b: 42 };

    let b = TestStruct { a: true, b: 43 };

    let diff = TestDiff {
        a: Change::Change(true.into()),
        b: Change::Change(1),
    };
    assert_eq!(a.diff(&b), Change::Change(diff));

    identity_test(a);
}

#[derive(Debug, PartialEq, Diff)]
#[diff(attr(
    #[derive(Debug, PartialEq)]
))]
#[diff(path(crate))]
struct TestTupleStruct(i32);

#[test]
fn test_tuple_derive() {
    let a = TestTupleStruct(10);
    let b = TestTupleStruct(30);
    let diff = TestTupleStructDiff(Change::Change(20));
    assert_eq!(a.diff(&b), Change::Change(diff));
}

// #[derive(Debug, Default, PartialEq, Diff)]
// #[diff(visibility(pub))]
// #[diff(path(crate))]
// struct ProjectMeta {
//     contributors: Vec<String>,
//     combined_work_hours: usize,
// }

// #[test]
// fn test_apply() {
//     let mut base = ProjectMeta::default();
//     let contribution_a = ProjectMeta {
//         contributors: vec!["Alice".into()],
//         combined_work_hours: 3,
//     };
//     let contribution_b = ProjectMeta {
//         contributors: vec!["Bob".into(), "Candice".into()],
//         combined_work_hours: 10,
//     };
//     let expected = ProjectMeta {
//         contributors: vec!["Bob".into(), "Candice".into(), "Alice".into()],
//         combined_work_hours: 13,
//     };
//     let diff_a = base.diff(&contribution_a);
//     let diff_b = base.diff(&contribution_b);
//     base.apply(&diff_a);
//     base.apply(&diff_b);
//     assert_eq!(base, expected);
// }

// #[test]
// fn test_vecs() {
//     let a = vec![0, 1, 2, 3, 4, 5, 6, 7];
//     let b = vec![0, /*1, 2*/ 3, 4, 42, 5, /*6 ->*/ 10, 7];
//     let diff = VecDiff(vec![
//         VecDiffType::Removed { index: 1, len: 2 },
//         VecDiffType::Inserted {
//             index: 5,
//             changes: vec![42],
//         },
//         VecDiffType::Altered {
//             index: 6,
//             changes: vec![4], // add 4 to 6
//         },
//     ]);
//     assert_eq!(diff, a.diff(&b));
//     assert_eq!(a.apply_new(&diff), b);
// }

// #[test]
// fn test_arrays() {
//     let array = [1, 2, 3, 4, 5];
//     identity_test(array);
//     let other = [1, 2, 7, 4, 0];
//     let diff = array.diff(&other);
//     assert_eq!(
//         diff,
//         ArrayDiff(vec![
//             ArrayDiffType {
//                 index: 2,
//                 change: 4
//             },
//             ArrayDiffType {
//                 index: 4,
//                 change: -5
//             }
//         ])
//     );
//     assert_eq!(array.apply_new(&diff), other);
// }

// use serde::Serialize;

// #[derive(Default, PartialEq, Serialize, Diff)]
// #[diff(name(SpecialName))]
// #[diff(visibility(pub))]
// #[diff(attr(
//     #[derive(Default, PartialEq, Serialize)]
// ))]
// #[diff(path(crate))]
// /// A struct with a lot of attributes
// struct MyTestStruct {
//     #[diff(name(special_field_name))]
//     #[diff(visibility(pub))]
//     #[diff(attr(
//         #[serde(rename = "name")]
//     ))]
//     /// This field has a lot of attributes too
//     test_field: u32,
// }

// #[test]
// fn test_full_struct() {
//     let base = MyTestStruct::default();
//     let other = MyTestStruct { test_field: 1 };

//     let diff = base.diff(&other);
//     assert_eq!(diff.special_field_name, 1);
// }

// #[derive(Diff, PartialEq)]
// #[diff(attr(#[derive(Debug, PartialEq)]))]
// #[diff(path(crate))]
// struct PhantomDataTest<T> {
//     value: i32,
//     phantom: PhantomData<T>,
// }

// #[test]
// fn test_phantom_data() {
//     let base = PhantomDataTest::<String> {
//         value: 100,
//         phantom: Default::default(),
//     };
//     let other = PhantomDataTest::<String> {
//         value: 142,
//         phantom: Default::default(),
//     };
//     assert_eq!(
//         base.diff(&other),
//         PhantomDataTestDiff::<String> {
//             value: 42,
//             phantom: Default::default(),
//         }
//     );
// }

// #[test]
// fn test_box() {
//     let array = [1, 2, 3, 4, 5];
//     identity_test(array);
//     let other = [1, 2, 7, 4, 0];

//     let array = Box::new(array);
//     let other = Box::new(other);

//     let diff = array.diff(&other);
//     assert_eq!(
//         diff,
//         Box::new(ArrayDiff(vec![
//             ArrayDiffType {
//                 index: 2,
//                 change: 4
//             },
//             ArrayDiffType {
//                 index: 4,
//                 change: -5
//             }
//         ]))
//     );
//     assert_eq!(array.apply_new(&diff), other);
// }

// #[derive(Debug, PartialEq, Diff)]
// #[diff(path(crate))]
// #[diff(attr(#[derive(Debug, PartialEq)]))]
// struct LinkedListNode<T: Diff + PartialEq>
// where
//     T::Repr: Debug + PartialEq,
// {
//     value: T,
//     child: Option<Box<LinkedListNode<T>>>,
// }

// #[test]
// fn test_box_recursive() {
//     let node = LinkedListNode {
//         value: 10,
//         child: Some(Box::new(LinkedListNode {
//             value: 2,
//             child: Some(Box::new(LinkedListNode {
//                 value: 1,
//                 child: None,
//             })),
//         })),
//     };
//     let other = LinkedListNode {
//         value: 42,
//         child: Some(Box::new(LinkedListNode {
//             value: 2,
//             child: None,
//         })),
//     };
//     let diff = node.diff(&other);
//     assert_eq!(
//         diff,
//         LinkedListNodeDiff {
//             value: 32,
//             child: OptionDiff::Some(Box::new(LinkedListNodeDiff {
//                 value: 0,
//                 child: OptionDiff::None
//             })),
//         }
//     );
//     assert_eq!(node.apply_new(&diff), other);
// }

// #[test]
// fn test_rc() {
//     let array = [1, 2, 7, 4, 5];
//     identity_test(array);
//     let other = [1, 2, 7, 4, 0];

//     let array = Rc::new(array);
//     let other = Rc::new(other);

//     let diff = array.diff(&other);
//     assert_eq!(
//         diff,
//         ArrayDiff(vec![ArrayDiffType {
//             index: 4,
//             change: -5
//         },])
//     );
//     assert_eq!(array.apply_new(&diff), other);
// }
