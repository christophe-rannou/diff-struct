use super::utils::find_match;
use super::*;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub enum Change<T> {
    Change(T),
    #[default]
    NoChange,
}

impl<T> Change<T> {
    pub fn no_change(&self) -> bool {
        matches!(self, Change::NoChange)
    }

    pub fn unwrap(self) -> T {
        match self {
            Change::Change(value) => value,
            Change::NoChange => panic!("No change"),
        }
    }
}

impl Diff for bool {
    type Repr = bool;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        if self != other {
            Change::Change(*other)
        } else {
            Change::NoChange
        }
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        match diff {
            Change::Change(diff) => *self = *diff,
            Change::NoChange => {}
        }
    }

    fn identity() -> Self {
        false
    }
}

impl<T> Diff for Arc<T>
where
    T: Diff + Clone,
{
    type Repr = T::Repr;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        self.deref().diff(other.deref())
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        match Arc::get_mut(self) {
            Some(m) => m.apply(diff),
            None => {
                let mut x = (**self).clone();
                x.apply(diff);
                *self = Arc::new(x);
            }
        }
    }

    fn identity() -> Self {
        Arc::new(T::identity())
    }
}

impl<T> Diff for Box<T>
where
    T: Diff,
{
    type Repr = Box<Change<T::Repr>>;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        Change::Change(Box::new(self.deref().diff(other.deref())))
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        match diff {
            Change::Change(diff) => self.as_mut().apply(diff.as_ref()),
            Change::NoChange => {}
        }
    }

    fn identity() -> Self {
        Box::new(T::identity())
    }
}

impl<T> Diff for Rc<T>
where
    T: Diff + Clone,
{
    type Repr = T::Repr;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        self.deref().diff(other.deref())
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        match Rc::get_mut(self) {
            Some(m) => m.apply(diff),
            None => {
                let mut x = (**self).clone();
                x.apply(diff);
                *self = Rc::new(x);
            }
        }
    }

    fn identity() -> Self {
        Rc::new(T::identity())
    }
}

macro_rules! diff_tuple {
    (($($ty:ident),*), ($($access:tt),*)) => {
        impl<$($ty),*> Diff for ($($ty),*,)
            where $($ty: Diff),*
        {
            type Repr = ($(Change<<$ty>::Repr>),*,);

            fn diff(&self, other: &Self) -> Change<Self::Repr> {
                let diff_repr = ($(self.$access.diff(&other.$access)),*,);
                if $(diff_repr.$access.no_change())&&* {
                    Change::NoChange
                } else {
                    Change::Change(diff_repr)
                }
            }

            fn apply(&mut self, diff: &Change<Self::Repr>) {
                match diff {
                    Change::Change(diff) => {
                        $(self.$access.apply(&diff.$access));*;
                    }
                    Change::NoChange => {}
                }
            }

            fn identity() -> Self {
                ($(<$ty>::identity()),*,)
            }
        }
    }
}

diff_tuple!((A), (0));
diff_tuple!((A, B), (0, 1));
diff_tuple!((A, B, C), (0, 1, 2));
diff_tuple!((A, B, C, D), (0, 1, 2, 3));
diff_tuple!((A, B, C, D, F), (0, 1, 2, 3, 4));
diff_tuple!((A, B, C, D, F, G), (0, 1, 2, 3, 4, 5));
diff_tuple!((A, B, C, D, F, G, H), (0, 1, 2, 3, 4, 5, 6));
diff_tuple!((A, B, C, D, F, G, H, I), (0, 1, 2, 3, 4, 5, 6, 7));
diff_tuple!((A, B, C, D, F, G, H, I, J), (0, 1, 2, 3, 4, 5, 6, 7, 8));
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N, O),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N, O, P),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q, R),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
);

macro_rules! diff_int {
    ($($ty:ty),*) => {
        $(impl Diff for $ty {
            type Repr = $ty;

            fn diff(&self, other: &Self) -> Change<Self::Repr> {
                let value = other.wrapping_sub(*self);
                if value != 0 {
                    Change::Change(value)
                } else {
                    Change::NoChange
                }
            }

            fn apply(&mut self, diff: &Change<Self::Repr>) {
                match diff {
                    Change::Change(diff) => *self = self.wrapping_add(*diff),
                    Change::NoChange => {}
                }
            }

            fn identity() -> $ty {
                0
            }
        })*
    };
}

macro_rules! diff_float {
    ($($ty:ty),*) => {
        $(impl Diff for $ty {
            type Repr = $ty;

            fn diff(&self, other: &Self) -> Change<Self::Repr> {
                let value = other - self;
                if value != 0.0 {
                    Change::Change(value)
                } else {
                    Change::NoChange
                }
            }

            fn apply(&mut self, diff: &Change<Self::Repr>){
                match diff {
                    Change::Change(diff) => *self += diff,
                    Change::NoChange => {}
                }
            }

            fn identity() -> $ty {
                0.0
            }
        })*
    };
}

diff_int!(u8, i8, u16, i16, u32, i32, u64, i64, usize, isize);

macro_rules! diff_non_zero_int {
    ($($ty:ty, $original:ty),*) => {
        #[cfg(feature = "impl_num")]
        $(impl Diff for $ty {
            type Repr = $original;

            fn diff(&self, other: &Self) -> Change<Self::Repr> {
                let value = other.get().wrapping_sub(self.get());
                if value != 0 {
                    Change::Change(value)
                } else {
                    Change::NoChange
                }
            }

            fn apply(&mut self, diff: &Change<Self::Repr>) {
                match diff {
                    Change::Change(diff) => *self = <$ty>::new(self.get() + *diff).unwrap(),
                    Change::NoChange => {}
                }
            }

            fn identity() -> $ty {
                use num::traits::One;
                <$ty>::new(<$original>::one()).unwrap()
            }
        })*
    };
}

#[cfg(feature = "impl_num")]
use std::num::{NonZeroU128, NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8, NonZeroUsize};

diff_non_zero_int!(NonZeroU8, u8);
diff_non_zero_int!(NonZeroU16, u16);
diff_non_zero_int!(NonZeroU32, u32);
diff_non_zero_int!(NonZeroU64, u64);
diff_non_zero_int!(NonZeroU128, u128);
diff_non_zero_int!(NonZeroUsize, usize);

diff_float!(f32, f64);

impl Diff for char {
    type Repr = char;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        if self != other {
            Change::Change(*other)
        } else {
            Change::NoChange
        }
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        match diff {
            Change::Change(diff) => *self = *diff,
            Change::NoChange => {}
        }
    }

    fn identity() -> Self {
        '\x00'
    }
}

impl Diff for String {
    type Repr = String;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        if self != other {
            Change::Change(other.clone())
        } else {
            Change::NoChange
        }
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        match diff {
            Change::Change(diff) => *self = diff.clone(),
            Change::NoChange => {}
        }
    }

    fn identity() -> Self {
        String::new()
    }
}

impl Diff for PathBuf {
    type Repr = PathBuf;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        if self != other {
            Change::Change(other.clone())
        } else {
            Change::NoChange
        }
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        match diff {
            Change::Change(diff) => *self = diff.clone(),
            Change::NoChange => {}
        }
    }

    fn identity() -> Self {
        PathBuf::new()
    }
}

impl<'a> Diff for &'a str {
    type Repr = &'a str;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        if self != other {
            Change::Change(other)
        } else {
            Change::NoChange
        }
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        match diff {
            Change::Change(diff) => *self = diff,
            Change::NoChange => {}
        }
    }

    fn identity() -> Self {
        Default::default()
    }
}

impl<T> Diff for Cow<'_, T>
where
    T: ToOwned + PartialEq + ?Sized,
    <T as ToOwned>::Owned: Clone + Default,
{
    /// Note: This was done to make sure a diff is able to outlive its sources,
    /// which is the most desirable outcome if the diff is to be moved around
    /// and consumed much later (or even serialized into foreign programs)
    type Repr = <T as ToOwned>::Owned;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        if self != other {
            Change::Change(other.clone().into_owned())
        } else {
            Change::NoChange
        }
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        match diff {
            Change::Change(diff) => *self = Cow::Owned(diff.clone()),
            Change::NoChange => {}
        }
    }

    fn identity() -> Self {
        Default::default()
    }
}

impl<T: Diff + PartialEq> Diff for Option<T>
where
    T::Repr: Clone, // Clone required for Repr to apply, is that too much of a constraint ? Alternatively the Repr could be Option<Change<T::Repr>> to avoid the clone but it makes the diff more complex
{
    type Repr = Option<T::Repr>;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        match (self, other) {
            (Some(value), Some(other_value)) => match value.diff(other_value) {
                Change::Change(diff) => Change::Change(Some(diff)),
                Change::NoChange => Change::NoChange,
            },
            (Some(_), None) => Change::Change(None),
            (None, Some(other_value)) => {
                Change::Change(Some(T::identity().diff(other_value).unwrap()))
            }
            (None, None) => Change::NoChange,
        }
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        match diff {
            Change::Change(diff) => match diff.clone() {
                Some(change) => {
                    if let Some(value) = self {
                        value.apply(&Change::Change(change));
                    } else {
                        *self = Some(T::identity().apply_new(&Change::Change(change)))
                    }
                }
                None => *self = None,
            },
            Change::NoChange => {}
        }
    }

    fn identity() -> Self {
        None
    }
}

/// The diff struct used to compare two [HashMap]'s
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "V: Serialize, V::Repr: Serialize, K: Serialize"))]
#[serde(bound(
    deserialize = "V: Deserialize<'de>, V::Repr: Deserialize<'de>, K: Deserialize<'de>"
))]
pub struct HashMapDiff<K: Hash + Eq, V: Diff> {
    /// Values that are changed or added
    pub altered: HashMap<K, <V as Diff>::Repr>,
    /// Values that were added
    pub added: HashMap<K, V>,
    /// Values that are removed
    pub removed: HashSet<K>,
}

/// The diff struct used to compare two [BTreeMap]'s
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "V: Serialize, V::Repr: Serialize, K: Serialize"))]
#[serde(bound(
    deserialize = "V: Deserialize<'de>, V::Repr: Deserialize<'de>, K: Deserialize<'de>"
))]
pub struct BTreeMapDiff<K: Ord + Eq, V: Diff> {
    /// Values that changed
    pub altered: BTreeMap<K, <V as Diff>::Repr>,
    /// Values that were added
    pub added: BTreeMap<K, V>,
    /// Values that are removed
    pub removed: BTreeSet<K>,
}

macro_rules! diff_map {
    ($ty: ident, $diffty: ident, $diffkey: ident, ($($constraints:tt)*)) => {
        impl<K: $($constraints)*, V: Diff> Diff for $ty<K, V>
        where
            K: Clone,
            V: PartialEq + Clone,
            <V as Diff>::Repr: Clone,
        {
            type Repr = $diffty<K, V>;

            fn diff(&self, other: &Self) -> Change<Self::Repr> {
                let mut diff = $diffty {
                    altered: $ty::new(),
                    added: $ty::new(),
                    removed: $diffkey::new(),
                };
                for (key, value) in self {
                    if let Some(other_value) = other.get(key) {
                        match value.diff(other_value) {
                            Change::Change(value_diff) => {
                                diff.altered.insert(key.clone(), value_diff);
                            }
                            Change::NoChange => {}
                        }
                    } else {
                        diff.removed.insert(key.clone());
                    }
                }
                for (key, value) in other {
                    if let None = self.get(key) {
                        diff.added.insert(key.clone(), value.clone());
                    }
                }
                if diff.altered.is_empty() && diff.added.is_empty() && diff.removed.is_empty() {
                    Change::NoChange
                } else {
                    Change::Change(diff)
                }
            }

            // basically inexpensive
            fn apply(&mut self, diff: &Change<Self::Repr>) {
                match diff {
                    Change::Change(diff) => {
                        diff.removed.iter().for_each(|del| {
                            self.remove(del);
                        });
                        for (key, value) in &diff.added {
                            self.insert(key.clone(), value.clone());
                        }
                        for (key, change) in &diff.altered {
                            if let Some(original) = self.get_mut(key) {
                                original.apply(&Change::Change(change.clone()));
                            } else {
                                self.insert(key.clone(), V::identity().apply_new(&Change::Change(change.clone())));
                            }
                        }
                    }
                    Change::NoChange => {}
                }
            }

            fn identity() -> Self {
                $ty::new()
            }
        }

        impl<K: $($constraints)*, V: Diff> Debug for $diffty<K, V>
        where
            K: Debug,
            V: Debug,
            V::Repr: Debug,
        {
            fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
                f.debug_struct(stringify!($diffty))
                    .field("altered", &self.altered)
                    .field("added", &self.added)
                    .field("removed", &self.removed)
                    .finish()
            }
        }

        impl<K: $($constraints)*, V: Diff> PartialEq for $diffty<K, V>
        where
            V: PartialEq,
            V::Repr: PartialEq,
        {
            fn eq(&self, other: &Self) -> bool {
                self.altered == other.altered && self.removed == other.removed && self.added == other.added
            }
        }

        impl<K: $($constraints)*, V: Diff> Clone for $diffty<K, V>
        where
            K: Clone,
            V: Clone,
            V::Repr: Clone,
        {
            fn clone(&self) -> Self {
                $diffty {
                    altered: self.altered.clone(),
                    added: self.added.clone(),
                    removed: self.removed.clone(),
                }
            }
        }
    }
}

diff_map!(HashMap, HashMapDiff, HashSet, (Hash + Eq));
diff_map!(BTreeMap, BTreeMapDiff, BTreeSet, (Ord));

/// The diff struct used to compare two [HashSet]'s
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "T: Serialize"))]
#[serde(bound(deserialize = "T: Deserialize<'de>"))]
pub struct HashSetDiff<T: Hash + Eq> {
    /// Values that are added
    pub added: HashSet<T>,
    /// Values that are removed
    pub removed: HashSet<T>,
}

/// The diff struct used to compare two [BTreeMap]'s
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "T: Serialize"))]
#[serde(bound(deserialize = "T: Deserialize<'de>"))]
pub struct BTreeSetDiff<T: Ord + Eq> {
    /// Values that are added
    pub added: BTreeSet<T>,
    /// Values that are removed
    pub removed: BTreeSet<T>,
}

macro_rules! diff_set {
    ($ty: ident, $diffty: ident, $diffkey: ident, ($($constraints:tt)*)) => {
        impl<T: $($constraints)*> Diff for $ty<T>
        where
            T: Clone,
        {
            type Repr = $diffty<T>;

            fn diff(&self, other: &Self) -> Change<Self::Repr> {
                let mut diff = $diffty {
                    added: $diffkey::new(),
                    removed: $diffkey::new(),
                };
                for value in self {
                    if !other.contains(value) {
                        diff.removed.insert(value.clone());
                    }
                }
                for value in other {
                    if !self.contains(value) {
                        diff.added.insert(value.clone());
                    }
                }
                if diff.added.is_empty() && diff.removed.is_empty() {
                    Change::NoChange
                } else {
                    Change::Change(diff)
                }
            }

            // basically inexpensive
            fn apply(&mut self, diff: &Change<Self::Repr>) {
                match diff {
                    Change::Change(diff) => {
                        diff.removed.iter().for_each(|del| {
                            self.remove(del);
                        });
                        diff.added.iter().for_each(|add| {
                            self.insert(add.clone());
                        });
                    }
                    Change::NoChange => {}
                }
            }

            fn identity() -> Self {
                $ty::new()
            }
        }

        impl<T: $($constraints)*> Debug for $diffty<T>
        where
            T: Debug,
        {
            fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
                f.debug_struct(stringify!($diffty))
                    .field("added", &self.added)
                    .field("removed", &self.removed)
                    .finish()
            }
        }

        impl<T: $($constraints)*> PartialEq for $diffty<T>
        where
            T: PartialEq,
        {
            fn eq(&self, other: &Self) -> bool {
                self.added == other.added && self.removed == other.removed
            }
        }

        impl<T: $($constraints)*> Clone for $diffty<T>
        where
            T: Clone,
        {
            fn clone(&self) -> Self {
                $diffty {
                    added: self.added.clone(),
                    removed: self.removed.clone(),
                }
            }
        }
    }
}

diff_set!(HashSet, HashSetDiff, HashSet, (Hash + Eq));
diff_set!(BTreeSet, BTreeSetDiff, BTreeSet, (Ord));

/// The type of change to make to a vec
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "T:Serialize, T::Repr: Serialize"))]
#[serde(bound(deserialize = "T: Deserialize<'de>, T::Repr: Deserialize<'de>"))]
pub enum VecDiffType<T: Diff> {
    Removed { index: usize, len: usize },
    Altered { index: usize, changes: Vec<T::Repr> },
    Inserted { index: usize, changes: Vec<T> },
}

/// The collection of difference-vec's
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "T:Serialize, T::Repr: Serialize"))]
#[serde(bound(deserialize = "T: Deserialize<'de>, T::Repr: Deserialize<'de>"))]
pub struct VecDiff<T: Diff>(pub Vec<VecDiffType<T>>);

impl<T: Diff + PartialEq> Diff for Vec<T>
where
    T: Clone,
    T::Repr: Clone,
{
    type Repr = VecDiff<T>;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        let mut changes = Vec::new();
        let mut pos_x = 0;
        let mut pos_y = 0;
        loop {
            let (is_match, deletions, insertions) = find_match(&self[pos_x..], &other[pos_y..]);

            // TODO: simplify logic here
            if deletions == 0 || insertions == 0 {
                if deletions > 0 {
                    changes.push(VecDiffType::Removed {
                        index: pos_x,
                        len: deletions,
                    });
                } else if insertions > 0 {
                    changes.push(VecDiffType::Inserted {
                        index: pos_x,
                        changes: other[pos_y..pos_y + insertions].to_vec(),
                    });
                }
            } else if deletions == insertions {
                changes.push(VecDiffType::Altered {
                    index: pos_x,
                    changes: self[pos_x..pos_x + deletions]
                        .iter()
                        .zip(other[pos_y..pos_y + insertions].iter())
                        .map(|(a, b)| a.diff(b).unwrap())
                        .collect(),
                });
            } else if deletions > insertions {
                changes.push(VecDiffType::Altered {
                    index: pos_x,
                    changes: self[pos_x..pos_x + insertions]
                        .iter()
                        .zip(other[pos_y..pos_y + insertions].iter())
                        .map(|(a, b)| a.diff(b).unwrap())
                        .collect(),
                });
                changes.push(VecDiffType::Removed {
                    index: pos_x + insertions,
                    len: deletions - insertions,
                });
            } else {
                changes.push(VecDiffType::Altered {
                    index: pos_x,
                    changes: self[pos_x..pos_x + deletions]
                        .iter()
                        .zip(other[pos_y..pos_y + deletions].iter())
                        .map(|(a, b)| a.diff(b).unwrap())
                        .collect(),
                });
                changes.push(VecDiffType::Inserted {
                    index: pos_x + deletions,
                    changes: other[pos_y + deletions..pos_y + insertions].to_vec(),
                });
            }

            if is_match {
                pos_x += deletions + 1;
                pos_y += insertions + 1;
            } else {
                break;
            }
        }
        if changes.is_empty() {
            Change::NoChange
        } else {
            Change::Change(VecDiff(changes))
        }
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        let changes = match diff {
            Change::Change(changes) => &changes.0,
            Change::NoChange => return,
        };
        let mut relative_index = 0_isize;
        for change in changes {
            match change {
                VecDiffType::Removed { index, len } => {
                    let index = (*index as isize + relative_index) as usize;
                    self.drain(index..index + len);
                    relative_index -= *len as isize;
                }
                VecDiffType::Inserted { index, changes } => {
                    let index = (*index as isize + relative_index) as usize;
                    self.splice(index..index, changes.iter().cloned());
                    relative_index += changes.len() as isize;
                }
                VecDiffType::Altered { index, changes } => {
                    let index = (*index as isize + relative_index) as usize;
                    let range = index..index + changes.len();
                    for (value, diff) in self[range].iter_mut().zip(changes.iter()) {
                        value.apply(&Change::Change(diff.clone()));
                    }
                }
            }
        }
    }

    fn identity() -> Self {
        Vec::new()
    }
}

impl<T: Diff> Debug for VecDiffType<T>
where
    T: Debug,
    T::Repr: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            VecDiffType::Removed { index, len } => f
                .debug_struct("Removed")
                .field("index", index)
                .field("len", len)
                .finish(),
            VecDiffType::Altered { index, changes } => f
                .debug_struct("Altered")
                .field("index", index)
                .field("changes", changes)
                .finish(),
            VecDiffType::Inserted { index, changes } => f
                .debug_struct("Inserted")
                .field("index", index)
                .field("changes", changes)
                .finish(),
        }
    }
}

impl<T: Diff> PartialEq for VecDiffType<T>
where
    T: PartialEq,
    T::Repr: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                VecDiffType::Removed { index, len },
                VecDiffType::Removed {
                    index: ref index_,
                    len: ref len_,
                },
            ) => index == index_ && len == len_,
            (
                VecDiffType::Altered { index, changes },
                VecDiffType::Altered {
                    index: ref index_,
                    changes: ref changes_,
                },
            ) => index == index_ && changes == changes_,
            (
                VecDiffType::Inserted { index, changes },
                VecDiffType::Inserted {
                    index: ref index_,
                    changes: ref changes_,
                },
            ) => index == index_ && changes == changes_,
            _ => false,
        }
    }
}

impl<T: Diff> Clone for VecDiffType<T>
where
    T: Clone,
    T::Repr: Clone,
{
    fn clone(&self) -> Self {
        match self {
            VecDiffType::Removed { index, len } => VecDiffType::Removed {
                index: *index,
                len: *len,
            },
            VecDiffType::Altered { index, changes } => VecDiffType::Altered {
                index: *index,
                changes: changes.clone(),
            },
            VecDiffType::Inserted { index, changes } => VecDiffType::Inserted {
                index: *index,
                changes: changes.clone(),
            },
        }
    }
}

impl<T: Diff> Debug for VecDiff<T>
where
    T: Debug,
    T::Repr: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_list().entries(self.0.iter()).finish()
    }
}

impl<T: Diff> PartialEq for VecDiff<T>
where
    T: PartialEq,
    T::Repr: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Diff> Clone for VecDiff<T>
where
    T: Clone,
    T::Repr: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

/// The type of change to make to an array
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "T::Repr: Serialize"))]
#[serde(bound(deserialize = "T::Repr: Deserialize<'de>"))]
pub struct ArrayDiffType<T: Diff> {
    pub index: usize,
    pub change: T::Repr,
}

/// The collection of difference-vec's
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "T::Repr: Serialize"))]
#[serde(bound(deserialize = "T::Repr: Deserialize<'de>"))]
pub struct ArrayDiff<T: Diff>(pub Vec<ArrayDiffType<T>>);

impl<T: Diff + PartialEq, const N: usize> Diff for [T; N]
where
    T::Repr: Clone,
{
    type Repr = ArrayDiff<T>;

    fn diff(&self, other: &Self) -> Change<Self::Repr> {
        let diffs = self
            .iter()
            .zip(other.iter())
            .enumerate()
            .filter(|&(_, (self_el, other_el))| self_el.ne(other_el))
            .map(|(index, (self_el, other_el))| ArrayDiffType {
                index,
                change: self_el.diff(other_el).unwrap(),
            })
            .collect::<Vec<ArrayDiffType<T>>>();
        if diffs.is_empty() {
            Change::NoChange
        } else {
            Change::Change(ArrayDiff(diffs))
        }
    }

    fn apply(&mut self, diff: &Change<Self::Repr>) {
        let diffs = match diff {
            Change::Change(diffs) => &diffs.0,
            Change::NoChange => return,
        };

        for ArrayDiffType { index, change } in diffs {
            self[*index].apply(&Change::Change(change.clone()));
        }
    }

    fn identity() -> Self {
        std::array::from_fn(|_| T::identity())
    }
}

impl<T: Diff> Debug for ArrayDiffType<T>
where
    T::Repr: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("ArrayDiffType")
            .field("index", &self.index)
            .field("change", &self.change)
            .finish()
    }
}

impl<T: Diff> PartialEq for ArrayDiffType<T>
where
    T::Repr: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index && self.change == other.change
    }
}

impl<T: Diff> Clone for ArrayDiffType<T>
where
    T::Repr: Clone,
{
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            change: self.change.clone(),
        }
    }
}

impl<T: Diff> Debug for ArrayDiff<T>
where
    T::Repr: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_list().entries(self.0.iter()).finish()
    }
}

impl<T: Diff> PartialEq for ArrayDiff<T>
where
    T::Repr: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Diff> Clone for ArrayDiff<T>
where
    T::Repr: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Diff for PhantomData<T> {
    type Repr = PhantomData<T>;

    fn diff(&self, _other: &Self) -> Change<Self::Repr> {
        Change::NoChange
    }

    fn apply(&mut self, _diff: &Change<Self::Repr>) {}

    fn identity() -> Self {
        PhantomData
    }
}
