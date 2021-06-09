#![allow(missing_docs)] // don't publish a crate with this!
#![deny(rust_2018_idioms)]
#![deny(clippy::too_many_arguments)]
#![deny(clippy::complexity)]
#![deny(clippy::perf)]
#![forbid(unsafe_code)]
#![warn(clippy::style)]
// select lints from clippy::pedantic
#![deny(clippy::needless_pass_by_value)]
#![deny(clippy::explicit_iter_loop)]
#![deny(clippy::implicit_clone)]
#![deny(clippy::redundant_else)]
#![deny(clippy::missing_panics_doc)]
#![deny(clippy::fn_params_excessive_bools)]

use std::{
    cmp,
    collections::HashMap,
    collections::HashSet,
    path::{Path, PathBuf},
};

pub mod display;
mod platform;
pub mod tree;
pub mod walk_dirs;

pub fn simplify_dir_names<P: AsRef<Path>>(filenames: Vec<P>) -> HashSet<PathBuf> {
    let mut top_level_names: HashSet<PathBuf> = HashSet::with_capacity(filenames.len());
    let mut to_remove: Vec<PathBuf> = Vec::with_capacity(filenames.len());

    for t in filenames {
        let top_level_name = normalize_path(t);
        let mut can_add = true;

        for tt in &top_level_names {
            if is_a_parent_of(&top_level_name, tt) {
                to_remove.push(tt.clone());
            } else if is_a_parent_of(tt, &top_level_name) {
                can_add = false;
            }
        }
        to_remove.sort_unstable();
        top_level_names.retain(|tr| to_remove.binary_search(tr).is_err());
        to_remove.clear();
        if can_add {
            top_level_names.insert(top_level_name);
        }
    }

    top_level_names
}

pub(crate) fn is_a_parent_of<P: AsRef<Path>>(parent: P, child: P) -> bool {
    let parent = parent.as_ref();
    let child = child.as_ref();
    child.starts_with(parent) && !parent.starts_with(child)
}

#[derive(Clone)]
pub struct FileSize {
    pub(crate) size: u64,
    pub(crate) filename: PathBuf,
}

pub fn sort_into_vec(nodes: HashMap<PathBuf, u64>) -> Vec<FileSize> {
    use cmp::Ordering::{self, *};
    fn reverse_size_then_filename(a: &FileSize, b: &FileSize) -> Ordering {
        let result = b.size.cmp(&a.size);
        if result == Equal {
            a.filename.cmp(&b.filename)
        } else {
            result
        }
    }
    let mut new_l = nodes
        .into_iter()
        .map(|(k, v)| FileSize {
            size: v,
            filename: k,
        })
        .collect::<Vec<_>>();
    new_l.sort_unstable_by(reverse_size_then_filename);
    new_l
}

pub fn find_big_ones(nodes: Vec<FileSize>, max_to_show: usize) -> Vec<FileSize> {
    if max_to_show > 0 && nodes.len() > max_to_show {
        nodes[0..max_to_show].to_vec()
    } else {
        nodes
    }
}

/// Normalizes the path, by:
/// 1. removing repeated separators.
/// 2. removing interior '.' ("current directory") path segments.
/// 3. removing trailing extra separators and '.' ("current directory") path segments.
///    Note: `Path.components()` does all the above work; ref: <https://doc.rust-lang.org/std/path/struct.Path.html#method.components>
/// 4. changing to os preferred separator (automatically done by recollecting components back into a PathBuf).
fn normalize_path<P: AsRef<Path>>(path: P) -> PathBuf {
    path.as_ref().components().collect::<PathBuf>()
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_simplify_dir() {
        let mut correct = HashSet::new();
        correct.insert(PathBuf::from("a"));
        assert_eq!(simplify_dir_names(vec!["a"]), correct);
    }

    #[test]
    fn test_simplify_dir_rm_subdir() {
        let mut correct = HashSet::new();
        correct.insert(["a", "b"].iter().collect::<PathBuf>());
        assert_eq!(simplify_dir_names(vec!["a/b", "a/b/c", "a/b/d/f"]), correct);
    }

    #[test]
    fn test_simplify_dir_duplicates() {
        let mut correct = HashSet::new();
        correct.insert(["a", "b"].iter().collect::<PathBuf>());
        correct.insert(PathBuf::from("c"));
        assert_eq!(
            simplify_dir_names(vec![
                "a/b",
                "a/b//",
                "a/././b///",
                "c",
                "c/",
                "c/.",
                "c/././",
                "c/././."
            ]),
            correct
        );
    }
    #[test]
    fn test_simplify_dir_rm_subdir_and_not_substrings() {
        let mut correct = HashSet::new();
        correct.insert(PathBuf::from("b"));
        correct.insert(["c", "a", "b"].iter().collect::<PathBuf>());
        correct.insert(["a", "b"].iter().collect::<PathBuf>());
        assert_eq!(simplify_dir_names(vec!["a/b", "c/a/b/", "b"]), correct);
    }

    #[test]
    fn test_simplify_dir_dots() {
        let mut correct = HashSet::new();
        correct.insert(PathBuf::from("src"));
        assert_eq!(simplify_dir_names(vec!["src/."]), correct);
    }

    #[test]
    fn test_simplify_dir_substring_names() {
        let mut correct = HashSet::new();
        correct.insert(PathBuf::from("src"));
        correct.insert(PathBuf::from("src_v2"));
        assert_eq!(simplify_dir_names(vec!["src/", "src_v2"]), correct);
    }
    #[test]
    fn test_is_a_parent_of() {
        assert!(is_a_parent_of("/usr", "/usr/andy"));
        assert!(is_a_parent_of("/usr", "/usr/andy/i/am/descendant"));
        assert!(!is_a_parent_of("/usr", "/usr/."));
        assert!(!is_a_parent_of("/usr", "/usr/"));
        assert!(!is_a_parent_of("/usr", "/usr"));
        assert!(!is_a_parent_of("/usr/", "/usr"));
        assert!(!is_a_parent_of("/usr/andy", "/usr"));
        assert!(!is_a_parent_of("/usr/andy", "/usr/sibling"));
        assert!(!is_a_parent_of("/usr/folder", "/usr/folder_not_a_child"));
    }

    #[test]
    fn test_is_a_parent_of_root() {
        assert!(is_a_parent_of("/", "/usr/andy"));
        assert!(is_a_parent_of("/", "/usr"));
        assert!(!is_a_parent_of("/", "/"));
    }
}
