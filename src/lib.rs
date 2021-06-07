#![allow(missing_docs)] // don't publish a crate with this!
#![deny(rust_2018_idioms)]
#![deny(clippy::too_many_arguments)]
#![deny(clippy::complexity)]
#![deny(clippy::perf)]
#![forbid(unsafe_code)]
// select lints from clippy::pedantic
#![deny(clippy::needless_pass_by_value)]
#![deny(clippy::explicit_iter_loop)]
#![deny(clippy::implicit_clone)]
#![deny(clippy::redundant_else)]
#![deny(clippy::missing_panics_doc)]
#![deny(clippy::fn_params_excessive_bools)]

use std::{
    cmp,
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

pub mod display;
mod platform;
pub mod walk_dirs;

type PathData = (PathBuf, u64, Option<platform::INode>);

// ======================= Given the HashMap of Dir Entries, build a tree out of it ===================

pub fn build_tree(biggest_ones: Vec<(PathBuf, u64)>, depth: Option<usize>) -> Node {
    let mut top_parent = Node::default();

    // assume sorted order
    for b in biggest_ones {
        let n = Node {
            name: b.0,
            size: b.1,
            children: Vec::default(),
        };
        recursively_build_tree(&mut top_parent, n, depth);
    }
    top_parent
}

fn recursively_build_tree(parent_node: &mut Node, new_node: Node, depth: Option<usize>) {
    let new_depth = match depth {
        None => None,
        Some(0) => return,
        Some(d) => Some(d - 1),
    };
    if let Some(c) = parent_node
        .children
        .iter_mut()
        .find(|c| is_a_parent_of(&c.name, &new_node.name))
    {
        recursively_build_tree(c, new_node, new_depth);
    } else {
        parent_node.children.push(new_node);
    }
}

#[derive(Debug, Default, Clone)]
pub struct Node {
    pub size: u64,
    pub name: PathBuf,
    pub children: Vec<Node>,
}

impl Node {
    pub fn num_siblings(&self) -> u64 {
        self.children.len() as u64
    }

    pub fn get_children_from_node(&self, is_reversed: bool) -> impl Iterator<Item = Node> {
        if is_reversed {
            let children: Vec<Node> = self.children.clone().into_iter().rev().collect();
            children.into_iter()
        } else {
            self.children.clone().into_iter()
        }
    }
}

fn is_a_parent_of<P: AsRef<Path>>(parent: P, child: P) -> bool {
    let parent = parent.as_ref();
    let child = child.as_ref();
    child.starts_with(parent) && !parent.starts_with(child)
}

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

pub fn normalize_path<P: AsRef<Path>>(path: P) -> PathBuf {
    // normalize path ...
    // 1. removing repeated separators
    // 2. removing interior '.' ("current directory") path segments
    // 3. removing trailing extra separators and '.' ("current directory") path segments
    // * `Path.components()` does all the above work; ref: <https://doc.rust-lang.org/std/path/struct.Path.html#method.components>
    // 4. changing to os preferred separator (automatically done by recollecting components back into a PathBuf)
    path.as_ref().components().collect::<PathBuf>()
}

pub fn sort_by_size_first_name_second(a: &(PathBuf, u64), b: &(PathBuf, u64)) -> cmp::Ordering {
    let result = b.1.cmp(&a.1);
    if result == cmp::Ordering::Equal {
        a.0.cmp(&b.0)
    } else {
        result
    }
}

pub fn sort(data: &HashMap<PathBuf, u64>) -> Vec<(PathBuf, u64)> {
    let mut new_l: Vec<(PathBuf, u64)> = data.iter().map(|(a, b)| (a.clone(), *b)).collect();
    new_l.sort_unstable_by(sort_by_size_first_name_second);
    new_l
}

pub fn find_big_ones(new_l: Vec<(PathBuf, u64)>, max_to_show: usize) -> Vec<(PathBuf, u64)> {
    if max_to_show > 0 && new_l.len() > max_to_show {
        new_l[0..max_to_show].to_vec()
    } else {
        new_l
    }
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
