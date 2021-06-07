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
    path::{Component, Path, PathBuf},
    sync::atomic::{self, AtomicBool},
    thread,
};

use ignore::{WalkBuilder, WalkState};

pub mod display;
mod platform;

type PathData = (PathBuf, u64, Option<(u64, u64)>);

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

pub fn recursively_build_tree(parent_node: &mut Node, new_node: Node, depth: Option<usize>) {
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

#[derive(Debug, Default, Eq, Clone)]
pub struct Node {
    pub name: PathBuf,
    pub size: u64,
    pub children: Vec<Node>,
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        if self.size == other.size {
            self.name.cmp(&other.name)
        } else {
            self.size.cmp(&other.size)
        }
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.size == other.size && self.children == other.children
    }
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

pub struct Errors {
    pub permissions: bool,
    pub not_found: bool,
}

pub fn is_a_parent_of<P: AsRef<Path>>(parent: P, child: P) -> bool {
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

fn prepare_walk_dir_builder<P: AsRef<Path>>(
    top_level_names: &HashSet<P>,
    limit_filesystem: bool,
    show_hidden: bool,
) -> WalkBuilder {
    let mut it = top_level_names.iter();
    let mut builder = WalkBuilder::new(it.next().unwrap());
    builder.follow_links(false);
    if show_hidden {
        builder.hidden(false);
        builder.ignore(false);
        builder.git_global(false);
        builder.git_ignore(false);
        builder.git_exclude(false);
    }

    if limit_filesystem {
        builder.same_file_system(true);
    }

    for b in it {
        builder.add(b);
    }
    builder
}

fn is_not_found(e: &ignore::Error) -> bool {
    use ignore::Error;
    if let Error::WithPath { err, .. } = e {
        if let Error::Io(e) = &**err {
            if e.kind() == std::io::ErrorKind::NotFound {
                return true;
            }
        }
    }
    false
}

/// Opts for get_dir_tree
pub struct DirTreeOpts {
    pub use_apparent_size: bool,
    pub limit_filesystem: bool,
    pub by_filecount: bool,
    pub show_hidden: bool,
}

/// Gets the directory tree for a given path.
/// # Panics
/// Panics if txc fails to send, which shouldn't happen,
/// and also in case the threads fail to join, which shouldn't happen too.
pub fn get_dir_tree<P: AsRef<Path>>(
    top_level_names: &HashSet<P>,
    ignore_directories: &Option<Vec<PathBuf>>,
    opts: &DirTreeOpts,
) -> (Errors, HashMap<PathBuf, u64>) {
    let (tx, rx) = crossbeam_channel::bounded::<PathData>(1000);

    let permissions_flag = AtomicBool::new(false);
    let not_found_flag = AtomicBool::new(false);

    let t2 = top_level_names
        .iter()
        .map(|p| p.as_ref().to_path_buf())
        .collect();

    let t = create_reader_thread(rx, t2, opts.use_apparent_size);
    let walk_dir_builder =
        prepare_walk_dir_builder(top_level_names, opts.limit_filesystem, opts.show_hidden);

    walk_dir_builder.build_parallel().run(|| {
        let txc = tx.clone();
        let pf = &permissions_flag;
        let nf = &not_found_flag;
        Box::new(move |path| {
            match path {
                Ok(p) => {
                    if let Some(dirs) = ignore_directories {
                        let path = p.path();
                        let parts = path.components().collect::<Vec<Component<'_>>>();
                        for d in dirs {
                            if parts
                                .windows(d.components().count())
                                .any(|window| window.iter().collect::<PathBuf>() == *d)
                            {
                                return WalkState::Continue;
                            }
                        }
                    }

                    let maybe_size_and_inode = platform::get_metadata(&p, opts.use_apparent_size);

                    match maybe_size_and_inode {
                        Some(data) => {
                            let (size, inode_device) =
                                if opts.by_filecount { (1, data.1) } else { data };
                            txc.send((p.into_path(), size, inode_device)).unwrap();
                        }
                        None => {
                            pf.store(true, atomic::Ordering::Relaxed);
                        }
                    }
                }
                Err(e) => {
                    if is_not_found(&e) {
                        nf.store(true, atomic::Ordering::Relaxed);
                    } else {
                        pf.store(true, atomic::Ordering::Relaxed);
                    }
                }
            };
            WalkState::Continue
        })
    });

    drop(tx);
    let data = t.join().unwrap();
    let errors = Errors {
        permissions: permissions_flag.load(atomic::Ordering::SeqCst),
        not_found: not_found_flag.load(atomic::Ordering::SeqCst),
    };
    (errors, data)
}

fn create_reader_thread(
    rx: crossbeam_channel::Receiver<PathData>,
    top_level_names: HashSet<PathBuf>,
    use_apparent_size: bool,
) -> thread::JoinHandle<HashMap<PathBuf, u64>> {
    // Receiver thread
    thread::spawn(move || {
        let mut hash: HashMap<PathBuf, u64> = HashMap::new();
        let mut inodes: HashSet<(u64, u64)> = HashSet::new();

        for dent in rx {
            let (path, size, maybe_inode_device) = dent;

            if should_ignore_file(use_apparent_size, &mut inodes, maybe_inode_device) {
                continue;
            }
            for p in path.ancestors() {
                let s = hash.entry(p.to_path_buf()).or_insert(0);
                *s += size;

                if top_level_names.contains(p) {
                    break;
                }
            }
        }
        hash
    })
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

fn should_ignore_file(
    use_apparent_size: bool,
    inodes: &mut HashSet<(u64, u64)>,
    maybe_inode_device: Option<(u64, u64)>,
) -> bool {
    match maybe_inode_device {
        None => false,
        Some(data) => {
            let (inode, device) = data;
            if !use_apparent_size {
                // Ignore files already visited or symlinked
                if inodes.contains(&(inode, device)) {
                    return true;
                }
                inodes.insert((inode, device));
            }
            false
        }
    }
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

    #[test]
    fn test_should_ignore_file() {
        let mut files = HashSet::new();
        files.insert((10, 20));

        assert!(!should_ignore_file(true, &mut files, Some((0, 0))));

        // New file is not known it will be inserted to the hashmp and should not be ignored
        assert!(!should_ignore_file(false, &mut files, Some((11, 12))));
        assert!(files.contains(&(11, 12)));

        // The same file will be ignored the second time
        assert!(should_ignore_file(false, &mut files, Some((11, 12))));
    }

    #[test]
    fn test_should_ignore_file_on_different_device() {
        let mut files = HashSet::new();
        files.insert((10, 20));

        // We do not ignore files on the same device
        assert!(!should_ignore_file(false, &mut files, Some((2, 99))));
        assert!(!should_ignore_file(true, &mut files, Some((2, 99))));
    }
}
