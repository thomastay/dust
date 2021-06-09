//! # Reading and building the directory entries
//! These functions recurse through the file system, and create a HashMap of Nodes.

use ignore::{DirEntry, ParallelVisitor, ParallelVisitorBuilder, WalkBuilder, WalkState};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Mutex;

use crate::platform;
type PathData = (PathBuf, u64, Option<platform::INode>);

/// Opts for get_dir_tree
#[derive(Clone)]
pub struct DirTreeOpts {
    /// Use file length instead of blocks
    pub use_apparent_size: bool,
    /// Only count the files and directories on the same filesystem as the supplied directory
    pub limit_filesystem: bool,
    /// Directory 'size' is number of child files/dirs not disk size
    pub by_filecount: bool,
    /// Ignore .gitignore rules and display hidden files
    pub show_hidden: bool,
}

#[derive(Default, Clone)]
pub struct Errors {
    pub permissions: bool,
    pub not_found: bool,
}

type WalkDirChannelData = (Vec<PathData>, Errors);
/// Gets the directory tree for a given path.
/// # Panics
/// Panics if the mutex is poisoned.
pub fn get_dir_tree(
    top_level_names: &HashSet<PathBuf>,
    ignore_directories: Option<&Vec<PathBuf>>,
    opts: &DirTreeOpts,
) -> (HashMap<PathBuf, u64>, Errors) {
    let final_results: Mutex<Vec<WalkDirChannelData>> = Mutex::new(Vec::new());
    let walk_builder = prepare_walk_builder(top_level_names, opts);

    walk_builder.build_parallel().visit(&mut FnBuilder {
        builder: || WalkDirVisitor {
            final_results: &final_results,
            ignore_directories,
            opts,
            results: Vec::new(),
            errors: Default::default(),
        },
    });

    let final_results = final_results.lock().unwrap();
    handle_results(&final_results, top_level_names, opts.use_apparent_size)
}

/// Creates a WalkBuilder from the options.
fn prepare_walk_builder(top_level_names: &HashSet<PathBuf>, opts: &DirTreeOpts) -> WalkBuilder {
    let mut it = top_level_names.iter();
    let mut builder = WalkBuilder::new(it.next().unwrap());
    builder.follow_links(false);
    if opts.show_hidden {
        builder.hidden(false);
        builder.ignore(false);
        builder.git_global(false);
        builder.git_ignore(false);
        builder.git_exclude(false);
    }

    if opts.limit_filesystem {
        builder.same_file_system(true);
    }

    for b in it {
        builder.add(b);
    }
    builder
}

/// The following FnBuilder and WalkDirVisitor are to satisfy the visit API.
/// FnBuilder and its implementation are taken from ignore's crate.
///
/// FnBuilder is useless here, it exists solely to satisfy the API, and just returns the WalkDirVisitor,
/// which is the main implementation.
struct FnBuilder<F> {
    builder: F,
}

// don't worry too much about this signature, it just says that F is a closure returning a WalkDirVisitor
impl<'s, F: FnMut() -> WalkDirVisitor<'s>> ParallelVisitorBuilder<'s> for FnBuilder<F> {
    fn build(&mut self) -> Box<dyn ParallelVisitor + 's> {
        Box::new((self.builder)())
    }
}

struct WalkDirVisitor<'s> {
    final_results: &'s Mutex<Vec<WalkDirChannelData>>,
    ignore_directories: Option<&'s Vec<PathBuf>>,
    opts: &'s DirTreeOpts,
    errors: Errors,
    results: Vec<PathData>,
}

impl ParallelVisitor for WalkDirVisitor<'_> {
    fn visit(&mut self, entry: Result<DirEntry, ignore::Error>) -> WalkState {
        match entry {
            Ok(p) => {
                if let Some(dirs) = self.ignore_directories {
                    let parts = p.path().components().collect::<Vec<_>>();
                    if dirs.iter().any(|d| {
                        parts
                            .windows(d.components().count())
                            .any(|window| window.iter().collect::<PathBuf>() == *d)
                    }) {
                        return WalkState::Continue;
                    }
                }
                // At this point, the direntry we received is not ignored. So, push it into the results vec
                match platform::get_metadata(&p, self.opts.use_apparent_size) {
                    Some(data) => {
                        let inode = data.1;
                        let size = if self.opts.by_filecount { 1 } else { data.0 };
                        self.results.push((p.into_path(), size, inode));
                    }
                    None => {
                        // Since we cannot get the metadata, we must have faced a permissions error.
                        self.errors.permissions = true;
                    }
                }
            }
            Err(e) => {
                if is_not_found(&e) {
                    self.errors.not_found = true;
                } else {
                    self.errors.permissions = true;
                }
            }
        };
        WalkState::Continue
    }
}

impl<'a> Drop for WalkDirVisitor<'a> {
    fn drop(&mut self) {
        // Move results out of self. Safe, since this is in a drop.
        let results = std::mem::take(&mut self.results);
        let errors = self.errors.clone();
        let to_push = (results, errors);

        // push the collated results into the global final_results vec.
        let mut final_results = self.final_results.lock().unwrap();
        (*final_results).push(to_push);
    }
}

/// Reads from the channel and create a hashmap
fn handle_results(
    final_results: &[WalkDirChannelData],
    top_level_names: &HashSet<PathBuf>,
    use_apparent_size: bool,
) -> (HashMap<PathBuf, u64>, Errors) {
    let mut hash: HashMap<PathBuf, u64> = HashMap::new();
    let mut inodes: HashSet<(u64, u64)> = HashSet::new();
    let mut errors: Errors = Default::default();

    for (dents, errors_rx) in final_results {
        // merge errors
        if errors_rx.not_found {
            errors.not_found = true;
        }
        if errors_rx.permissions {
            errors.permissions = true;
        }
        // Merge paths
        for (path, size, maybe_inode_device) in dents {
            if should_ignore_file(use_apparent_size, &mut inodes, maybe_inode_device) {
                continue;
            }
            for p in path.ancestors() {
                let curr_size = hash.entry(p.to_path_buf()).or_insert(0);
                *curr_size += size;

                if top_level_names.contains(p) {
                    break; // why the break??
                }
            }
        }
    }
    (hash, errors)
}

fn should_ignore_file(
    use_apparent_size: bool,
    inodes: &mut HashSet<(u64, u64)>,
    maybe_inode_device: &Option<(u64, u64)>,
) -> bool {
    // if use_apparent_size is true, we always return false.
    !use_apparent_size
        && match maybe_inode_device {
            None => false,
            Some(inode_device) => {
                // Ignore files already visited or symlinked
                if inodes.contains(inode_device) {
                    true
                } else {
                    inodes.insert(*inode_device);
                    false
                }
            }
        }
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

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_should_ignore_file() {
        let mut files = HashSet::new();
        files.insert((10, 20));

        assert!(!should_ignore_file(true, &mut files, &Some((0, 0))));

        // New file is not known it will be inserted to the hashmp and should not be ignored
        assert!(!should_ignore_file(false, &mut files, &Some((11, 12))));
        assert!(files.contains(&(11, 12)));

        // The same file will be ignored the second time
        assert!(should_ignore_file(false, &mut files, &Some((11, 12))));
    }

    #[test]
    fn test_should_ignore_file_on_different_device() {
        let mut files = HashSet::new();
        files.insert((10, 20));

        // We do not ignore files on the same device
        assert!(!should_ignore_file(false, &mut files, &Some((2, 99))));
        assert!(!should_ignore_file(true, &mut files, &Some((2, 99))));
    }
}
