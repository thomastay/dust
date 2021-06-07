//! # Reading and building the directory entries
//! These functions recurse through the file system, and create a HashMap of Nodes.

use crossbeam_channel::Sender;
use ignore::{ParallelVisitor, ParallelVisitorBuilder, WalkBuilder, WalkState};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::path::{self, Path};
use std::sync::atomic::{self, AtomicBool};
use std::thread;

use crate::{platform, PathData};

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

/// Gets the directory tree for a given path.
/// # Panics
/// Panics if txc fails to send, which shouldn't happen,
/// and also in case the threads fail to join, which shouldn't happen too.
pub fn get_dir_tree<P: AsRef<Path>>(
    top_level_names: &HashSet<P>,
    ignore_directories: &Option<Vec<PathBuf>>,
    opts: &DirTreeOpts,
) -> (Errors, HashMap<PathBuf, u64>) {
    // Create a channel, setup a receiver thread, then run walk_tree on the directory recursively in parallel,
    // then collect the results.
    let (tx, rx) = crossbeam_channel::bounded::<PathData>(1000);

    let permissions_flag = AtomicBool::new(false);
    let not_found_flag = AtomicBool::new(false);

    let t2 = top_level_names
        .iter()
        .map(|p| p.as_ref().to_path_buf())
        .collect();

    let reader_thread = create_reader_thread(rx, t2, opts.use_apparent_size);
    let walk_dir_builder =
        prepare_walk_dir_builder(top_level_names, opts.limit_filesystem, opts.show_hidden);

    walk_dir_builder.build_parallel().run(|| {
        let reader_channel = tx.clone();
        let permissions_flag = &permissions_flag;
        let not_found_flag = &not_found_flag;
        Box::new(move |path| {
            walk_tree(
                path,
                &reader_channel,
                ignore_directories,
                opts,
                permissions_flag,
                not_found_flag,
            )
        })
    });

    drop(tx);
    let data = reader_thread.join().unwrap();
    let errors = Errors {
        permissions: permissions_flag.load(atomic::Ordering::SeqCst),
        not_found: not_found_flag.load(atomic::Ordering::SeqCst),
    };
    (errors, data)
}

/// Creates a WalkBuilder from the options.
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

type WalkDirChannel = Sender<(Vec<crate::PathData>, Errors)>;

struct WalkDirBuilder<'s> {
    reader_channel: WalkDirChannel,
    opts: DirTreeOpts,
    ignore_directories: Option<&'s Vec<PathBuf>>,
}

impl<'s> ParallelVisitorBuilder<'s> for WalkDirBuilder<'s> {
    fn build(&mut self) -> Box<dyn ParallelVisitor + 's> {
        Box::new(WalkDirVisitor {
            reader_channel: self.reader_channel.clone(),
            ignore_directories: self.ignore_directories,
            opts: self.opts.clone(),
            results: Vec::new(),
            errors: Default::default(),
        })
    }
}

struct WalkDirVisitor<'a> {
    reader_channel: WalkDirChannel,
    ignore_directories: Option<&'a Vec<PathBuf>>,
    opts: DirTreeOpts,
    errors: Errors,
    results: Vec<crate::PathData>,
}

impl ParallelVisitor for WalkDirVisitor<'_> {
    fn visit(&mut self, entry: Result<ignore::DirEntry, ignore::Error>) -> WalkState {
        match entry {
            Ok(p) => {
                if let Some(dirs) = &self.ignore_directories {
                    let parts: Vec<path::Component<'_>> = p.path().components().collect();
                    if dirs.iter().any(|d| {
                        parts
                            .windows(d.components().count())
                            .any(|window| window.iter().collect::<PathBuf>() == *d)
                    }) {
                        return WalkState::Continue;
                    }
                }
                // At this point, the direntry we received is not ignored. So, send it to the receiver thread.
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
        let mut results = Vec::new();
        std::mem::swap(&mut results, &mut self.results);
        let errors = self.errors.clone();

        self.reader_channel.send((results, errors)).unwrap();
    }
}

/// Walks the directory tree. The inner implementation of the walker.
fn walk_tree(
    entry: Result<ignore::DirEntry, ignore::Error>,
    reader_channel: &crossbeam_channel::Sender<crate::PathData>,
    ignore_directories: &Option<Vec<PathBuf>>,
    opts: &DirTreeOpts,
    permissions_flag: &AtomicBool,
    not_found_flag: &AtomicBool,
) -> ignore::WalkState {
    match entry {
        Ok(p) => {
            if let Some(dirs) = ignore_directories {
                let parts: Vec<path::Component<'_>> = p.path().components().collect();
                if dirs.iter().any(|d| {
                    parts
                        .windows(d.components().count())
                        .any(|window| window.iter().collect::<PathBuf>() == *d)
                }) {
                    return WalkState::Continue;
                }
            }
            // At this point, the direntry we received is not ignored. So, send it to the receiver thread.
            match platform::get_metadata(&p, opts.use_apparent_size) {
                Some(data) => {
                    let inode = data.1;
                    let size = if opts.by_filecount { 1 } else { data.0 };
                    reader_channel.send((p.into_path(), size, inode)).unwrap();
                }
                None => {
                    // Since we cannot get the metadata, we must have faced a permissions error.
                    permissions_flag.store(true, atomic::Ordering::Relaxed);
                }
            }
        }
        Err(e) => {
            if is_not_found(&e) {
                not_found_flag.store(true, atomic::Ordering::Relaxed);
            } else {
                permissions_flag.store(true, atomic::Ordering::Relaxed);
            }
        }
    };
    WalkState::Continue
}

/// Creates the reader thread
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
