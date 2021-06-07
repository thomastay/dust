#![allow(missing_docs)] // don't publish a crate with this!
#![warn(clippy::pedantic)]
#![deny(rust_2018_idioms)]
#![deny(clippy::too_many_arguments)]
#![deny(clippy::complexity)]
#![deny(clippy::perf)]
#![forbid(unsafe_code)]

use std::{cmp::max, path::PathBuf};

use clap::{crate_version, value_t, App, AppSettings, Arg};
use du_dust::{build_tree, display, find_big_ones, get_dir_tree, simplify_dir_names, sort};
use terminal_size::{terminal_size, Height, Width};

static DEFAULT_NUMBER_OF_LINES: usize = 30;
static DEFAULT_TERMINAL_WIDTH: usize = 80;

#[cfg(windows)]
fn init_color(no_color: bool) -> bool {
    // If no color is already set do not print a warning message
    if no_color {
        true
    } else {
        // Required for windows 10
        // Fails to resolve for windows 8 so disable color
        match ansi_term::enable_ansi_support() {
            Ok(_) => no_color,
            Err(_) => {
                eprintln!(
                    "This version of Windows does not support ANSI colors, setting no_color flag"
                );
                true
            }
        }
    }
}

#[cfg(not(windows))]
fn init_color(no_color: bool) -> bool {
    no_color
}

fn get_height_of_terminal() -> usize {
    // Windows CI runners detect a terminal height of 0
    if let Some((Width(_w), Height(h))) = terminal_size() {
        max(h as usize, DEFAULT_NUMBER_OF_LINES) - 10
    } else {
        DEFAULT_NUMBER_OF_LINES - 10
    }
}

fn get_width_of_terminal() -> usize {
    // Windows CI runners detect a very low terminal width
    if let Some((Width(w), Height(_h))) = terminal_size() {
        max(w as usize, DEFAULT_TERMINAL_WIDTH)
    } else {
        DEFAULT_TERMINAL_WIDTH
    }
}

#[allow(clippy::too_many_lines)]
fn main() {
    let default_height = get_height_of_terminal();
    let def_num_str = default_height.to_string();

    let options = App::new("Dust")
        .about("Like du but more intuitive")
        .version(crate_version!())
        .setting(AppSettings::TrailingVarArg)
        .arg(
            Arg::with_name("depth")
                .short("d")
                .long("depth")
                .help("Depth to show")
                .takes_value(true)
                .conflicts_with("number_of_lines"),
        )
        .arg(
            Arg::with_name("number_of_lines")
                .short("n")
                .long("number-of-lines")
                .help("Number of lines of output to show. This is Height, (but h is help)")
                .takes_value(true)
                .default_value(def_num_str.as_ref()),
        )
        .arg(
            Arg::with_name("display_full_paths")
                .short("p")
                .long("full-paths")
                .help("Subdirectories will not have their path shortened"),
        )
        .arg(
            Arg::with_name("ignore_directory")
                .short("X")
                .long("ignore-directory")
                .takes_value(true)
                .number_of_values(1)
                .multiple(true)
                .help("Exclude any file or directory with this name"),
        )
        .arg(
            Arg::with_name("limit_filesystem")
                .short("x")
                .long("limit-filesystem")
                .help("Only count the files and directories on the same filesystem as the supplied directory"),
        )
        .arg(
            Arg::with_name("display_apparent_size")
                .short("s")
                .long("apparent-size")
                .help("Use file length instead of blocks"),
        )
        .arg(
            Arg::with_name("reverse")
                .short("r")
                .long("reverse")
                .help("Print tree upside down (biggest highest)"),
        )
        .arg(
            Arg::with_name("no_colors")
                .short("c")
                .long("no-colors")
                .help("No colors will be printed (normally largest directories are colored)"),
        )
        .arg(
            Arg::with_name("no_bars")
                .short("b")
                .long("no-percent-bars")
                .help("No percent bars or percentages will be displayed"),
        )
        .arg(
            Arg::with_name("by_filecount")
                .short("f")
                .long("filecount")
                .help("Directory 'size' is number of child files/dirs not disk size"),
        )
        .arg(
            Arg::with_name("ignore_hidden")
                .short("i") // Do not use 'h' this is used by 'help'
                .long("ignore_hidden")
                .help("Obey .git_ignore rules & Do not display hidden files"),
        )
        .arg(
            Arg::with_name("width")
                .short("w")
                .long("terminal_width")
                .takes_value(true)
                .number_of_values(1)
                .help("Specify width of output overriding the auto detection of terminal width"),
        )
        .arg(Arg::with_name("inputs").multiple(true))
        .get_matches();

    let target_dirs = {
        match options.values_of("inputs") {
            None => vec!["."],
            Some(r) => r.collect(),
        }
    };

    let number_of_lines = value_t!(options, "number_of_lines", usize).unwrap_or_else(|e| {
        eprintln!(
            "{}Ignoring bad value for number_of_lines, and using default height {}",
            e, default_height
        );
        default_height
    });

    let terminal_width =
        value_t!(options, "width", usize).unwrap_or_else(|_| get_width_of_terminal());

    let depth = options.value_of("depth").and_then(|depth| {
        depth
            .parse::<usize>()
            .map(|v| v + 1)
            .map_err(|_| eprintln!("Ignoring bad value for depth"))
            .ok()
    });

    let no_colors = init_color(options.is_present("no_colors"));
    let use_apparent_size = options.is_present("display_apparent_size");
    let limit_filesystem = options.is_present("limit_filesystem");
    let ignore_directories = options
        .values_of("ignore_directory")
        .map(|i| i.map(PathBuf::from).collect());
    let by_filecount = options.is_present("by_filecount");
    let show_hidden = !options.is_present("ignore_hidden");

    // Run Dust.

    let simplified_dirs = simplify_dir_names(target_dirs);
    let (errors, nodes) = get_dir_tree(
        &simplified_dirs,
        &ignore_directories,
        &du_dust::DirTreeOpts {
            use_apparent_size,
            limit_filesystem,
            by_filecount,
            show_hidden,
        },
    );
    let sorted_data = sort(&nodes);
    let biggest_ones = {
        match depth {
            None => find_big_ones(sorted_data, number_of_lines),
            Some(_) => sorted_data,
        }
    };
    let root_node = build_tree(biggest_ones, depth);
    let use_full_path = options.is_present("display_full_paths");
    let is_reversed = !options.is_present("reverse");
    let no_percents = options.is_present("no_bars");

    display::draw_it(
        &errors,
        &root_node,
        &display::DrawOpts {
            use_full_path,
            is_reversed,
            no_colors,
            no_percents,
            terminal_width,
            by_filecount,
        },
    );
}
