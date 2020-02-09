extern crate ansi_term;

use self::ansi_term::Colour::Fixed;
use self::ansi_term::Style;
use crate::utils::Node;
use terminal_size::{Width, Height, terminal_size};

use std::iter::repeat;
use std::path::Path;
use std::cmp::max;

static UNITS: [char; 4] = ['T', 'G', 'M', 'K'];

pub struct DisplayData {
    pub short_paths: bool,
    pub is_reversed: bool,
    pub colors_on: bool,
    pub terminal_size: Option<(Width, Height)>,
}

impl DisplayData {
    fn get_first_chars(&self) -> &str {
        if self.is_reversed {
            "─┴"
        } else {
            "─┬"
        }
    }

    #[allow(clippy::collapsible_if)]
    fn get_tree_chars(
        &self,
        num_siblings: u64,
        max_siblings: u64,
        has_children: bool,
    ) -> &'static str {
        if self.is_reversed {
            if num_siblings == max_siblings - 1 {
                if has_children {
                    "┌─┴"
                } else {
                    "┌──"
                }
            } else if has_children {
                "├─┴"
            } else {
                "├──"
            }
        } else {
            if num_siblings == 0 {
                if has_children {
                    "└─┬"
                } else {
                    "└──"
                }
            } else if has_children {
                "├─┬"
            } else {
                "├──"
            }
        }
    }

    fn is_biggest(&self, num_siblings: u64, max_siblings: u64) -> bool {
        if self.is_reversed {
            num_siblings == 0
        } else {
            num_siblings == max_siblings - 1
        }
    }

    fn get_children_from_node(&self, node: Node) -> impl Iterator<Item = Node> {
        if self.is_reversed {
            let n: Vec<Node> = node.children.into_iter().rev().map(|a| a).collect();
            n.into_iter()
        } else {
            node.children.into_iter()
        }
    }
}

pub fn draw_it(
    permissions: bool,
    use_full_path: bool,
    is_reversed: bool,
    no_colors: bool,
    root_node: Node,
) {
    if !permissions {
        eprintln!("Did not have permissions for all directories");
    }

    let display_data = DisplayData {
        short_paths: !use_full_path,
        is_reversed,
        colors_on: !no_colors,
        terminal_size: terminal_size(),
    };

    let longest_str_length = root_node.children.iter().map(|c| 
        find_longest_dir_name(c, &display_data.get_first_chars(), &display_data)
    ).max();

    let longest_str = 
        match longest_str_length {
            Some(l) => l,
            None => 40, //FIX ME
        };

    for c in display_data.get_children_from_node(root_node) {
        let first_tree_chars = display_data.get_first_chars();
        let base_size = c.size;
        display_node(c, true, first_tree_chars, &display_data, base_size, longest_str);
    }
}

fn find_longest_dir_name(node: &Node, indent: &str, display_data: &DisplayData) -> usize {
    let mut longest = get_printable_name(node.name.clone(), display_data, indent).chars().count();

    for c in node.children.iter() {
        // each tree drawing is 3 chars
        let full_indent :String = indent.to_string() + "   ";
        longest = max(longest, find_longest_dir_name(c, &*full_indent, display_data));
    }
    longest
}

// can we make 2 of these one for pre and post order traversal?
fn display_node(node: Node, is_biggest: bool, indent: &str, display_data: &DisplayData, base_size: u64, longest_string_length: usize) {
    let mut num_siblings = node.children.len() as u64;
    let max_sibling = num_siblings;
    let new_indent = clean_indentation_string(indent);
    let name = node.name.clone();
    let size = node.size;

    if !display_data.is_reversed {
        print_this_node(&name, size, is_biggest, display_data, indent, base_size, longest_string_length);
    }

    for c in display_data.get_children_from_node(node) {
        num_siblings -= 1;
        let chars = display_data.get_tree_chars(num_siblings, max_sibling, !c.children.is_empty());
        // can we just do is first ? + handle reverse mode. consider equal values
        let is_biggest = display_data.is_biggest(num_siblings, max_sibling);
        let full_indent = new_indent.clone() + chars;
        display_node(c, is_biggest, &*full_indent, display_data, base_size, longest_string_length);
    }

    if display_data.is_reversed {
        print_this_node(&name, size, is_biggest, display_data, indent, base_size, longest_string_length);
    }
}

fn clean_indentation_string(s: &str) -> String {
    let mut is: String = s.into();
    // For reversed:
    is = is.replace("┌─┴", "  ");
    is = is.replace("┌──", "  ");
    is = is.replace("├─┴", "│ ");
    is = is.replace("─┴", " ");
    // For normal
    is = is.replace("└─┬", "  ");
    is = is.replace("└──", "  ");
    is = is.replace("├─┬", "│ ");
    is = is.replace("─┬", " ");
    // For both
    is = is.replace("├──", "│ ");
    is
}

fn print_this_node<P: AsRef<Path>>(
    name: P,
    size: u64,
    is_biggest: bool,
    display_data: &DisplayData,
    indentation: &str,
    base_size: u64,
    longest_string_length: usize,
) {
    let pretty_size = format!("{:>5}", human_readable_number(size),);
    let percent_size = size as f32 / base_size as f32;
    println!(
        "{}",
        format_string(name, is_biggest, display_data, &*pretty_size, percent_size, indentation, longest_string_length)
    )
}

fn get_printable_name<P: AsRef<Path>>(
    dir_name: P,
    display_data: &DisplayData,
    indentation: &str,
) -> String {
    let dir_name = dir_name.as_ref();
    let printable_name = {
        if display_data.short_paths {
            match dir_name.parent() {
                Some(prefix) => match dir_name.strip_prefix(prefix) {
                    Ok(base) => base,
                    Err(_) => dir_name,
                },
                None => dir_name,
            }
        } else {
            dir_name
        }
    };
    format!("{} {}", indentation , printable_name.display())
}

pub fn format_string<P: AsRef<Path>>(
    dir_name: P,
    is_biggest: bool,
    display_data: &DisplayData,
    size: &str,
    percent_size: f32,
    indentation: &str,
    longest_string_length: usize,
) -> String {

    let percent_size_str = format!("{:.0}%", percent_size * 100.0);

    let dir_name = dir_name.as_ref();
    let tree_and_path = get_printable_name(dir_name, display_data, indentation);
     
    let ww = {
        if let Some((Width(w), Height(h))) = display_data.terminal_size {
            w
        } else {
            80
        }
    } - 16;

    let printable_chars = tree_and_path.chars().count();
    let tree_and_path = tree_and_path + &(repeat(" ").take(longest_string_length - printable_chars).collect::<String>());

    let max_bar_length = ww as usize - longest_string_length;
    let num_bars = (max_bar_length as f32 * percent_size) as usize;
    let num_spaces = max_bar_length - num_bars;
    let markers = (repeat(" ").take(num_spaces).collect::<String>()) + &(repeat("▇").take(num_bars).collect::<String>());
    
    format!(
        "{} {} | {} │ {:>4}",
        if is_biggest && display_data.colors_on {
            Fixed(196).paint(size)
        } else {
            Style::new().paint(size)
        },
        tree_and_path,
        markers,
        percent_size_str,
    )
}

fn human_readable_number(size: u64) -> String {
    for (i, u) in UNITS.iter().enumerate() {
        let marker = 1024u64.pow((UNITS.len() - i) as u32);
        if size >= marker {
            if size / marker < 10 {
                return format!("{:.1}{}", (size as f32 / marker as f32), u);
            } else {
                return format!("{}{}", (size / marker), u);
            }
        }
    }
    return format!("{}B", size);
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_human_readable_number() {
        assert_eq!(human_readable_number(1), "1B");
        assert_eq!(human_readable_number(956), "956B");
        assert_eq!(human_readable_number(1004), "1004B");
        assert_eq!(human_readable_number(1024), "1.0K");
        assert_eq!(human_readable_number(1536), "1.5K");
        assert_eq!(human_readable_number(1024 * 512), "512K");
        assert_eq!(human_readable_number(1024 * 1024), "1.0M");
        assert_eq!(human_readable_number(1024 * 1024 * 1024 - 1), "1023M");
        assert_eq!(human_readable_number(1024 * 1024 * 1024 * 20), "20G");
        assert_eq!(human_readable_number(1024 * 1024 * 1024 * 1024), "1.0T");
    }
}
