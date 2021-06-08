//! # Given the HashMap of Dir Entries, build a tree out of it
use std::path::PathBuf;

pub fn build(biggest_ones: Vec<(PathBuf, u64)>, depth: Option<usize>) -> Node {
    fn rec(parent_node: &mut Node, new_node: Node, depth: Option<usize>) {
        let new_depth = match depth {
            None => None,
            Some(0) => return,
            Some(d) => Some(d - 1),
        };
        if let Some(c) = parent_node
            .children
            .iter_mut()
            .find(|c| crate::is_a_parent_of(&c.name, &new_node.name))
        {
            rec(c, new_node, new_depth);
        } else {
            parent_node.children.push(new_node);
        }
    }

    let mut top_parent = Node::default();

    // assume sorted order
    for b in biggest_ones {
        let n = Node {
            name: b.0,
            size: b.1,
            children: Vec::default(),
        };
        rec(&mut top_parent, n, depth);
    }
    top_parent
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
