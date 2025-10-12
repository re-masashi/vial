//! Utilities for working with the HIR

use super::{BlockId, ControlFlowGraph, Function, Opcode};
use std::collections::{HashMap, HashSet};

/// A dominator tree for a function
#[derive(Debug)]
pub struct DominatorTree {
    dominators: HashMap<BlockId, HashSet<BlockId>>,
    idom: HashMap<BlockId, Option<BlockId>>, // Immediate dominator
    children: HashMap<BlockId, Vec<BlockId>>,
}

impl DominatorTree {
    /// Compute the dominator tree for a function
    pub fn new(cfg: &ControlFlowGraph) -> Self {
        let mut tree = DominatorTree {
            dominators: HashMap::new(),
            idom: HashMap::new(),
            children: HashMap::new(),
        };

        tree.compute(cfg);
        tree
    }

    /// Compute dominators using the Lengauer-Tarjan algorithm (simplified version)
    fn compute(&mut self, cfg: &ControlFlowGraph) {
        if let Some(entry) = cfg.get_entry_block() {
            // Initialize dominators: each block dominates itself
            for block in cfg.function().basic_blocks.iter() {
                let mut dom_set = HashSet::new();
                dom_set.insert(block.id);
                self.dominators.insert(block.id, dom_set);
            }

            // The entry block is dominated only by itself initially
            self.dominators.get_mut(&entry).unwrap().clear();
            self.dominators.get_mut(&entry).unwrap().insert(entry);

            // Iteratively compute dominators until fixed point
            let mut changed = true;
            while changed {
                changed = false;

                // Process blocks in reverse postorder (simplified as basic block order)
                for block in &cfg.function().basic_blocks {
                    if block.id == entry {
                        continue; // Entry block is special
                    }

                    // Get all predecessors
                    if let Some(preds) = cfg.get_predecessors(block.id)
                        && !preds.is_empty()
                    {
                        // Start with dominators of the first predecessor
                        let mut new_dom = self.dominators.get(&preds[0]).unwrap().clone();

                        // Intersect with dominators of other predecessors
                        for &pred in &preds[1..] {
                            if let Some(pred_dom) = self.dominators.get(&pred) {
                                new_dom = new_dom.intersection(pred_dom).cloned().collect();
                            }
                        }

                        // Add the block itself
                        new_dom.insert(block.id);

                        // If this changes the dominator set, update and continue
                        if new_dom != *self.dominators.get(&block.id).unwrap() {
                            self.dominators.insert(block.id, new_dom);
                            changed = true;
                        }
                    }
                }
            }

            // Compute immediate dominators
            for block in &cfg.function().basic_blocks {
                if block.id == entry {
                    self.idom.insert(block.id, None);
                } else {
                    // Find the immediate dominator (dominator closest to the block)
                    let doms = self.dominators.get(&block.id).unwrap();
                    let mut idom = None;
                    for &dom in doms {
                        if dom != block.id && idom.is_none()
                            || self.is_dominated_by(cfg, &dom, idom.unwrap())
                        {
                            idom = Some(dom);
                        }
                    }
                    self.idom.insert(block.id, idom);

                    // Add to children of immediate dominator
                    if let Some(idom_block) = idom {
                        self.children.entry(idom_block).or_default().push(block.id);
                    }
                }
            }
        }
    }

    /// Check if block a is dominated by block b
    fn is_dominated_by(&self, _cfg: &ControlFlowGraph, a: &BlockId, b: BlockId) -> bool {
        if let Some(doms) = self.dominators.get(a) {
            doms.contains(&b)
        } else {
            false
        }
    }

    /// Get the immediate dominator of a block
    pub fn immediate_dominator(&self, block: BlockId) -> Option<BlockId> {
        *self.idom.get(&block).unwrap_or(&None)
    }

    /// Get the children of a block in the dominator tree
    pub fn children(&self, block: BlockId) -> &[BlockId] {
        self.children
            .get(&block)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Check if one block dominates another
    pub fn dominates(&self, dominator: BlockId, dominated: BlockId) -> bool {
        if dominator == dominated {
            return true;
        }

        if let Some(doms) = self.dominators.get(&dominated) {
            doms.contains(&dominator)
        } else {
            false
        }
    }
}

/// A simple constant folding utility
pub struct ConstantFolder;

impl ConstantFolder {
    /// Try to fold a binary operation with constant operands
    pub fn fold_binary_op(opcode: Opcode, left: i128, right: i128) -> Option<i128> {
        match opcode {
            Opcode::Add => Some(left.wrapping_add(right)),
            Opcode::Sub => Some(left.wrapping_sub(right)),
            Opcode::Mul => Some(left.wrapping_mul(right)),
            Opcode::Div if right != 0 => Some(left.wrapping_div(right)),
            Opcode::Rem if right != 0 => Some(left.wrapping_rem(right)),
            Opcode::BitAnd => Some(left & right),
            Opcode::BitOr => Some(left | right),
            Opcode::BitXor => Some(left ^ right),
            Opcode::Shl => Some(left.wrapping_shl(right as u32)),
            Opcode::Shr => Some(left.wrapping_shr(right as u32)),
            Opcode::Eq => Some(if left == right { 1 } else { 0 }),
            Opcode::Ne => Some(if left != right { 1 } else { 0 }),
            Opcode::Lt => Some(if left < right { 1 } else { 0 }),
            Opcode::Gt => Some(if left > right { 1 } else { 0 }),
            Opcode::Le => Some(if left <= right { 1 } else { 0 }),
            Opcode::Ge => Some(if left >= right { 1 } else { 0 }),
            _ => None,
        }
    }
}

/// A utility for inlining simple functions
pub struct Inliner;

impl Inliner {
    /// Check if a function is a good candidate for inlining
    pub fn is_inline_candidate(func: &Function) -> bool {
        // Simple heuristic: functions with few instructions are good candidates
        let total_instructions: usize = func
            .basic_blocks
            .iter()
            .map(|block| block.instructions.len())
            .sum();

        // Inline if function has 5 or fewer instructions
        total_instructions <= 5
    }
}
