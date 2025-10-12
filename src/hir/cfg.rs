//! Control Flow Graph representation for the HIR

use super::{BlockId, Function, Terminator};
use std::collections::{HashMap, HashSet};

/// A Control Flow Graph (CFG) for a function
#[derive(Debug)]
pub struct ControlFlowGraph {
    /// The function this CFG represents
    function: Function,
    /// Predecessors of each block
    preds: HashMap<BlockId, Vec<BlockId>>,
    /// Successors of each block
    succs: HashMap<BlockId, Vec<BlockId>>,
}

impl ControlFlowGraph {
    /// Create a new CFG from a function
    pub fn new(function: Function) -> Self {
        let mut cfg = ControlFlowGraph {
            function,
            preds: HashMap::new(),
            succs: HashMap::new(),
        };

        cfg.compute_edges();
        cfg
    }

    /// Compute the predecessor and successor relationships
    fn compute_edges(&mut self) {
        // Initialize empty lists for all blocks
        for block in &self.function.basic_blocks {
            self.preds.insert(block.id, Vec::new());
            self.succs.insert(block.id, Vec::new());
        }

        // For each block, examine its terminator to find successors
        for block in &self.function.basic_blocks {
            let successors = self.get_terminator_successors(&block.terminator);

            for &succ_id in &successors {
                // Add successor relationship
                self.succs.get_mut(&block.id).unwrap().push(succ_id);
                // Add predecessor relationship
                if let Some(pred_list) = self.preds.get_mut(&succ_id)
                    && !pred_list.contains(&block.id)
                {
                    pred_list.push(block.id);
                }
            }
        }
    }

    /// Get the successor blocks of a terminator instruction
    fn get_terminator_successors(&self, terminator: &Terminator) -> Vec<BlockId> {
        match terminator {
            Terminator::Jump { target, .. } => vec![*target],
            Terminator::Branch {
                then_block,
                else_block,
                ..
            } => vec![*then_block, *else_block],
            Terminator::Switch {
                targets, default, ..
            } => {
                let mut succs: Vec<BlockId> = targets.iter().map(|(_, block)| *block).collect();
                succs.push(*default);
                succs
            }
            Terminator::Return { .. } | Terminator::Unreachable => vec![],
        }
    }

    /// Get the predecessors of a block
    pub fn get_predecessors(&self, block_id: BlockId) -> Option<&[BlockId]> {
        self.preds.get(&block_id).map(|v| v.as_slice())
    }

    /// Get the successors of a block
    pub fn get_successors(&self, block_id: BlockId) -> Option<&[BlockId]> {
        self.succs.get(&block_id).map(|v| v.as_slice())
    }

    /// Get the entry block of the function (the first block)
    pub fn get_entry_block(&self) -> Option<BlockId> {
        self.function.basic_blocks.first().map(|b| b.id)
    }

    /// Perform a depth-first traversal of the CFG
    pub fn dfs_traverse<F>(&self, start: BlockId, mut visitor: F)
    where
        F: FnMut(BlockId) -> bool, // Return true to continue, false to stop
    {
        let mut visited = HashSet::new();
        let mut stack = vec![start];

        while let Some(block_id) = stack.pop() {
            if visited.contains(&block_id) {
                continue;
            }

            visited.insert(block_id);

            if !visitor(block_id) {
                break;
            }

            if let Some(successors) = self.get_successors(block_id) {
                for &succ in successors {
                    if !visited.contains(&succ) {
                        stack.push(succ);
                    }
                }
            }
        }
    }

    /// Get all blocks reachable from the entry block
    pub fn get_reachable_blocks(&self) -> HashSet<BlockId> {
        let mut reachable = HashSet::new();
        if let Some(entry) = self.get_entry_block() {
            self.dfs_traverse(entry, |block_id| {
                reachable.insert(block_id);
                true
            });
        }
        reachable
    }

    /// Check if the CFG is well-formed (all referenced blocks exist)
    pub fn is_well_formed(&self) -> bool {
        for block in &self.function.basic_blocks {
            let successors = self.get_terminator_successors(&block.terminator);
            for succ_id in successors {
                if !self.function.basic_blocks.iter().any(|b| b.id == succ_id) {
                    return false; // Successor block doesn't exist
                }
            }
        }

        // Check that all predecessors exist as blocks
        for (block_id, preds) in &self.preds {
            if !self.function.basic_blocks.iter().any(|b| b.id == *block_id) {
                return false; // Block doesn't exist
            }
            for pred_id in preds {
                if !self.function.basic_blocks.iter().any(|b| b.id == *pred_id) {
                    return false; // Predecessor doesn't exist
                }
            }
        }

        true
    }

    /// Get the underlying function
    pub fn into_function(self) -> Function {
        self.function
    }

    /// Get a reference to the underlying function
    pub fn function(&self) -> &Function {
        &self.function
    }

    /// Get a mutable reference to the underlying function
    pub fn function_mut(&mut self) -> &mut Function {
        &mut self.function
    }
}
