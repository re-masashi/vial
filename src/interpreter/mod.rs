pub mod builtins;
pub mod error;
pub mod heap;
mod logic;
pub mod stack;
pub mod value;

pub use logic::Interpreter;
