pub mod ast;
pub mod desugar;
pub mod lexer;
pub mod parser;
pub mod typechecker;
pub mod validation; // IDK SOME STUFF NEEDS TO BE IN THE VALIDATOR SOME HERE
// I CANT DECIDE WHICH GOES WHERE
pub mod ir;
pub mod vm;