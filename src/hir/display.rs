//! Display implementations for HIR with LLVM-like formatting

use super::{
    BasicBlock, BlockId, Constant, FloatType, Function, Instruction, IntType, Opcode, Terminator,
    Type, TypeContext, TypeId, Value, ValueId,
};
use std::fmt;

/// Helper struct to format HIR with type context
pub struct HirDisplay<'a> {
    pub function: &'a Function,
    pub type_context: Option<&'a TypeContext>,
}

impl<'a> fmt::Display for HirDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print function signature
        write!(f, "define ")?;
        if let Some(type_context) = self.type_context {
            write!(
                f,
                "{} ",
                type_to_ir(type_context, self.function.signature.return_type)
            )?;
        } else {
            write!(f, "<?> ")?;
        }

        write!(f, "@{}(", self.function.name)?;

        // Print parameters
        let params: Vec<String> = self
            .function
            .signature
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let type_str = if let Some(type_context) = self.type_context {
                    type_to_ir(type_context, param.ty)
                } else {
                    String::from("<?>")
                };
                let param_name = format!("%{}", i);
                let name = param.name.as_deref().unwrap_or(param_name.as_str());
                format!("{} {}", type_str, name)
            })
            .collect();

        write!(f, "{}", params.join(", "))?;
        writeln!(f, ") {{")?;

        // Print basic blocks
        for block in &self.function.basic_blocks {
            write!(f, "{}:", block_label(block.id))?;

            // Print phi nodes first
            if !block.phi_nodes.is_empty() {
                writeln!(f)?;
                for phi_node in &block.phi_nodes {
                    write!(f, "  {} = phi ", value_label(phi_node.result))?;
                    if let Some(type_context) = self.type_context {
                        write!(f, "{}", type_to_ir(type_context, phi_node.ty))?;
                    } else {
                        write!(f, "<?>")?;
                    }

                    let incomings: Vec<String> = phi_node
                        .incomings
                        .iter()
                        .map(|(value_id, block_id)| {
                            format!("[ {}, {} ]", value_label(*value_id), block_label(*block_id))
                        })
                        .collect();

                    writeln!(f, " {}", incomings.join(", "))?;
                }
            }

            // Print regular instructions
            for &inst_idx in &block.instructions {
                if let Some(instruction) = self.function.instructions.get(inst_idx) {
                    write!(f, "  {} = ", value_label(ValueId(inst_idx)))?;
                    write_instruction(f, instruction, self.type_context)?;
                    writeln!(f)?;
                }
            }

            // Print terminator
            write!(f, "  ")?;
            write_terminator(f, &block.terminator, self.type_context)?;
            writeln!(f)?;
        }

        writeln!(f, "}}")
    }
}

/// Format a basic block label
fn block_label(block_id: BlockId) -> String {
    format!("bb{}", block_id.0)
}

/// Format a value label
fn value_label(value_id: ValueId) -> String {
    format!("%{}", value_id.0)
}

/// Write an instruction in LLVM-like format
fn write_instruction(
    f: &mut fmt::Formatter<'_>,
    instruction: &Instruction,
    type_context: Option<&TypeContext>,
) -> fmt::Result {
    // Write the opcode
    write!(f, "{} ", opcode_to_ir(&instruction.opcode))?;

    // Write the type if available
    if let Some(type_context) = type_context {
        write!(f, "{} ", type_to_ir(type_context, instruction.ty))?;
    } else {
        write!(f, "<?> ")?;
    }

    // Write the arguments
    let args: Vec<String> = instruction
        .args
        .iter()
        .map(|&arg_id| value_label(arg_id))
        .collect();

    write!(f, "{}", args.join(", "))
}

/// Write a terminator instruction in LLVM-like format
fn write_terminator(
    f: &mut fmt::Formatter<'_>,
    terminator: &Terminator,
    type_context: Option<&TypeContext>,
) -> fmt::Result {
    match terminator {
        Terminator::Jump { target, args } => {
            write!(f, "br label {}", block_label(*target))?;
            if !args.is_empty() {
                let arg_strs: Vec<String> = args
                    .iter()
                    .map(|&arg_id| {
                        if let Some(_type_context) = type_context {
                            // We'd need to look up the value to get its type, but for now just show the value
                            value_label(arg_id).to_string()
                        } else {
                            value_label(arg_id).to_string()
                        }
                    })
                    .collect();
                write!(f, ", args {}", arg_strs.join(", "))?;
            }
        }
        Terminator::Branch {
            condition,
            then_block,
            else_block,
            then_args,
            else_args,
        } => {
            write!(
                f,
                "br {}, {}, {}",
                value_label(*condition),
                block_label(*then_block),
                block_label(*else_block)
            )?;

            if !then_args.is_empty() || !else_args.is_empty() {
                // For simplicity in this format, we'll note the args but not include them in the br instruction
                if !then_args.is_empty() {
                    let then_arg_strs: Vec<String> = then_args
                        .iter()
                        .map(|&arg_id| value_label(arg_id))
                        .collect();
                    write!(f, " // then_args: {}", then_arg_strs.join(", "))?;
                }
                if !else_args.is_empty() {
                    let else_arg_strs: Vec<String> = else_args
                        .iter()
                        .map(|&arg_id| value_label(arg_id))
                        .collect();
                    write!(f, " // else_args: {}", else_arg_strs.join(", "))?;
                }
            }
        }
        Terminator::Switch {
            value,
            targets,
            default,
        } => {
            write!(
                f,
                "switch {}, {}, [",
                value_label(*value),
                block_label(*default)
            )?;
            let target_strs: Vec<String> = targets
                .iter()
                .map(|(val, block_id)| format!("{} -> {}", val, block_label(*block_id)))
                .collect();
            write!(f, "{}", target_strs.join(", "))?;
            write!(f, "]")?;
        }
        Terminator::Return { value } => {
            match value {
                Some(val_id) => {
                    if let Some(_type_context) = type_context {
                        // Look up the value to get its type for proper formatting
                        write!(f, "ret <?> {}", value_label(*val_id))?; // We'd need to look up the value type
                    } else {
                        write!(f, "ret <?> {}", value_label(*val_id))?;
                    }
                }
                None => write!(f, "ret void")?,
            }
        }
        Terminator::Unreachable => write!(f, "unreachable")?,
    }
    Ok(())
}

/// Convert opcode to LLVM-like string
fn opcode_to_ir(opcode: &Opcode) -> String {
    match opcode {
        // Binary operations
        Opcode::Add => "add".to_string(),
        Opcode::Sub => "sub".to_string(),
        Opcode::Mul => "mul".to_string(),
        Opcode::Div => "sdiv".to_string(), // signed division
        Opcode::Rem => "srem".to_string(), // signed remainder
        Opcode::Pow => "pow".to_string(),  // signed remainder
        Opcode::BitAnd => "and".to_string(),
        Opcode::BitOr => "or".to_string(),
        Opcode::BitXor => "xor".to_string(),
        Opcode::Shl => "shl".to_string(),
        Opcode::Shr => "ashr".to_string(), // arithmetic shift right
        Opcode::Eq => "icmp eq".to_string(),
        Opcode::Ne => "icmp ne".to_string(),
        Opcode::Lt => "icmp slt".to_string(), // signed less than
        Opcode::Gt => "icmp sgt".to_string(), // signed greater than
        Opcode::Le => "icmp sle".to_string(), // signed less or equal
        Opcode::Ge => "icmp sge".to_string(), // signed greater or equal

        // Unary operations
        Opcode::Neg => "sub".to_string(),    // 0 - operand
        Opcode::Not => "xor".to_string(),    // For boolean not
        Opcode::BitNot => "xor".to_string(), // For bitwise not

        // Memory operations
        Opcode::Load => "load".to_string(),
        Opcode::Store => "store".to_string(),
        Opcode::Alloca => "alloca".to_string(),
        Opcode::AllocaHeap => "alloca_heap".to_string(), // Custom for heap allocation
        Opcode::GetElementPtr => "getelementptr".to_string(),
        Opcode::Cast => "bitcast".to_string(), // Using bitcast as a general cast operation

        // Control flow
        Opcode::Jump => "br".to_string(),
        Opcode::Branch => "br".to_string(),
        Opcode::Switch => "switch".to_string(),
        Opcode::Phi => "phi".to_string(),

        // Function calls
        Opcode::Call => "call".to_string(),
        Opcode::Return => "ret".to_string(),
        Opcode::CallIndirect => "call_indirect".to_string(),

        // Aggregate operations
        Opcode::ExtractValue => "extractvalue".to_string(),
        Opcode::InsertValue => "insertvalue".to_string(),

        // Effect operations
        Opcode::PerformEffect => "perform_effect".to_string(),

        // Other operations
        Opcode::Nop => "nop".to_string(),
        Opcode::Unreachable => "unreachable".to_string(),
    }
}

/// Convert type to LLVM-like string representation
fn type_to_ir(type_context: &TypeContext, type_id: TypeId) -> String {
    match type_context.get(type_id) {
        Some(Type::Int(int_type)) => int_type_to_ir(*int_type).to_string(),
        Some(Type::Float(float_type)) => float_type_to_ir(*float_type).to_string(),
        Some(Type::Bool) => "i1".to_string(),
        Some(Type::Char) => "i8".to_string(), // Characters as 8-bit integers
        Some(Type::String) => "ptr".to_string(), // String as pointer to character array
        Some(Type::Pointer { element_type, .. }) => {
            let elem_type = type_to_ir(type_context, *element_type);
            format!("ptr to {}", elem_type) // Using 'ptr' for LLVM 15+ style
        }
        Some(Type::Array { element_type, size }) => {
            let elem_type = type_to_ir(type_context, *element_type);
            format!("[{} x {}]", size, elem_type)
        }
        Some(Type::Slice { element_type }) => {
            let elem_type = type_to_ir(type_context, *element_type);
            format!("slice<{}>", elem_type) // Custom representation for slices
        }
        Some(Type::Tuple { elements }) => {
            let elem_types: Vec<String> = elements
                .iter()
                .map(|&ty| type_to_ir(type_context, ty))
                .collect();
            format!("{{ {} }}", elem_types.join(", "))
        }
        Some(Type::Struct { fields, .. }) => {
            let field_types: Vec<String> = fields
                .iter()
                .map(|(_, ty)| type_to_ir(type_context, *ty))
                .collect();
            format!("%struct.{{ {} }}", field_types.join(", ")) // Custom struct representation
        }
        Some(Type::Function {
            params,
            return_type,
        }) => {
            let param_types: Vec<String> = params
                .iter()
                .map(|&ty| type_to_ir(type_context, ty))
                .collect();
            let return_type_str = type_to_ir(type_context, *return_type);
            format!("{} ({})*", return_type_str, param_types.join(", ")) // Function pointer
        }
        Some(Type::Reference { element_type, .. }) => {
            let elem_type = type_to_ir(type_context, *element_type);
            format!("ref {}", elem_type) // Custom representation for references
        }
        Some(Type::Void) => "void".to_string(),
        Some(Type::Unit) => "unit".to_string(), // Unit type
        None => "<?>".to_string(),
    }
}

/// Convert integer type to LLVM-like string
fn int_type_to_ir(int_type: IntType) -> String {
    match int_type {
        IntType::I8 => "i8".to_string(),
        IntType::I16 => "i16".to_string(),
        IntType::I32 => "i32".to_string(),
        IntType::I64 => "i64".to_string(),
        IntType::I128 => "i128".to_string(),
        IntType::ISize => "isize".to_string(), // Custom for platform-dependent signed integer
        IntType::U8 => "i8".to_string(),       // Unsigned represented as signed in LLVM
        IntType::U16 => "i16".to_string(),
        IntType::U32 => "i32".to_string(),
        IntType::U64 => "i64".to_string(),
        IntType::U128 => "i128".to_string(),
        IntType::USize => "usize".to_string(), // Custom for platform-dependent unsigned integer
    }
}

/// Convert float type to LLVM-like string
fn float_type_to_ir(float_type: FloatType) -> String {
    match float_type {
        FloatType::F32 => "float".to_string(),
        FloatType::F64 => "double".to_string(),
    }
}

// Implement Display for individual HIR types as well

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let display = HirDisplay {
            function: self,
            type_context: None,
        };
        write!(f, "{}", display)
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}:", self.id.0)?;

        // Print phi nodes
        for phi_node in &self.phi_nodes {
            writeln!(f)?;
            write!(f, "  {} = phi <?> ", value_label(phi_node.result))?;

            let incomings: Vec<String> = phi_node
                .incomings
                .iter()
                .map(|(value_id, block_id)| {
                    format!("[ {}, {} ]", value_label(*value_id), block_label(*block_id))
                })
                .collect();

            write!(f, "{}", incomings.join(", "))?;
        }

        // Print terminator
        write!(f, "  // Block terminator: {:?}", self.terminator)
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", opcode_to_ir(&self.opcode))?;

        // Write type
        write!(f, " <?> ")?; // Would need type context to get actual type

        // Write arguments
        let args: Vec<String> = self
            .args
            .iter()
            .map(|&arg_id| value_label(arg_id))
            .collect();

        write!(f, "{}", args.join(", "))
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", opcode_to_ir(self))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Constant(constant) => write!(f, "{}", constant),
            Value::Parameter { index, .. } => write!(f, "param_{}", index),
            Value::Local { .. } => write!(f, "local"),
            Value::InstructionResult { .. } => write!(f, "instruction_result"),
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Int(value) => write!(f, "{}", value),
            Constant::Float(value) => write!(f, "{}", value),
            Constant::Bool(value) => write!(f, "{}", value),
            Constant::Char(value) => write!(f, "'{}'", value),
            Constant::String(value) => write!(f, "\"{}\"", value),
            Constant::Null(_) => write!(f, "null"),
            Constant::Unit => write!(f, "()"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int(int_type) => write!(f, "{}", int_type_to_ir(*int_type)),
            Type::Float(float_type) => write!(f, "{}", float_type_to_ir(*float_type)),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::String => write!(f, "string"),
            Type::Pointer {
                element_type,
                mutable,
            } => {
                write!(
                    f,
                    "ptr<{}{}>",
                    if *mutable { "mut " } else { "" },
                    element_type.0
                )
            }
            Type::Array { element_type, size } => write!(f, "[{}; {}]", element_type.0, size),
            Type::Slice { element_type } => write!(f, "[{}]", element_type.0),
            Type::Tuple { elements } => {
                let elem_strs: Vec<String> =
                    elements.iter().map(|ty| format!("{}", ty.0)).collect();
                write!(f, "({})", elem_strs.join(", "))
            }
            Type::Struct { name, fields } => {
                write!(f, "struct {} {{ ", name)?;
                for (i, (field_name, field_type)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field_name, field_type.0)?;
                }
                write!(f, " }}")
            }
            Type::Function {
                params,
                return_type,
            } => {
                let param_strs: Vec<String> = params.iter().map(|ty| format!("{}", ty.0)).collect();
                write!(f, "fn({}) -> {}", param_strs.join(", "), return_type.0)
            }
            Type::Reference {
                element_type,
                mutable,
            } => {
                write!(
                    f,
                    "&{}{}",
                    if *mutable { "mut " } else { "" },
                    element_type.0
                )
            }
            Type::Void => write!(f, "void"),
            Type::Unit => write!(f, "()"),
        }
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Jump { target, args } => {
                write!(f, "jump bb{}(", target.0)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", arg.0)?;
                }
                write!(f, ")")
            }
            Terminator::Branch {
                condition,
                then_block,
                else_block,
                ..
            } => {
                write!(
                    f,
                    "br %{} ? bb{} : bb{}",
                    condition.0, then_block.0, else_block.0
                )
            }
            Terminator::Switch {
                value,
                targets,
                default,
            } => {
                write!(f, "switch %{} [", value.0)?;
                for (i, (val, block)) in targets.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} => bb{}", val, block.0)?;
                }
                write!(f, "], default: bb{}", default.0)
            }
            Terminator::Return { value } => match value {
                Some(val) => write!(f, "return %{}", val.0),
                None => write!(f, "return"),
            },
            Terminator::Unreachable => write!(f, "unreachable"),
        }
    }
}
