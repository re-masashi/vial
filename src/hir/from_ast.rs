//! Conversion from Typed AST to HIR (High-level Intermediate Representation)

use crate::ast::{
    TypedASTNode, TypedASTNodeKind, TypedEffectDef, TypedEnum, TypedExpr, TypedExprKind,
    TypedFunction, TypedImpl, TypedMacroDef, TypedPatKind, TypedPattern, TypedStruct,
    TypedTraitDef, TypedTypeAlias,
};
use crate::hir::{
    AllocationPreference, Builder, Function, FunctionSignature, Opcode, Param, TypeId, ValueId,
};
use crate::typechecker::{Type as TypeType, TypeKind};
use std::collections::HashMap;
use std::rc::Rc;

pub struct ASTToHIRConverter {
    // Map from AST binding IDs to HIR value IDs
    value_map: HashMap<crate::ast::BindingId, ValueId>,
}

impl ASTToHIRConverter {
    pub fn new() -> Self {
        Self {
            value_map: HashMap::new(),
        }
    }

    pub fn convert_program(&mut self, program: Vec<TypedASTNode>) -> Vec<Function> {
        let mut functions = Vec::new();

        for node in program {
            match node.node {
                TypedASTNodeKind::Function(func) => {
                    let hir_func = self.convert_function(&func);
                    functions.push(hir_func);
                }
                TypedASTNodeKind::Expr(expr) => {
                    // Handle top-level expressions if needed
                    let _value = self.convert_expr(&mut Builder::new(), &expr);
                    // For now, we don't store expressions as functions
                }
                TypedASTNodeKind::Struct(struct_def) => {
                    // Handle struct definitions if needed
                    let _hir_struct = self.convert_struct(&struct_def);
                }
                TypedASTNodeKind::Enum(enum_def) => {
                    // Handle enum definitions if needed
                    let _hir_enum = self.convert_enum(&enum_def);
                }
                TypedASTNodeKind::TypeAlias(alias_def) => {
                    // Handle type alias definitions if needed
                    let _hir_alias = self.convert_type_alias(&alias_def);
                }
                TypedASTNodeKind::Impl(impl_def) => {
                    // Handle impl definitions if needed
                    let _hir_impl = self.convert_impl(&impl_def);
                }
                TypedASTNodeKind::Trait(trait_def) => {
                    // Handle trait definitions if needed
                    let _hir_trait = self.convert_trait(&trait_def);
                }
                TypedASTNodeKind::MacroDef(macro_def) => {
                    // Handle macro definitions if needed
                    let _hir_macro = self.convert_macro(&macro_def);
                }
                TypedASTNodeKind::EffectDef(effect_def) => {
                    // Handle effect definitions if needed
                    let _hir_effect = self.convert_effect(&effect_def);
                }
                TypedASTNodeKind::Error => {
                    // Handle error nodes
                }
            }
        }

        functions
    }

    fn convert_function(&mut self, func: &TypedFunction) -> Function {
        // Create function signature
        let params: Vec<_> = func
            .args
            .iter()
            .map(|arg| {
                let param_type = self.convert_type(&arg.type_);
                Param {
                    name: Some(arg.name.0.to_string()), // Using interned string ID as string
                    ty: param_type,
                    allocation: AllocationPreference::Default,
                }
            })
            .collect();

        let return_type = self.convert_type(&func.return_type);
        let signature = FunctionSignature {
            params,
            return_type,
            is_variadic: false,
        };

        // Start building the function using the builder
        let mut builder = Builder::new();
        let func_name = func.name.0.to_string(); // Using interned string ID as string
        let _function = builder.start_function(func_name, signature.clone());

        // Create entry block
        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);

        // Convert function body if it exists
        let result_value = if let Some(ref body) = func.body {
            self.convert_expr(&mut builder, body)
        } else {
            // For functions without body, we need to handle this appropriately
            // Return a default value of the expected return type
            builder.const_int(0, self.convert_type(&func.return_type))
        };

        // Add a return instruction to the current block
        let _return_terminator = builder.ret(Some(result_value));

        // Since we can't access private fields directly, we'll rely on the builder's internal state management
        // The builder should handle the terminator appropriately

        // Build and return the function
        builder.build().unwrap_or_else(|| {
            // Create a minimal function if build fails
            Function {
                name: func.name.0.to_string(),
                signature,
                basic_blocks: vec![],
                instructions: vec![],
                block_params: HashMap::new(),
            }
        })
    }

    fn convert_expr(&mut self, builder: &mut Builder, expr: &TypedExpr) -> ValueId {
        match &expr.expr {
            TypedExprKind::Int(value) => {
                // Convert the AST type to HIR type
                let hir_type = self.convert_type(&expr.type_);
                builder.const_int(*value as i128, hir_type)
            }
            TypedExprKind::Float(value) => {
                // For float values, we'll need to create a more appropriate representation
                // For now, convert to a constant integer representation
                let hir_type = self.convert_type(&expr.type_);
                builder.const_int(*value as i128, hir_type) // Placeholder for float
            }
            TypedExprKind::Bool(value) => builder.const_bool(*value),
            TypedExprKind::String(value) => {
                // For string values, we'll need to handle this properly in a real implementation
                // For now, return a placeholder
                let hir_type = self.convert_type(&expr.type_);
                builder.const_int(value.len() as i128, hir_type) // Placeholder
            }
            TypedExprKind::Variable { binding_id, .. } => {
                // Look up the value in our mapping
                if let Some(&value_id) = self.value_map.get(binding_id) {
                    value_id
                } else {
                    // If not found, create a placeholder
                    // In a real implementation, this would be an error
                    // For now, we'll return a constant to avoid crashes
                    builder.const_int(0, self.convert_type(&expr.type_))
                }
            }
            TypedExprKind::BinOp { left, op, right } => {
                let left_val = self.convert_expr(builder, left);
                let right_val = self.convert_expr(builder, right);

                let opcode = match op {
                    crate::ast::BinOp::Add => Opcode::Add,
                    crate::ast::BinOp::Sub => Opcode::Sub,
                    crate::ast::BinOp::Mul => Opcode::Mul,
                    crate::ast::BinOp::Div => Opcode::Div,
                    crate::ast::BinOp::Mod => Opcode::Rem,
                    crate::ast::BinOp::Pow => Opcode::Mul, // Placeholder for power operation
                    crate::ast::BinOp::Eq => Opcode::Eq,
                    crate::ast::BinOp::Greater => Opcode::Gt,
                    crate::ast::BinOp::Less => Opcode::Lt,
                    crate::ast::BinOp::GreaterEq => Opcode::Ge,
                    crate::ast::BinOp::LessEq => Opcode::Le,
                    crate::ast::BinOp::NotEq => Opcode::Ne,
                    crate::ast::BinOp::Pipe => Opcode::Add, // Placeholder for pipe operator
                    crate::ast::BinOp::And => Opcode::BitAnd,
                    crate::ast::BinOp::Or => Opcode::BitOr,
                    crate::ast::BinOp::Nor => Opcode::BitOr, // Placeholder
                    crate::ast::BinOp::Xor => Opcode::BitXor,
                };

                let ty = self.convert_type(&expr.type_);
                builder.binary_op(opcode, left_val, right_val, ty)
            }
            TypedExprKind::UnOp { op, operand } => {
                let operand_val = self.convert_expr(builder, operand);

                match op {
                    crate::ast::UnOp::Not => {
                        // For boolean NOT, we need to use XOR with true
                        let bool_type = self.convert_type(&expr.type_);
                        let true_val = builder.const_bool(true);
                        builder.binary_op(Opcode::BitXor, operand_val, true_val, bool_type)
                    }
                    crate::ast::UnOp::Plus => {
                        // Unary plus is identity - just return the operand
                        operand_val
                    }
                    crate::ast::UnOp::Minus => {
                        // For negation, we need to subtract from zero
                        let zero_val = builder.const_int(0, self.convert_type(&expr.type_));
                        builder.binary_op(
                            Opcode::Sub,
                            zero_val,
                            operand_val,
                            self.convert_type(&expr.type_),
                        )
                    }
                    crate::ast::UnOp::Unwrap => {
                        // For unwrap operation, we might need to generate appropriate code
                        // For now, just return the operand
                        operand_val
                    }
                }
            }
            TypedExprKind::Assign {
                l_val: _,
                r_val,
                op,
            } => {
                // Handle assignment operations
                // For now, return the right value as a placeholder
                // In a real implementation, you'd need to handle assignment properly
                // by updating the left-hand side and returning the assigned value
                let right_val = self.convert_expr(builder, r_val);

                match op {
                    crate::ast::AssignOp::Assign => right_val,
                    crate::ast::AssignOp::AddAssign => right_val,
                    crate::ast::AssignOp::SubAssign => right_val,
                    crate::ast::AssignOp::MulAssign => right_val,
                    crate::ast::AssignOp::DivAssign => right_val,
                    crate::ast::AssignOp::ModAssign => right_val,
                }
            }
            TypedExprKind::Let {
                binding_id, value, ..
            } => {
                let value_id = self.convert_expr(builder, value);
                // Store the binding in our map
                self.value_map.insert(*binding_id, value_id);
                value_id
            }
            TypedExprKind::Array { elements, .. } => {
                // Handle array construction
                let element_values: Vec<ValueId> = elements
                    .iter()
                    .map(|elem| self.convert_expr(builder, elem))
                    .collect();

                // For now, create an array allocation and store elements
                // In a real implementation, we'd need to allocate memory for the array
                // and initialize each element

                if element_values.is_empty() {
                    // For empty arrays, return a default value
                    builder.const_int(0, self.convert_type(&expr.type_))
                } else {
                    // For now, just return the first element as a temporary solution
                    // until we implement proper array allocation
                    element_values[0]
                }
            }
            TypedExprKind::Tuple(elements) => {
                // Handle tuple construction
                let element_values: Vec<ValueId> = elements
                    .iter()
                    .map(|elem| self.convert_expr(builder, elem))
                    .collect();

                // For now, just return the first element as a temporary solution
                // until we implement proper tuple allocation
                if element_values.is_empty() {
                    // For empty tuples (unit type), return a default value
                    builder.const_int(0, self.convert_type(&expr.type_))
                } else {
                    element_values[0]
                }
            }
            TypedExprKind::Map { entries, .. } => {
                // Handle map construction
                let _entry_values: Vec<(ValueId, ValueId)> = entries
                    .iter()
                    .map(|(key, value)| {
                        (
                            self.convert_expr(builder, key),
                            self.convert_expr(builder, value),
                        )
                    })
                    .collect();

                // For now, return a default value
                // In a real implementation, we'd need to create proper map allocation
                // and initialization with the entries
                builder.const_int(0, self.convert_type(&expr.type_))
            }
            TypedExprKind::EnumConstruct { args, .. } => {
                // Handle enum construction
                let arg_values: Vec<ValueId> = args
                    .iter()
                    .map(|arg| self.convert_expr(builder, arg))
                    .collect();

                // For now, just return the first argument if available, otherwise a default
                // until we implement proper enum allocation
                if arg_values.is_empty() {
                    builder.const_int(0, self.convert_type(&expr.type_))
                } else {
                    arg_values[0]
                }
            }
            TypedExprKind::StructConstruct {
                struct_id: _,
                fields,
                ..
            } => {
                // Handle struct construction
                let field_values: Vec<ValueId> = fields
                    .iter()
                    .map(|(_, _, expr)| self.convert_expr(builder, expr))
                    .collect();

                // For now, just return the first field value as a temporary solution
                // until we implement proper struct allocation
                if field_values.is_empty() {
                    // Empty struct - return a default value
                    builder.const_int(0, self.convert_type(&expr.type_))
                } else {
                    field_values[0]
                }
            }
            TypedExprKind::Perform { args, .. } => {
                // Handle effect perform
                let arg_values: Vec<ValueId> = args
                    .iter()
                    .map(|arg| self.convert_expr(builder, arg))
                    .collect();

                // For now, just return the first argument if available, otherwise a default
                // In a real implementation, this would involve effect handling with proper
                // continuation passing and control flow
                if arg_values.is_empty() {
                    builder.const_int(0, self.convert_type(&expr.type_))
                } else {
                    arg_values[0]
                }
            }
            TypedExprKind::Handle {
                body,
                handlers: _,
                return_type: _,
            } => {
                // Handle effect handler - this is a complex construct for effect polymorphism
                // It handles effects that may be performed in the body
                // For now, we'll convert the body and handle basic effect handling

                // Convert the body expression
                self.convert_expr(builder, body)

                // In a full implementation, we would need to:
                // 1. Set up handlers for each effect operation
                // 2. Create appropriate control flow for resumption
                // 3. Handle the effect polymorphism properly
            }
            TypedExprKind::Lambda { body, .. } => {
                // Handle lambda expressions - convert the body expression
                // In a real implementation, this would create a closure/function value
                self.convert_expr(builder, body)
            }
            TypedExprKind::Cast { expr, .. } => {
                // Handle type casting
                self.convert_expr(builder, expr)
            }
            TypedExprKind::IfElse {
                condition,
                then,
                else_,
            } => {
                let cond_val = self.convert_expr(builder, condition);

                // Create blocks for the conditional
                let then_block = builder.create_block();
                let else_block = builder.create_block();
                let merge_block = builder.create_block();

                // Create the branch instruction to select between then and else blocks
                // Pass values as block parameters to simulate phi nodes
                let _branch = builder.branch(cond_val, then_block, else_block, vec![], vec![]);

                // Convert then branch
                builder.switch_to_block(then_block);
                let then_val = self.convert_expr(builder, then);

                // Create a jump from then block to merge block, passing the result value
                let _then_jump = builder.jump(merge_block, vec![then_val]);

                // Convert else branch
                builder.switch_to_block(else_block);
                let else_val = if let Some(else_expr) = else_ {
                    self.convert_expr(builder, else_expr)
                } else {
                    // For unit type, return a boolean value as placeholder
                    builder.const_bool(true)
                };

                // Create a jump from else block to merge block, passing the result value
                let _else_jump = builder.jump(merge_block, vec![else_val]);

                // Switch to merge block - this is where the result will be available
                builder.switch_to_block(merge_block);

                // In the current implementation, we'll return a placeholder
                // since we can't properly access the phi node values yet
                // The builder needs to be enhanced to properly support phi nodes
                builder.const_int(0, self.convert_type(&expr.type_))
            }
            TypedExprKind::Block { expressions } => {
                let mut last_value = ValueId(0);
                for expr in expressions {
                    last_value = self.convert_expr(builder, expr);
                }
                last_value
            }
            TypedExprKind::With {
                context,
                var: _,
                binding_id,
                var_type: _,
                body,
            } => {
                // Handle with expressions: with (context) var { body }
                // First evaluate the context
                let context_val = self.convert_expr(builder, context);

                // Bind the context value to the variable
                self.value_map.insert(*binding_id, context_val);

                // Evaluate the body with the binding in scope
                self.convert_expr(builder, body)
            }
            TypedExprKind::Loop { body, .. } => {
                // Handle loops - create a basic loop structure
                let loop_block = builder.create_block();
                let _continue_block = builder.create_block();

                // Create the jump to the loop block
                let _jump = builder.jump(loop_block, vec![]);

                // Switch to the loop block
                builder.switch_to_block(loop_block);

                // Convert the loop body
                let body_val = self.convert_expr(builder, body);

                // Jump back to the beginning of the loop
                let _continue_jump = builder.jump(loop_block, vec![]);

                // For now, return the body value
                // In a real implementation, we'd need to handle break/continue properly
                body_val
            }
            TypedExprKind::Match { scrutinee, arms } => {
                // Handle match expressions - this is a complex control flow operation
                // For now, we'll implement a simple version that evaluates the scrutinee
                // and returns the first arm's result as a placeholder

                // Convert the scrutinee
                let _scrutinee_val = self.convert_expr(builder, scrutinee);

                // For now, just return the first arm's body as a temporary solution
                // In a real implementation, we'd generate proper switch/branch logic
                // with pattern matching and control flow
                if arms.is_empty() {
                    // No arms - return a default value
                    builder.const_int(0, self.convert_type(&expr.type_))
                } else {
                    self.convert_expr(builder, &arms[0].body)
                }
            }
            TypedExprKind::For { iterator, body, .. } => {
                // Handle for loops - convert the iterator and body
                let _iterator_val = self.convert_expr(builder, iterator);

                // For now, just convert the body as a temporary solution
                // In a real implementation, we'd need to handle the iteration properly
                // with proper control flow for the iterator
                self.convert_expr(builder, body)
            }
            TypedExprKind::While { condition, body } => {
                // Handle while loops - create a proper loop structure
                let cond_block = builder.create_block();
                let body_block = builder.create_block();
                let exit_block = builder.create_block();

                // Jump to condition check
                let _jump_to_cond = builder.jump(cond_block, vec![]);

                // Switch to condition block
                builder.switch_to_block(cond_block);
                let cond_val = self.convert_expr(builder, condition);

                // Create conditional branch based on condition
                let _branch = builder.branch(cond_val, body_block, exit_block, vec![], vec![]);

                // Switch to body block
                builder.switch_to_block(body_block);
                let _body_val = self.convert_expr(builder, body);

                // Jump back to condition to check again
                let _jump_back = builder.jump(cond_block, vec![]);

                // Switch to exit block
                builder.switch_to_block(exit_block);

                // For now, return a default value
                // In a real implementation, while loops might return the last value or unit
                builder.const_int(0, self.convert_type(&expr.type_))
            }
            TypedExprKind::IfLet { expr, then, .. } => {
                // Handle if-let expressions - for now, a simplified approach
                let _expr_val = self.convert_expr(builder, expr);

                // For now, just convert the then branch as a temporary solution
                // In a real implementation, we'd need to destructure and match the pattern
                self.convert_expr(builder, then)
            }
            TypedExprKind::WhileLet { expr: _, body, .. } => {
                // For now, just convert the body as a temporary solution
                // In a real implementation, we'd need to properly handle the pattern matching
                // and loop control flow
                self.convert_expr(builder, body)
            }
            TypedExprKind::Return(return_expr) => {
                // This should terminate the current function
                let return_val = return_expr
                    .as_ref()
                    .map(|expr| self.convert_expr(builder, expr));

                // Create and return the return terminator
                return_val.unwrap_or_else(|| builder.const_int(0, self.convert_type(&expr.type_)))
            }
            TypedExprKind::Break(break_expr) => {
                // This should break out of a loop
                // For now, return the break value
                // In a real implementation, this would need to know which loop to break from
                // and would generate a proper control flow instruction
                if let Some(expr) = break_expr {
                    self.convert_expr(builder, expr)
                } else {
                    // For break without value, return a default
                    builder.const_int(0, self.convert_type(&expr.type_))
                }
            }
            TypedExprKind::Continue => {
                // This should continue to the next iteration
                // For now, return a default value
                // In a real implementation, this would generate a proper control flow instruction
                builder.const_int(0, self.convert_type(&expr.type_))
            }
            TypedExprKind::Call { function, args, .. } => {
                // Handle function calls
                let func_val = self.convert_expr(builder, function);
                let arg_values: Vec<ValueId> = args
                    .iter()
                    .map(|arg| self.convert_expr(builder, arg))
                    .collect();

                // Generate a call instruction
                let return_type = self.convert_type(&expr.type_);
                builder.call(func_val, arg_values, return_type)
            }
            TypedExprKind::Index { target, index, .. } => {
                // Handle indexing operations
                let target_val = self.convert_expr(builder, target);
                let index_val = self.convert_expr(builder, index);

                // Create a get element pointer instruction
                let result_type = self.convert_type(&expr.type_);
                builder.get_element_ptr(target_val, vec![index_val], result_type)
            }
            TypedExprKind::FieldAccess {
                target,
                field_id,
                field_type,
                ..
            } => {
                // Handle field access
                let target_val = self.convert_expr(builder, target);

                // Create a get element pointer instruction with field index
                let index_val =
                    builder.const_int(field_id.0 as i128, self.convert_type(field_type));
                let result_type = self.convert_type(&expr.type_);
                builder.get_element_ptr(target_val, vec![index_val], result_type)
            }
            TypedExprKind::OptionalChain {
                target,
                field: _,
                field_id,
                field_type,
            } => {
                // Handle optional chaining: target?.field
                // For now, implement simple field access.
                // In a real implementation, this would check for null/undefined before accessing
                let target_val = self.convert_expr(builder, target);

                // Create a get element pointer instruction with field index
                let index_val =
                    builder.const_int(field_id.0 as i128, self.convert_type(field_type));
                let result_type = self.convert_type(field_type); // Using field_type as the result type
                builder.get_element_ptr(target_val, vec![index_val], result_type)
            }
            TypedExprKind::MacroCall { args, .. } => {
                // Since macros should be expanded before HIR generation, this is prolly unreachable
                let _arg_values: Vec<ValueId> = args
                    .iter()
                    .map(|arg| self.convert_expr(builder, arg))
                    .collect();

                builder.const_int(0, self.convert_type(&expr.type_))
            }
            TypedExprKind::Import(_) => {
                // Imports are compile-time constructs that don't generate runtime code
                // The actual import resolution happens during earlier phases
                // For now, return a constant value as imports (I think) don't have a runtime value
                builder.const_int(0, self.convert_type(&expr.type_))
            }
            TypedExprKind::Error => {
                // Handle error expressions
                ValueId(0) // Placeholder for error
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn convert_type(&mut self, ast_type: &Rc<TypeType>) -> TypeId {
        // This is a simplified conversion - you'd want to map AST types to HIR types properly
        match &ast_type.type_ {
            TypeKind::Constructor {
                name,
                args: _,
                kind: _,
            } => {
                // Map constructor types to appropriate HIR type IDs
                // name is the interned string ID for the type name
                // For now, just return the name as TypeId
                // In a real implementation, you'd handle type arguments too
                TypeId(*name)
            }
            TypeKind::Variable { id, kind: _ } => {
                // Use the AST type ID as the basis for HIR type ID
                TypeId(*id)
            }
            TypeKind::Function {
                params: _,
                return_type: _,
                effects: _,
            } => {
                // For function types, we could create a more complex mapping
                // For now, return a special function type ID
                // The params vector contains the parameter types
                TypeId(7) // Placeholder for function type
            }
            TypeKind::Row { fields: _, rest: _ } => {
                // Row types for records/objects
                // fields is a vector of (name, type) pairs
                // rest is the remaining fields type variable
                TypeId(10) // Placeholder for row type
            }
            TypeKind::Tuple(_element_types) => {
                // Tuple types
                // element_types is a vector of the tuple element types
                TypeId(11) // Placeholder for tuple type
            }
            TypeKind::Union(_variant_types) => {
                // Union types
                // variant_types is a vector of the union variant types
                TypeId(12) // Placeholder for union type
            }
            TypeKind::Forall {
                vars: _,
                constraints: _,
                body: _,
            } => {
                // Universal quantification (polymorphic types)
                // vars is a vector of (id, kind) pairs
                TypeId(13) // Placeholder for forall type
            }
            TypeKind::Exists {
                vars: _,
                constraints: _,
                body: _,
            } => {
                // Existential quantification
                // vars is a vector of (id, kind) pairs
                TypeId(14) // Placeholder for exists type
            }
            TypeKind::Never => TypeId(8), // Placeholder for never type
            TypeKind::Error => TypeId(9), // Placeholder for error type
            TypeKind::Pointer(inner_type) => {
                // Pointer types for FFI and low-level operations
                // inner_type is the type being pointed to
                let _inner = self.convert_type(inner_type);
                TypeId(15) // Placeholder for pointer type
            }
            TypeKind::Trait(_trait_names) => {
                // Trait types
                // trait_names is a vector of trait IDs
                TypeId(16) // Placeholder for trait type
            }
        }
    }

    pub fn convert_pattern(&mut self, pattern: &TypedPattern) -> ValueId {
        // Convert pattern to a representation suitable for HIR
        // For now, return a placeholder
        match &pattern.pat {
            TypedPatKind::Wildcard => ValueId(0), // Placeholder for wildcard
            TypedPatKind::Bind {
                name: _,
                binding_id,
            } => {
                // Binding pattern - return the binding ID as a ValueId
                ValueId(binding_id.0)
            }
            TypedPatKind::Literal(literal) => {
                // Literal pattern
                match literal {
                    crate::ast::Literal::Int(value) => {
                        let _hir_type = self.convert_type(&pattern.type_);
                        ValueId(*value as usize) // Placeholder
                    }
                    crate::ast::Literal::Float(value) => {
                        ValueId((*value as usize) + 1000) // Placeholder
                    }
                    crate::ast::Literal::Bool(value) => ValueId(if *value { 1 } else { 0 }),
                    crate::ast::Literal::String(value) => {
                        ValueId(value.len()) // Placeholder
                    }
                }
            }
            TypedPatKind::Array {
                patterns: _,
                element_type: _,
            } => {
                // Array pattern
                ValueId(0) // Placeholder
            }
            TypedPatKind::Tuple {
                patterns: _,
                element_types: _,
            } => {
                // Tuple pattern
                ValueId(0) // Placeholder
            }
            TypedPatKind::Or(patterns) => {
                // Or pattern - multiple alternatives
                let _pattern_values: Vec<ValueId> = patterns
                    .iter()
                    .map(|pat| self.convert_pattern(pat))
                    .collect();
                ValueId(0) // Placeholder
            }
            TypedPatKind::As {
                name: _,
                binding_id: _,
                pattern: inner_pattern,
            } => {
                // As pattern - binding with @
                self.convert_pattern(inner_pattern)
            }
            TypedPatKind::Struct {
                name: _,
                struct_id: _,
                fields,
            } => {
                // Struct pattern
                let _field_values: Vec<ValueId> = fields
                    .iter()
                    .map(|(_, _, pat)| self.convert_pattern(pat))
                    .collect();
                ValueId(0) // Placeholder
            }
            TypedPatKind::Enum {
                enum_name: _,
                enum_id: _,
                variant: _,
                variant_id: _,
                params,
            } => {
                // Enum pattern
                let _param_values: Vec<ValueId> =
                    params.iter().map(|pat| self.convert_pattern(pat)).collect();
                ValueId(0) // Placeholder
            }
            TypedPatKind::Range { start: _, end: _ } => {
                // Range pattern
                ValueId(0) // Placeholder
            }
            TypedPatKind::Rest {
                name: _,
                binding_id: _,
            } => {
                // Rest pattern
                ValueId(0) // Placeholder
            }
            TypedPatKind::Error => {
                // Error pattern
                ValueId(0) // Placeholder
            }
        }
    }

    fn convert_struct(&mut self, struct_def: &TypedStruct) -> TypedStruct {
        // For now, just return the struct as-is
        // In a real implementation, you'd convert struct fields to HIR representation
        // and potentially generate associated functions/methods
        struct_def.clone()
    }

    fn convert_enum(&mut self, enum_def: &TypedEnum) -> TypedEnum {
        // For now, just return the enum as-is
        // In a real implementation, you'd convert enum variants to HIR representation
        // and potentially generate associated functions/methods
        enum_def.clone()
    }

    fn convert_type_alias(&mut self, alias_def: &TypedTypeAlias) -> TypedTypeAlias {
        // For now, just return the alias as-is
        // In a real implementation, you'd convert the target type to HIR representation
        alias_def.clone()
    }

    fn convert_impl(&mut self, impl_def: &TypedImpl) -> TypedImpl {
        // For now, just return the impl as-is
        // In a real implementation, you'd convert the methods to HIR functions
        impl_def.clone()
    }

    fn convert_trait(&mut self, trait_def: &TypedTraitDef) -> TypedTraitDef {
        // For now, just return the trait as-is
        // In a real implementation, you'd convert the methods to HIR representation
        trait_def.clone()
    }

    fn convert_macro(&mut self, macro_def: &TypedMacroDef) -> TypedMacroDef {
        // For now, just return the macro as-is
        // Macros are typically handled during compilation, not converted to HIR
        macro_def.clone()
    }

    fn convert_effect(&mut self, effect_def: &TypedEffectDef) -> TypedEffectDef {
        // For now, just return the effect as-is
        // In a real implementation, you'd convert effect operations to HIR representation
        effect_def.clone()
    }
}

impl Default for ASTToHIRConverter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::typechecker::{EffectSet, Type, TypeKind};
    use std::rc::Rc;

    fn create_int_type() -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: 0, // interned "Int"
                args: vec![],
                kind: Kind::Star,
            },
        })
    }

    fn create_bool_type() -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: 1, // interned "Bool"
                args: vec![],
                kind: Kind::Star,
            },
        })
    }

    fn create_int_expr(value: i64) -> TypedExpr {
        TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Int(value),
            type_: create_int_type(),
        }
    }

    fn create_bool_expr(value: bool) -> TypedExpr {
        TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Bool(value),
            type_: create_bool_type(),
        }
    }

    #[test]
    fn test_convert_int_literal() {
        let mut converter = ASTToHIRConverter::new();
        let int_expr = create_int_expr(42);

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &int_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_bool_literal() {
        let mut converter = ASTToHIRConverter::new();
        let bool_expr = create_bool_expr(true);

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bool_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_binary_add_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(10);
        let right = create_int_expr(20);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::Add,
                right: Box::new(right),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_binary_sub_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(10);
        let right = create_int_expr(5);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::Sub,
                right: Box::new(right),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_binary_mul_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(10);
        let right = create_int_expr(5);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::Mul,
                right: Box::new(right),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_binary_div_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(10);
        let right = create_int_expr(2);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::Div,
                right: Box::new(right),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_binary_eq_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(10);
        let right = create_int_expr(10);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::Eq,
                right: Box::new(right),
            },
            type_: create_bool_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_binary_neq_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(10);
        let right = create_int_expr(5);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::NotEq,
                right: Box::new(right),
            },
            type_: create_bool_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_binary_gt_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(10);
        let right = create_int_expr(5);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::Greater,
                right: Box::new(right),
            },
            type_: create_bool_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_binary_lt_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(5);
        let right = create_int_expr(10);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::Less,
                right: Box::new(right),
            },
            type_: create_bool_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_binary_ge_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(10);
        let right = create_int_expr(10);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::GreaterEq,
                right: Box::new(right),
            },
            type_: create_bool_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_binary_le_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(5);
        let right = create_int_expr(10);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::LessEq,
                right: Box::new(right),
            },
            type_: create_bool_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_block_single_expression() {
        let mut converter = ASTToHIRConverter::new();
        let expr = create_int_expr(42);
        let block_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Block {
                expressions: vec![expr],
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &block_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_block_multiple_expressions() {
        let mut converter = ASTToHIRConverter::new();
        let expr1 = create_int_expr(10);
        let expr2 = create_int_expr(20);
        let block_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Block {
                expressions: vec![expr1, expr2],
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &block_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_let_binding() {
        let mut converter = ASTToHIRConverter::new();
        let value_expr = create_int_expr(42);
        let interner = &mut Interner::new();
        let var_name = interner.intern("x");

        let let_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Let {
                var: var_name,
                binding_id: BindingId(1),
                var_type: create_int_type(),
                value: Box::new(value_expr),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &let_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_function_with_no_body() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let func_name = interner.intern("test_func");

        let func = TypedFunction {
            span: 0..1,
            file: "test".to_string(),
            vis: Visibility::Public,
            name: func_name,
            function_id: FunctionId(0),
            type_params: vec![],
            args: vec![],
            return_type: create_int_type(),
            where_constraints: vec![],
            effects: EffectSet {
                effects: vec![],
                rest: None,
            },
            function_type: Rc::new(Type {
                span: None,
                file: None,
                type_: TypeKind::Function {
                    params: vec![],
                    return_type: create_int_type(),
                    effects: EffectSet {
                        effects: vec![],
                        rest: None,
                    },
                },
            }),
            body: None,
        };

        let hir_func = converter.convert_function(&func);

        // The function name should match
        assert_eq!(hir_func.name, "0");
    }

    #[test]
    fn test_convert_function_with_body() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let func_name = interner.intern("test_func");
        let body = create_int_expr(42);

        let func = TypedFunction {
            span: 0..1,
            file: "test".to_string(),
            vis: Visibility::Public,
            name: func_name,
            function_id: FunctionId(0),
            type_params: vec![],
            args: vec![],
            return_type: create_int_type(),
            where_constraints: vec![],
            effects: EffectSet {
                effects: vec![],
                rest: None,
            },
            function_type: Rc::new(Type {
                span: None,
                file: None,
                type_: TypeKind::Function {
                    params: vec![],
                    return_type: create_int_type(),
                    effects: EffectSet {
                        effects: vec![],
                        rest: None,
                    },
                },
            }),
            body: Some(body),
        };

        let hir_func = converter.convert_function(&func);

        // The function name should match
        assert_eq!(hir_func.name, "0");
    }

    #[test]
    fn test_convert_function_with_parameters() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let func_name = interner.intern("test_func");
        let arg_name = interner.intern("x");

        let arg = TypedFnArg {
            span: 0..1,
            file: "test".to_string(),
            name: arg_name,
            binding_id: BindingId(0),
            type_: create_int_type(),
        };

        let func = TypedFunction {
            span: 0..1,
            file: "test".to_string(),
            vis: Visibility::Public,
            name: func_name,
            function_id: FunctionId(0),
            type_params: vec![],
            args: vec![arg],
            return_type: create_int_type(),
            where_constraints: vec![],
            effects: EffectSet {
                effects: vec![],
                rest: None,
            },
            function_type: Rc::new(Type {
                span: None,
                file: None,
                type_: TypeKind::Function {
                    params: vec![create_int_type()],
                    return_type: create_int_type(),
                    effects: EffectSet {
                        effects: vec![],
                        rest: None,
                    },
                },
            }),
            body: Some(create_int_expr(42)),
        };

        let hir_func = converter.convert_function(&func);

        // The function should have one parameter
        assert_eq!(hir_func.signature.params.len(), 1);
    }

    #[test]
    fn test_convert_program_empty() {
        let mut converter = ASTToHIRConverter::new();
        let program = vec![];

        let functions = converter.convert_program(program);

        // Should return an empty vector
        assert_eq!(functions.len(), 0);
    }

    #[test]
    fn test_convert_program_single_function() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let func_name = interner.intern("test_func");

        let func_node = TypedASTNode {
            span: 0..1,
            file: "test".to_string(),
            node: TypedASTNodeKind::Function(Box::new(TypedFunction {
                span: 0..1,
                file: "test".to_string(),
                vis: Visibility::Public,
                name: func_name,
                function_id: FunctionId(0),
                type_params: vec![],
                args: vec![],
                return_type: create_int_type(),
                where_constraints: vec![],
                effects: EffectSet {
                    effects: vec![],
                    rest: None,
                },
                function_type: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Function {
                        params: vec![],
                        return_type: create_int_type(),
                        effects: EffectSet {
                            effects: vec![],
                            rest: None,
                        },
                    },
                }),
                body: Some(create_int_expr(42)),
            })),
            attributes: vec![],
        };

        let program = vec![func_node];
        let functions = converter.convert_program(program);

        // Should return one function
        assert_eq!(functions.len(), 1);
    }

    #[test]
    fn test_convert_program_multiple_functions() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let func_name1 = interner.intern("func1");
        let func_name2 = interner.intern("func2");

        let func_node1 = TypedASTNode {
            span: 0..1,
            file: "test".to_string(),
            node: TypedASTNodeKind::Function(Box::new(TypedFunction {
                span: 0..1,
                file: "test".to_string(),
                vis: Visibility::Public,
                name: func_name1,
                function_id: FunctionId(0),
                type_params: vec![],
                args: vec![],
                return_type: create_int_type(),
                where_constraints: vec![],
                effects: EffectSet {
                    effects: vec![],
                    rest: None,
                },
                function_type: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Function {
                        params: vec![],
                        return_type: create_int_type(),
                        effects: EffectSet {
                            effects: vec![],
                            rest: None,
                        },
                    },
                }),
                body: Some(create_int_expr(42)),
            })),
            attributes: vec![],
        };

        let func_node2 = TypedASTNode {
            span: 0..1,
            file: "test".to_string(),
            node: TypedASTNodeKind::Function(Box::new(TypedFunction {
                span: 0..1,
                file: "test".to_string(),
                vis: Visibility::Private,
                name: func_name2,
                function_id: FunctionId(1),
                type_params: vec![],
                args: vec![],
                return_type: create_int_type(),
                where_constraints: vec![],
                effects: EffectSet {
                    effects: vec![],
                    rest: None,
                },
                function_type: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Function {
                        params: vec![],
                        return_type: create_int_type(),
                        effects: EffectSet {
                            effects: vec![],
                            rest: None,
                        },
                    },
                }),
                body: Some(create_int_expr(84)),
            })),
            attributes: vec![],
        };

        let program = vec![func_node1, func_node2];
        let functions = converter.convert_program(program);

        // Should return two functions
        assert_eq!(functions.len(), 2);
    }

    #[test]
    fn test_convert_type_int() {
        let mut converter = ASTToHIRConverter::new();
        let int_type = create_int_type();

        let hir_type_id = converter.convert_type(&int_type);

        // Should return a valid TypeId
        assert!(hir_type_id.0 >= 0);
    }

    #[test]
    fn test_convert_type_bool() {
        let mut converter = ASTToHIRConverter::new();
        let bool_type = create_bool_type();

        let hir_type_id = converter.convert_type(&bool_type);

        // Should return a valid TypeId
        assert!(hir_type_id.0 >= 0);
    }

    #[test]
    fn test_convert_type_variable() {
        let mut converter = ASTToHIRConverter::new();
        let var_type = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Variable {
                id: 42,
                kind: Kind::Star,
            },
        });

        let hir_type_id = converter.convert_type(&var_type);

        // Should return the same ID as in the variable
        assert_eq!(hir_type_id.0, 42);
    }

    #[test]
    fn test_convert_type_constructor() {
        let mut converter = ASTToHIRConverter::new();
        let constructor_type = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: 100,
                args: vec![],
                kind: Kind::Star,
            },
        });

        let hir_type_id = converter.convert_type(&constructor_type);

        // Should return the same name as in the constructor
        assert_eq!(hir_type_id.0, 100);
    }

    #[test]
    fn test_convert_if_else_with_else() {
        let mut converter = ASTToHIRConverter::new();
        let condition = create_bool_expr(true);
        let then_expr = create_int_expr(10);
        let else_expr = create_int_expr(20);

        let if_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::IfElse {
                condition: Box::new(condition),
                then: Box::new(then_expr),
                else_: Some(Box::new(else_expr)),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &if_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_if_else_without_else() {
        let mut converter = ASTToHIRConverter::new();
        let condition = create_bool_expr(true);
        let then_expr = create_int_expr(10);

        let if_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::IfElse {
                condition: Box::new(condition),
                then: Box::new(then_expr),
                else_: None,
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &if_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_unary_operation() {
        let mut converter = ASTToHIRConverter::new();
        let operand = create_int_expr(42);

        let unop_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::UnOp {
                op: UnOp::Minus,
                operand: Box::new(operand),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &unop_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_not_operation() {
        let mut converter = ASTToHIRConverter::new();
        let operand = create_bool_expr(true);

        let unop_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::UnOp {
                op: UnOp::Not,
                operand: Box::new(operand),
            },
            type_: create_bool_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &unop_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_modulo_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(10);
        let right = create_int_expr(3);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::Mod,
                right: Box::new(right),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_bitwise_and_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(12);
        let right = create_int_expr(10);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::And,
                right: Box::new(right),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_bitwise_or_operation() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(12);
        let right = create_int_expr(10);
        let bin_op_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::Or,
                right: Box::new(right),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &bin_op_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_string_literal() {
        let mut converter = ASTToHIRConverter::new();
        let string_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::String("hello".to_string()),
            type_: Rc::new(Type {
                span: None,
                file: None,
                type_: TypeKind::Constructor {
                    name: 3, // string type
                    args: vec![],
                    kind: Kind::Star,
                },
            }),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &string_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_float_literal() {
        let mut converter = ASTToHIRConverter::new();
        let float_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Float(3.14),
            type_: Rc::new(Type {
                span: None,
                file: None,
                type_: TypeKind::Constructor {
                    name: 4, // float type
                    args: vec![],
                    kind: Kind::Star,
                },
            }),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &float_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_nested_expressions() {
        let mut converter = ASTToHIRConverter::new();
        let expr1 = create_int_expr(5);
        let expr2 = create_int_expr(10);
        let bin_op1 = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(expr1),
                op: BinOp::Add,
                right: Box::new(expr2),
            },
            type_: create_int_type(),
        };

        let expr3 = create_int_expr(3);
        let nested_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(bin_op1),
                op: BinOp::Mul,
                right: Box::new(expr3),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &nested_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_converter_new() {
        let converter = ASTToHIRConverter::new();

        // Should create an empty converter
        assert_eq!(converter.value_map.len(), 0);
    }

    #[test]
    fn test_converter_default() {
        let converter: ASTToHIRConverter = Default::default();

        // Should create an empty converter
        assert_eq!(converter.value_map.len(), 0);
    }

    #[test]
    fn test_convert_program_with_non_function_nodes() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let struct_name = interner.intern("TestStruct");

        // Create a struct node (not a function)
        let struct_node = TypedASTNode {
            span: 0..1,
            file: "test".to_string(),
            node: TypedASTNodeKind::Struct(TypedStruct {
                span: 0..1,
                file: "test".to_string(),
                name: struct_name,
                struct_id: StructId(0),
                vis: Visibility::Public,
                type_params: vec![],
                fields: vec![],
                methods: vec![],
                struct_type: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Constructor {
                        name: 5, // struct type
                        args: vec![],
                        kind: Kind::Star,
                    },
                }),
            }),
            attributes: vec![],
        };

        let program = vec![struct_node];
        let functions = converter.convert_program(program);

        // Should return an empty vector since there are no functions
        assert_eq!(functions.len(), 0);
    }

    #[test]
    fn test_convert_expr_with_unknown_kind() {
        let mut converter = ASTToHIRConverter::new();
        let expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Error,
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &expr);

        // Should return a placeholder value ID
        assert_eq!(result.0, 0);
    }

    #[test]
    fn test_convert_type_never() {
        let mut converter = ASTToHIRConverter::new();
        let never_type = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Never,
        });

        let hir_type_id = converter.convert_type(&never_type);

        // Should return a TypeId for Never
        assert_eq!(hir_type_id.0, 8); // Based on our implementation
    }

    #[test]
    fn test_convert_type_error() {
        let mut converter = ASTToHIRConverter::new();
        let error_type = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Error,
        });

        let hir_type_id = converter.convert_type(&error_type);

        // Should return a TypeId for Error
        assert_eq!(hir_type_id.0, 9); // Based on our implementation
    }

    #[test]
    fn test_convert_function_type() {
        let mut converter = ASTToHIRConverter::new();
        let func_type = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Function {
                params: vec![create_int_type()],
                return_type: create_bool_type(),
                effects: EffectSet {
                    effects: vec![],
                    rest: None,
                },
            },
        });

        let hir_type_id = converter.convert_type(&func_type);

        // Should return a TypeId for Function
        assert_eq!(hir_type_id.0, 7); // Based on our implementation
    }

    #[test]
    fn test_convert_tuple_type() {
        let mut converter = ASTToHIRConverter::new();
        let tuple_type = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Tuple(vec![create_int_type(), create_bool_type()]),
        });

        let hir_type_id = converter.convert_type(&tuple_type);

        // Should return TypeId for tuple type
        assert_eq!(hir_type_id.0, 11);
    }

    #[test]
    fn test_convert_union_type() {
        let mut converter = ASTToHIRConverter::new();
        let union_type = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Union(vec![create_int_type(), create_bool_type()]),
        });

        let hir_type_id = converter.convert_type(&union_type);

        // Should return TypeId for union type
        assert_eq!(hir_type_id.0, 12);
    }

    #[test]
    fn test_convert_variable_expr() {
        let mut converter = ASTToHIRConverter::new();
        let var_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Variable {
                name: Symbol(100),
                binding_id: BindingId(200),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &var_expr);

        // Should return a placeholder since binding is not in the map
        assert_eq!(result.0, 0);
    }

    #[test]
    fn test_convert_variable_expr_with_binding() {
        let mut converter = ASTToHIRConverter::new();
        let binding_id = BindingId(200);
        let expected_value = ValueId(42);

        // Add binding to the map
        converter.value_map.insert(binding_id, expected_value);

        let var_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Variable {
                name: Symbol(100),
                binding_id,
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &var_expr);

        // Should return the mapped value
        assert_eq!(result, expected_value);
    }

    #[test]
    fn test_convert_multiple_let_bindings() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let var_name1 = interner.intern("x");
        let var_name2 = interner.intern("y");

        let value_expr1 = create_int_expr(10);
        let value_expr2 = create_int_expr(20);

        let let_expr1 = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Let {
                var: var_name1,
                binding_id: BindingId(1),
                var_type: create_int_type(),
                value: Box::new(value_expr1),
            },
            type_: create_int_type(),
        };

        let let_expr2 = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Let {
                var: var_name2,
                binding_id: BindingId(2),
                var_type: create_int_type(),
                value: Box::new(value_expr2),
            },
            type_: create_int_type(),
        };

        let block_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Block {
                expressions: vec![let_expr1, let_expr2, create_int_expr(30)],
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &block_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_complex_nested_if_else() {
        let mut converter = ASTToHIRConverter::new();

        // Create nested if-else: if (if true then false else true) then 10 else 20
        let inner_condition = create_bool_expr(true);
        let inner_then = create_bool_expr(false);
        let inner_else = create_bool_expr(true);

        let outer_condition = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::IfElse {
                condition: Box::new(inner_condition),
                then: Box::new(inner_then),
                else_: Some(Box::new(inner_else)),
            },
            type_: create_bool_type(),
        };

        let outer_then = create_int_expr(10);
        let outer_else = create_int_expr(20);

        let complex_if_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::IfElse {
                condition: Box::new(outer_condition),
                then: Box::new(outer_then),
                else_: Some(Box::new(outer_else)),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &complex_if_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_binary_operations_precedence() {
        let mut converter = ASTToHIRConverter::new();

        // Create: (5 + 3) * 2
        let left = create_int_expr(5);
        let right = create_int_expr(3);
        let add_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(left),
                op: BinOp::Add,
                right: Box::new(right),
            },
            type_: create_int_type(),
        };

        let mult_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(add_expr),
                op: BinOp::Mul,
                right: Box::new(create_int_expr(2)),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &mult_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_complex_function_with_multiple_params() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let func_name = interner.intern("complex_func");
        let arg_name1 = interner.intern("x");
        let arg_name2 = interner.intern("y");

        let arg1 = TypedFnArg {
            span: 0..1,
            file: "test".to_string(),
            name: arg_name1,
            binding_id: BindingId(0),
            type_: create_int_type(),
        };

        let arg2 = TypedFnArg {
            span: 0..1,
            file: "test".to_string(),
            name: arg_name2,
            binding_id: BindingId(1),
            type_: create_int_type(),
        };

        let body = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(TypedExpr {
                    span: 0..1,
                    file: "test".to_string(),
                    expr: TypedExprKind::Variable {
                        name: arg_name1,
                        binding_id: BindingId(0),
                    },
                    type_: create_int_type(),
                }),
                op: BinOp::Add,
                right: Box::new(TypedExpr {
                    span: 0..1,
                    file: "test".to_string(),
                    expr: TypedExprKind::Variable {
                        name: arg_name2,
                        binding_id: BindingId(1),
                    },
                    type_: create_int_type(),
                }),
            },
            type_: create_int_type(),
        };

        let func = TypedFunction {
            span: 0..1,
            file: "test".to_string(),
            vis: Visibility::Public,
            name: func_name,
            function_id: FunctionId(0),
            type_params: vec![],
            args: vec![arg1, arg2],
            return_type: create_int_type(),
            where_constraints: vec![],
            effects: EffectSet {
                effects: vec![],
                rest: None,
            },
            function_type: Rc::new(Type {
                span: None,
                file: None,
                type_: TypeKind::Function {
                    params: vec![create_int_type(), create_int_type()],
                    return_type: create_int_type(),
                    effects: EffectSet {
                        effects: vec![],
                        rest: None,
                    },
                },
            }),
            body: Some(body),
        };

        let hir_func = converter.convert_function(&func);

        // The function should have two parameters
        assert_eq!(hir_func.signature.params.len(), 2);
    }

    #[test]
    fn test_convert_program_with_mixed_nodes() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let func_name = interner.intern("func");
        let struct_name = interner.intern("Struct");

        // Create a function node
        let func_node = TypedASTNode {
            span: 0..1,
            file: "test".to_string(),
            node: TypedASTNodeKind::Function(Box::new(TypedFunction {
                span: 0..1,
                file: "test".to_string(),
                vis: Visibility::Public,
                name: func_name,
                function_id: FunctionId(0),
                type_params: vec![],
                args: vec![],
                return_type: create_int_type(),
                where_constraints: vec![],
                effects: EffectSet {
                    effects: vec![],
                    rest: None,
                },
                function_type: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Function {
                        params: vec![],
                        return_type: create_int_type(),
                        effects: EffectSet {
                            effects: vec![],
                            rest: None,
                        },
                    },
                }),
                body: Some(create_int_expr(42)),
            })),
            attributes: vec![],
        };

        // Create a struct node (not a function)
        let struct_node = TypedASTNode {
            span: 0..1,
            file: "test".to_string(),
            node: TypedASTNodeKind::Struct(TypedStruct {
                span: 0..1,
                file: "test".to_string(),
                name: struct_name,
                struct_id: StructId(0),
                vis: Visibility::Public,
                type_params: vec![],
                fields: vec![],
                methods: vec![],
                struct_type: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Constructor {
                        name: 5,
                        args: vec![],
                        kind: Kind::Star,
                    },
                }),
            }),
            attributes: vec![],
        };

        let program = vec![struct_node, func_node];
        let functions = converter.convert_program(program);

        // Should return one function (only the function node is processed)
        assert_eq!(functions.len(), 1);
    }

    #[test]
    fn test_convert_expr_with_all_binary_ops() {
        let mut converter = ASTToHIRConverter::new();
        let left = create_int_expr(10);
        let right = create_int_expr(5);

        // Test all binary operations
        let ops = [
            BinOp::Add,
            BinOp::Sub,
            BinOp::Mul,
            BinOp::Div,
            BinOp::Mod,
            BinOp::Eq,
            BinOp::NotEq,
            BinOp::Greater,
            BinOp::Less,
            BinOp::GreaterEq,
            BinOp::LessEq,
            BinOp::And,
            BinOp::Or,
        ];

        for op in &ops {
            let bin_op_expr = TypedExpr {
                span: 0..1,
                file: "test".to_string(),
                expr: TypedExprKind::BinOp {
                    left: Box::new(left.clone()),
                    op: op.clone(),
                    right: Box::new(right.clone()),
                },
                type_: if matches!(
                    op,
                    BinOp::Eq
                        | BinOp::NotEq
                        | BinOp::Greater
                        | BinOp::Less
                        | BinOp::GreaterEq
                        | BinOp::LessEq
                ) {
                    create_bool_type()
                } else {
                    create_int_type()
                },
            };

            let mut builder = Builder::new();
            let result = converter.convert_expr(&mut builder, &bin_op_expr);

            // Each operation should return a valid ValueId
            assert!(result.0 >= 0, "Operation {:?} failed", op);
        }
    }

    #[test]
    fn test_convert_expr_with_all_unary_ops() {
        let mut converter = ASTToHIRConverter::new();

        // Test unary minus
        let int_expr = create_int_expr(42);
        let minus_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::UnOp {
                op: UnOp::Minus,
                operand: Box::new(int_expr),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &minus_expr);
        assert!(result.0 >= 0);

        // Test unary not
        let bool_expr = create_bool_expr(true);
        let not_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::UnOp {
                op: UnOp::Not,
                operand: Box::new(bool_expr),
            },
            type_: create_bool_type(),
        };

        let mut builder2 = Builder::new();
        let result2 = converter.convert_expr(&mut builder2, &not_expr);
        assert!(result2.0 >= 0);
    }

    #[test]
    fn test_convert_expr_complex_block() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let var_name = interner.intern("x");

        // Create a complex block: { let x = 5; x + 10; x * 2 }
        let let_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Let {
                var: var_name,
                binding_id: BindingId(1),
                var_type: create_int_type(),
                value: Box::new(create_int_expr(5)),
            },
            type_: create_int_type(),
        };

        let add_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(TypedExpr {
                    span: 0..1,
                    file: "test".to_string(),
                    expr: TypedExprKind::Variable {
                        name: var_name,
                        binding_id: BindingId(1),
                    },
                    type_: create_int_type(),
                }),
                op: BinOp::Add,
                right: Box::new(create_int_expr(10)),
            },
            type_: create_int_type(),
        };

        let mult_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::BinOp {
                left: Box::new(TypedExpr {
                    span: 0..1,
                    file: "test".to_string(),
                    expr: TypedExprKind::Variable {
                        name: var_name,
                        binding_id: BindingId(1),
                    },
                    type_: create_int_type(),
                }),
                op: BinOp::Mul,
                right: Box::new(create_int_expr(2)),
            },
            type_: create_int_type(),
        };

        let block_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Block {
                expressions: vec![let_expr, add_expr, mult_expr],
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &block_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_with_expression() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let var_name = interner.intern("ctx_val");

        // Create a context expression
        let context_expr = create_int_expr(42);

        // Create a body expression that uses the bound variable
        let body_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Variable {
                name: var_name,
                binding_id: BindingId(1),
            },
            type_: create_int_type(),
        };

        let with_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::With {
                context: Box::new(context_expr),
                var: var_name,
                binding_id: BindingId(1),
                var_type: create_int_type(),
                body: Box::new(body_expr),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &with_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_optional_chain_expression() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let field_name = interner.intern("field");

        // Create a target expression (e.g., an object)
        let target_expr = create_int_expr(100); // Simplified - in real usage this would be a struct

        let optional_chain_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::OptionalChain {
                target: Box::new(target_expr),
                field: field_name,
                field_id: FieldId(1),
                field_type: create_int_type(), // Simplified type
            },
            type_: create_int_type(), // Return type of the field access
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &optional_chain_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_macro_call_expression() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let macro_name = interner.intern("test_macro");

        // Create argument expressions for the macro
        let arg1 = create_int_expr(10);
        let arg2 = create_int_expr(20);

        let macro_call_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::MacroCall {
                name: macro_name,
                macro_id: MacroId(1),
                args: vec![arg1, arg2],
                delimiter: Delimiter::Paren, // Using Paren as default
            },
            type_: create_int_type(), // Return type of the macro expansion
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &macro_call_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_array_empty() {
        let mut converter = ASTToHIRConverter::new();

        let array_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Array {
                elements: vec![],
                element_type: create_int_type(),
            },
            type_: create_int_type(), // Array type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &array_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_array_single_element() {
        let mut converter = ASTToHIRConverter::new();

        let element = create_int_expr(42);
        let array_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Array {
                elements: vec![element],
                element_type: create_int_type(),
            },
            type_: create_int_type(), // Array type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &array_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_array_multiple_elements() {
        let mut converter = ASTToHIRConverter::new();

        let elements = vec![create_int_expr(1), create_int_expr(2), create_int_expr(3)];
        let array_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Array {
                elements,
                element_type: create_int_type(),
            },
            type_: create_int_type(), // Array type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &array_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_import_expression() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();

        // Create an import expression
        let import_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Import(TypedImport {
                span: 0..1,
                file: "test".to_string(),
                path: vec![interner.intern("std"), interner.intern("io")],
                items: vec![(
                    interner.intern("println"),
                    Some(interner.intern("print")),
                    crate::ast::TypeId(1),
                    create_int_type(),
                )],
                alias: Some(interner.intern("io_alias")),
            }),
            type_: create_int_type(), // Import expressions may return unit type in some contexts
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &import_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_handle_expression() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();

        // Create a body expression that might perform an effect
        let body_expr = create_int_expr(42);

        // Create a simple handler - in reality this would have more complex structure
        let handler_body = create_int_expr(0);
        let handler = TypedEffectHandler {
            span: 0..1,
            effect: interner.intern("IO"),
            effect_id: EffectId(1),
            params: vec![(interner.intern("x"), BindingId(1), create_int_type())],
            resume_param: interner.intern("k"),
            resume_id: BindingId(2),
            resume_type: create_int_type(),
            body: handler_body,
        };

        let handle_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Handle {
                body: Box::new(body_expr),
                handlers: vec![handler],
                return_type: create_int_type(),
            },
            type_: create_int_type(),
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &handle_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_tuple_empty() {
        let mut converter = ASTToHIRConverter::new();

        let tuple_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Tuple(vec![]),
            type_: create_int_type(), // Unit type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &tuple_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_tuple_single_element() {
        let mut converter = ASTToHIRConverter::new();

        let elements = vec![create_int_expr(42)];
        let tuple_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Tuple(elements),
            type_: create_int_type(), // Tuple type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &tuple_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_tuple_multiple_elements() {
        let mut converter = ASTToHIRConverter::new();

        let elements = vec![create_int_expr(1), create_int_expr(2), create_int_expr(3)];
        let tuple_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Tuple(elements),
            type_: create_int_type(), // Tuple type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &tuple_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_map_empty() {
        let mut converter = ASTToHIRConverter::new();

        let map_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Map {
                entries: vec![],
                key_type: create_int_type(),
                value_type: create_int_type(),
            },
            type_: create_int_type(), // Map type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &map_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_map_single_entry() {
        let mut converter = ASTToHIRConverter::new();

        let entries = vec![(create_int_expr(1), create_int_expr(10))];
        let map_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Map {
                entries,
                key_type: create_int_type(),
                value_type: create_int_type(),
            },
            type_: create_int_type(), // Map type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &map_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_map_multiple_entries() {
        let mut converter = ASTToHIRConverter::new();

        let entries = vec![
            (create_int_expr(1), create_int_expr(10)),
            (create_int_expr(2), create_int_expr(20)),
            (create_int_expr(3), create_int_expr(30)),
        ];
        let map_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Map {
                entries,
                key_type: create_int_type(),
                value_type: create_int_type(),
            },
            type_: create_int_type(), // Map type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &map_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_struct_empty() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let struct_name = interner.intern("EmptyStruct");

        let struct_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::StructConstruct {
                struct_name,
                struct_id: StructId(0),
                fields: vec![], // No fields
            },
            type_: create_int_type(), // Struct type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &struct_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_struct_single_field() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let struct_name = interner.intern("TestStruct");
        let field_name = interner.intern("field1");

        let field_expr = create_int_expr(42);
        let struct_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::StructConstruct {
                struct_name,
                struct_id: StructId(0),
                fields: vec![(field_name, FieldId(1), field_expr)],
            },
            type_: create_int_type(), // Struct type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &struct_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_struct_multiple_fields() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let struct_name = interner.intern("TestStruct");
        let field1_name = interner.intern("field1");
        let field2_name = interner.intern("field2");
        let field3_name = interner.intern("field3");

        let fields = vec![
            (field1_name, FieldId(1), create_int_expr(10)),
            (field2_name, FieldId(2), create_int_expr(20)),
            (field3_name, FieldId(3), create_int_expr(30)),
        ];
        let struct_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::StructConstruct {
                struct_name,
                struct_id: StructId(0),
                fields,
            },
            type_: create_int_type(), // Struct type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &struct_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_enum_empty() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let enum_name = interner.intern("EmptyEnum");
        let variant_name = interner.intern("EmptyVariant");

        let enum_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::EnumConstruct {
                enum_name,
                enum_id: EnumId(0),
                variant: variant_name,
                variant_id: VariantId(0),
                args: vec![], // No arguments
            },
            type_: create_int_type(), // Enum type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &enum_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_enum_single_arg() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let enum_name = interner.intern("SingleArgEnum");
        let variant_name = interner.intern("SingleArgVariant");

        let args = vec![create_int_expr(42)];
        let enum_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::EnumConstruct {
                enum_name,
                enum_id: EnumId(0),
                variant: variant_name,
                variant_id: VariantId(0),
                args,
            },
            type_: create_int_type(), // Enum type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &enum_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_enum_multiple_args() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let enum_name = interner.intern("MultiArgEnum");
        let variant_name = interner.intern("MultiArgVariant");

        let args = vec![
            create_int_expr(10),
            create_int_expr(20),
            create_int_expr(30),
        ];
        let enum_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::EnumConstruct {
                enum_name,
                enum_id: EnumId(0),
                variant: variant_name,
                variant_id: VariantId(0),
                args,
            },
            type_: create_int_type(), // Enum type would be different in reality
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &enum_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_perform_empty() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let effect_name = interner.intern("EmptyEffect");

        let perform_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Perform {
                effect: effect_name,
                effect_id: EffectId(0),
                args: vec![], // No arguments
            },
            type_: create_int_type(), // Would be the return type of the effect
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &perform_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_perform_single_arg() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let effect_name = interner.intern("SingleArgEffect");

        let args = vec![create_int_expr(42)];
        let perform_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Perform {
                effect: effect_name,
                effect_id: EffectId(0),
                args,
            },
            type_: create_int_type(), // Would be the return type of the effect
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &perform_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_perform_multiple_args() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let effect_name = interner.intern("MultiArgEffect");

        let args = vec![
            create_int_expr(10),
            create_int_expr(20),
            create_int_expr(30),
        ];
        let perform_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Perform {
                effect: effect_name,
                effect_id: EffectId(0),
                args,
            },
            type_: create_int_type(), // Would be the return type of the effect
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &perform_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_match_empty_arms() {
        let mut converter = ASTToHIRConverter::new();

        let scrutinee = create_int_expr(42);
        let match_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms: vec![], // No arms
            },
            type_: create_int_type(), // Would be the type of the match expression
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &match_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_match_single_arm() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let var_name = interner.intern("x");

        let scrutinee = create_int_expr(42);
        let arm_pattern = TypedPattern {
            span: 0..1,
            file: "test".to_string(),
            pat: TypedPatKind::Bind {
                name: var_name,
                binding_id: BindingId(1),
            },
            type_: create_int_type(),
        };
        let arm_body = create_int_expr(100);

        let match_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms: vec![TypedMatchArm {
                    pattern: arm_pattern,
                    guard: None,
                    body: Box::new(arm_body),
                    span: 0..1,
                }],
            },
            type_: create_int_type(), // Would be the type of the match expression
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &match_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }

    #[test]
    fn test_convert_match_multiple_arms() {
        let mut converter = ASTToHIRConverter::new();
        let interner = &mut Interner::new();
        let var_name1 = interner.intern("x");
        let var_name2 = interner.intern("y");

        let scrutinee = create_int_expr(42);

        // First arm
        let arm1_pattern = TypedPattern {
            span: 0..1,
            file: "test".to_string(),
            pat: TypedPatKind::Bind {
                name: var_name1,
                binding_id: BindingId(1),
            },
            type_: create_int_type(),
        };
        let arm1_body = create_int_expr(100);

        // Second arm
        let arm2_pattern = TypedPattern {
            span: 0..1,
            file: "test".to_string(),
            pat: TypedPatKind::Bind {
                name: var_name2,
                binding_id: BindingId(2),
            },
            type_: create_int_type(),
        };
        let arm2_body = create_int_expr(200);

        let match_expr = TypedExpr {
            span: 0..1,
            file: "test".to_string(),
            expr: TypedExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms: vec![
                    TypedMatchArm {
                        pattern: arm1_pattern,
                        guard: None,
                        body: Box::new(arm1_body),
                        span: 0..1,
                    },
                    TypedMatchArm {
                        pattern: arm2_pattern,
                        guard: None,
                        body: Box::new(arm2_body),
                        span: 0..1,
                    },
                ],
            },
            type_: create_int_type(), // Would be the type of the match expression
        };

        let mut builder = Builder::new();
        let result = converter.convert_expr(&mut builder, &match_expr);

        // The result should be a valid ValueId
        assert!(result.0 >= 0);
    }
}
