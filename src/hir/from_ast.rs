//! Conversion from Typed AST to HIR (High-level Intermediate Representation)
use crate::ast::EffectId;
use crate::ast::{
    TypedASTNode, TypedASTNodeKind, TypedEffectDef, TypedEnum, TypedExpr, TypedExprKind,
    TypedFunction, TypedImpl, TypedMacroDef, TypedPatKind, TypedPattern, TypedStruct,
    TypedTraitDef, TypedTypeAlias,
};
use crate::hir::{
    AllocationPreference, BlockId, Builder, Function, FunctionSignature, Opcode, Param,
    TypeContext, TypeId, ValueId,
};
use crate::typechecker::{Type as TypeType, TypeKind};
use std::collections::HashMap;
use std::rc::Rc;

pub struct ASTToHIRConverter {
    // Map from AST binding IDs to HIR value IDs
    value_map: HashMap<crate::ast::BindingId, ValueId>,
    // Map from effect IDs to their runtime implementations (ValueId)
    effect_map: HashMap<EffectId, ValueId>,
    // Stack to keep track of loop contexts for break/continue
    loop_contexts: Vec<(BlockId, BlockId, BlockId)>, // (loop_continue_block, loop_body_block, loop_exit_block)
    // Type context for managing type IDs
    type_context: TypeContext,
}

impl ASTToHIRConverter {
    pub fn new() -> Self {
        Self {
            value_map: HashMap::new(),
            effect_map: HashMap::new(),
            loop_contexts: Vec::new(),
            type_context: TypeContext::new(),
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
            // Return a default value of the expected return type
            builder.const_int(0, self.convert_type(&func.return_type))
        };

        // Add a return instruction to the current block if not already added by a return statement
        // Check if the current block already has a terminator by attempting to add one
        // For now, we'll just add the return - the builder should handle duplicates appropriately
        let _return_terminator = builder.ret(Some(result_value));

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
                let hir_type = self.convert_type(&expr.type_);
                builder.const_int(*value as i128, hir_type)
            }
            TypedExprKind::Float(value) => {
                let hir_type = self.convert_type(&expr.type_);
                builder.const_float(*value, hir_type)
            }
            TypedExprKind::Bool(value) => builder.const_bool(*value),
            TypedExprKind::String(value) => {
                let hir_type = self.convert_type(&expr.type_);
                builder.const_string(value, hir_type)
            }
            TypedExprKind::Variable { binding_id, .. } => {
                if let Some(&value_id) = self.value_map.get(binding_id) {
                    value_id
                } else {
                    // If the variable is not in the value map, it means it's not bound in the current scope
                    // This can happen in tests or in cases where the variable is not properly bound
                    // For now, return a default value - in a real implementation, this would be an error
                    ValueId(0)
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
                    crate::ast::BinOp::Pow => Opcode::Pow,
                    crate::ast::BinOp::Eq => Opcode::Eq,
                    crate::ast::BinOp::Greater => Opcode::Gt,
                    crate::ast::BinOp::Less => Opcode::Lt,
                    crate::ast::BinOp::GreaterEq => Opcode::Ge,
                    crate::ast::BinOp::LessEq => Opcode::Le,
                    crate::ast::BinOp::NotEq => Opcode::Ne,
                    crate::ast::BinOp::Pipe => {
                        // The pipe operator is typically used for function chaining/composition
                        // For example: x |> f is equivalent to f(x)
                        // We need to convert this to a function call: f(x)
                        // right_val is the function, left_val is the argument
                        let func_val = right_val;
                        let arg_val = left_val;
                        return builder.call(
                            func_val,
                            vec![arg_val],
                            self.convert_type(&expr.type_),
                        );
                    }
                    crate::ast::BinOp::And => Opcode::BitAnd,
                    crate::ast::BinOp::Or => Opcode::BitOr,
                    crate::ast::BinOp::Nor => {
                        // NOR is NOT OR, so we need to compute OR and then NOT
                        // First compute OR
                        let or_result = builder.binary_op(
                            Opcode::BitOr,
                            left_val,
                            right_val,
                            self.convert_type(&expr.type_),
                        );
                        // Then compute NOT of the OR result
                        let true_val = builder.const_bool(true);
                        return builder.binary_op(
                            Opcode::BitXor,
                            or_result,
                            true_val,
                            self.convert_type(&expr.type_),
                        );
                    }
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
            TypedExprKind::Assign { l_val, r_val, op } => {
                // Handle assignment operations
                // For now, we'll focus on simple variable assignments
                // In a real implementation, you'd need to handle complex l-values like field access, array indexing, etc.

                let right_val = self.convert_expr(builder, r_val);

                // For simple variable assignment, we need to identify the target variable
                // and update it in our value map
                match op {
                    crate::ast::AssignOp::Assign => {
                        // Simple assignment: var = value
                        // Handle the l-value and store the result
                        match &l_val.expr {
                            TypedExprKind::Variable { binding_id, .. } => {
                                // Simple variable assignment - update the value map
                                self.value_map.insert(*binding_id, right_val);
                                right_val
                            }
                            TypedExprKind::FieldAccess {
                                target,
                                field_id,
                                field_type,
                                ..
                            } => {
                                // Handle field assignment: obj.field = value
                                let target_val = self.convert_expr(builder, target);
                                // Get pointer to the field using the field index
                                let index_val = builder
                                    .const_int(field_id.0 as i128, self.convert_type(field_type));
                                let field_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Store the right value into the field
                                builder.store(field_ptr, right_val);
                                right_val
                            }
                            TypedExprKind::Index { target, index, .. } => {
                                // Handle index assignment: arr[index] = value
                                let target_val = self.convert_expr(builder, target);
                                let index_val = self.convert_expr(builder, index);
                                let element_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Store the right value into the element
                                builder.store(element_ptr, right_val);
                                right_val
                            }
                            _ => {
                                // For unsupported l-value types, panic with an error
                                panic!("Unsupported l-value type for assignment: {:?}", l_val.expr);
                            }
                        }
                    }
                    crate::ast::AssignOp::AddAssign => {
                        // Addition assignment: var += value
                        // Load current value of var, add right value, store back to var
                        match &l_val.expr {
                            TypedExprKind::Variable { binding_id, .. } => {
                                if let Some(current_val) = self.value_map.get(binding_id) {
                                    let new_val = builder.binary_op(
                                        Opcode::Add,
                                        *current_val,
                                        right_val,
                                        self.convert_type(&expr.type_),
                                    );
                                    self.value_map.insert(*binding_id, new_val);
                                    new_val
                                } else {
                                    // Variable not in map - return the right value as fallback
                                    right_val
                                }
                            }
                            TypedExprKind::FieldAccess {
                                target,
                                field_id,
                                field_type,
                                ..
                            } => {
                                // Handle field assignment: obj.field += value
                                let target_val = self.convert_expr(builder, target);
                                // Get pointer to the field using the field index
                                let index_val = builder
                                    .const_int(field_id.0 as i128, self.convert_type(field_type));
                                let field_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Load current value of the field
                                let current_field_val =
                                    builder.load(field_ptr, self.convert_type(&l_val.type_));
                                // Perform addition
                                let new_val = builder.binary_op(
                                    Opcode::Add,
                                    current_field_val,
                                    right_val,
                                    self.convert_type(&expr.type_),
                                );
                                // Store the new value back to the field
                                builder.store(field_ptr, new_val);
                                new_val
                            }
                            TypedExprKind::Index { target, index, .. } => {
                                // Handle index assignment: arr[index] += value
                                let target_val = self.convert_expr(builder, target);
                                let index_val = self.convert_expr(builder, index);
                                let element_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Load current value of the element
                                let current_element_val =
                                    builder.load(element_ptr, self.convert_type(&l_val.type_));
                                // Perform addition
                                let new_val = builder.binary_op(
                                    Opcode::Add,
                                    current_element_val,
                                    right_val,
                                    self.convert_type(&expr.type_),
                                );
                                // Store the new value back to the element
                                builder.store(element_ptr, new_val);
                                new_val
                            }
                            _ => {
                                // For unsupported l-value types, panic with an error
                                panic!(
                                    "Unsupported l-value type for += assignment: {:?}",
                                    l_val.expr
                                );
                            }
                        }
                    }
                    crate::ast::AssignOp::SubAssign => {
                        // Subtraction assignment: var -= value
                        match &l_val.expr {
                            TypedExprKind::Variable { binding_id, .. } => {
                                if let Some(current_val) = self.value_map.get(binding_id) {
                                    let new_val = builder.binary_op(
                                        Opcode::Sub,
                                        *current_val,
                                        right_val,
                                        self.convert_type(&expr.type_),
                                    );
                                    self.value_map.insert(*binding_id, new_val);
                                    new_val
                                } else {
                                    right_val
                                }
                            }
                            TypedExprKind::FieldAccess {
                                target,
                                field_id,
                                field_type,
                                ..
                            } => {
                                // Handle field assignment: obj.field -= value
                                let target_val = self.convert_expr(builder, target);
                                // Get pointer to the field using the field index
                                let index_val = builder
                                    .const_int(field_id.0 as i128, self.convert_type(field_type));
                                let field_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Load current value of the field
                                let current_field_val =
                                    builder.load(field_ptr, self.convert_type(&l_val.type_));
                                // Perform subtraction
                                let new_val = builder.binary_op(
                                    Opcode::Sub,
                                    current_field_val,
                                    right_val,
                                    self.convert_type(&expr.type_),
                                );
                                // Store the new value back to the field
                                builder.store(field_ptr, new_val);
                                new_val
                            }
                            TypedExprKind::Index { target, index, .. } => {
                                // Handle index assignment: arr[index] -= value
                                let target_val = self.convert_expr(builder, target);
                                let index_val = self.convert_expr(builder, index);
                                let element_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Load current value of the element
                                let current_element_val =
                                    builder.load(element_ptr, self.convert_type(&l_val.type_));
                                // Perform subtraction
                                let new_val = builder.binary_op(
                                    Opcode::Sub,
                                    current_element_val,
                                    right_val,
                                    self.convert_type(&expr.type_),
                                );
                                // Store the new value back to the element
                                builder.store(element_ptr, new_val);
                                new_val
                            }
                            _ => {
                                // For unsupported l-value types, panic with an error
                                panic!(
                                    "Unsupported l-value type for -= assignment: {:?}",
                                    l_val.expr
                                );
                            }
                        }
                    }
                    crate::ast::AssignOp::MulAssign => {
                        // Multiplication assignment: var *= value
                        match &l_val.expr {
                            TypedExprKind::Variable { binding_id, .. } => {
                                if let Some(current_val) = self.value_map.get(binding_id) {
                                    let new_val = builder.binary_op(
                                        Opcode::Mul,
                                        *current_val,
                                        right_val,
                                        self.convert_type(&expr.type_),
                                    );
                                    self.value_map.insert(*binding_id, new_val);
                                    new_val
                                } else {
                                    right_val
                                }
                            }
                            TypedExprKind::FieldAccess {
                                target,
                                field_id,
                                field_type,
                                ..
                            } => {
                                // Handle field assignment: obj.field *= value
                                let target_val = self.convert_expr(builder, target);
                                // Get pointer to the field using the field index
                                let index_val = builder
                                    .const_int(field_id.0 as i128, self.convert_type(field_type));
                                let field_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Load current value of the field
                                let current_field_val =
                                    builder.load(field_ptr, self.convert_type(&l_val.type_));
                                // Perform multiplication
                                let new_val = builder.binary_op(
                                    Opcode::Mul,
                                    current_field_val,
                                    right_val,
                                    self.convert_type(&expr.type_),
                                );
                                // Store the new value back to the field
                                builder.store(field_ptr, new_val);
                                new_val
                            }
                            TypedExprKind::Index { target, index, .. } => {
                                // Handle index assignment: arr[index] *= value
                                let target_val = self.convert_expr(builder, target);
                                let index_val = self.convert_expr(builder, index);
                                let element_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Load current value of the element
                                let current_element_val =
                                    builder.load(element_ptr, self.convert_type(&l_val.type_));
                                // Perform multiplication
                                let new_val = builder.binary_op(
                                    Opcode::Mul,
                                    current_element_val,
                                    right_val,
                                    self.convert_type(&expr.type_),
                                );
                                // Store the new value back to the element
                                builder.store(element_ptr, new_val);
                                new_val
                            }
                            _ => {
                                // For unsupported l-value types, panic with an error
                                panic!(
                                    "Unsupported l-value type for *= assignment: {:?}",
                                    l_val.expr
                                );
                            }
                        }
                    }
                    crate::ast::AssignOp::DivAssign => {
                        // Division assignment: var /= value
                        match &l_val.expr {
                            TypedExprKind::Variable { binding_id, .. } => {
                                if let Some(current_val) = self.value_map.get(binding_id) {
                                    let new_val = builder.binary_op(
                                        Opcode::Div,
                                        *current_val,
                                        right_val,
                                        self.convert_type(&expr.type_),
                                    );
                                    self.value_map.insert(*binding_id, new_val);
                                    new_val
                                } else {
                                    right_val
                                }
                            }
                            TypedExprKind::FieldAccess {
                                target,
                                field_id,
                                field_type,
                                ..
                            } => {
                                // Handle field assignment: obj.field /= value
                                let target_val = self.convert_expr(builder, target);
                                // Get pointer to the field using the field index
                                let index_val = builder
                                    .const_int(field_id.0 as i128, self.convert_type(field_type));
                                let field_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Load current value of the field
                                let current_field_val =
                                    builder.load(field_ptr, self.convert_type(&l_val.type_));
                                // Perform division
                                let new_val = builder.binary_op(
                                    Opcode::Div,
                                    current_field_val,
                                    right_val,
                                    self.convert_type(&expr.type_),
                                );
                                // Store the new value back to the field
                                builder.store(field_ptr, new_val);
                                new_val
                            }
                            TypedExprKind::Index { target, index, .. } => {
                                // Handle index assignment: arr[index] /= value
                                let target_val = self.convert_expr(builder, target);
                                let index_val = self.convert_expr(builder, index);
                                let element_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Load current value of the element
                                let current_element_val =
                                    builder.load(element_ptr, self.convert_type(&l_val.type_));
                                // Perform division
                                let new_val = builder.binary_op(
                                    Opcode::Div,
                                    current_element_val,
                                    right_val,
                                    self.convert_type(&expr.type_),
                                );
                                // Store the new value back to the element
                                builder.store(element_ptr, new_val);
                                new_val
                            }
                            _ => {
                                // For unsupported l-value types, panic with an error
                                panic!(
                                    "Unsupported l-value type for /= assignment: {:?}",
                                    l_val.expr
                                );
                            }
                        }
                    }
                    crate::ast::AssignOp::ModAssign => {
                        // Modulo assignment: var %= value
                        match &l_val.expr {
                            TypedExprKind::Variable { binding_id, .. } => {
                                if let Some(current_val) = self.value_map.get(binding_id) {
                                    let new_val = builder.binary_op(
                                        Opcode::Rem,
                                        *current_val,
                                        right_val,
                                        self.convert_type(&expr.type_),
                                    );
                                    self.value_map.insert(*binding_id, new_val);
                                    new_val
                                } else {
                                    right_val
                                }
                            }
                            TypedExprKind::FieldAccess {
                                target,
                                field_id,
                                field_type,
                                ..
                            } => {
                                // Handle field assignment: obj.field %= value
                                let target_val = self.convert_expr(builder, target);
                                // Get pointer to the field using the field index
                                let index_val = builder
                                    .const_int(field_id.0 as i128, self.convert_type(field_type));
                                let field_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Load current value of the field
                                let current_field_val =
                                    builder.load(field_ptr, self.convert_type(&l_val.type_));
                                // Perform modulo operation
                                let new_val = builder.binary_op(
                                    Opcode::Rem,
                                    current_field_val,
                                    right_val,
                                    self.convert_type(&expr.type_),
                                );
                                // Store the new value back to the field
                                builder.store(field_ptr, new_val);
                                new_val
                            }
                            TypedExprKind::Index { target, index, .. } => {
                                // Handle index assignment: arr[index] %= value
                                let target_val = self.convert_expr(builder, target);
                                let index_val = self.convert_expr(builder, index);
                                let element_ptr = builder.get_element_ptr(
                                    target_val,
                                    vec![index_val],
                                    self.convert_type(&l_val.type_),
                                );
                                // Load current value of the element
                                let current_element_val =
                                    builder.load(element_ptr, self.convert_type(&l_val.type_));
                                // Perform modulo operation
                                let new_val = builder.binary_op(
                                    Opcode::Rem,
                                    current_element_val,
                                    right_val,
                                    self.convert_type(&expr.type_),
                                );
                                // Store the new value back to the element
                                builder.store(element_ptr, new_val);
                                new_val
                            }
                            _ => {
                                // For unsupported l-value types, panic with an error
                                panic!(
                                    "Unsupported l-value type for %= assignment: {:?}",
                                    l_val.expr
                                );
                            }
                        }
                    }
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
            TypedExprKind::Array {
                elements,
                element_type,
            } => {
                // Handle array construction
                let element_values: Vec<ValueId> = elements
                    .iter()
                    .map(|elem| self.convert_expr(builder, elem))
                    .collect();

                if element_values.is_empty() {
                    // For empty arrays, we still need to allocate an array structure
                    let size_val = builder.const_int(0, self.convert_type(&expr.type_));
                    builder.alloc_array(
                        self.convert_type(element_type), // element type
                        size_val,
                        false, // stack allocation for now
                    )
                } else {
                    // Calculate array size
                    let size_val = builder
                        .const_int(element_values.len() as i128, self.convert_type(&expr.type_));

                    // Allocate array storage - for now we'll use stack allocation
                    let array_ptr = builder.alloc_array(
                        self.convert_type(element_type), // element type
                        size_val,
                        false, // stack allocation for now
                    );

                    // Store each element in the array
                    for (i, &element_val) in element_values.iter().enumerate() {
                        // Get pointer to the i-th element
                        let index_val =
                            builder.const_int(i as i128, self.convert_type(&expr.type_));
                        let element_ptr = builder.get_element_ptr(
                            array_ptr,
                            vec![index_val],
                            self.convert_type(element_type),
                        );
                        // Store the element value at that location
                        builder.store(element_ptr, element_val);
                    }

                    array_ptr
                }
            }
            TypedExprKind::Tuple(elements) => {
                // Handle tuple construction
                let element_values: Vec<ValueId> = elements
                    .iter()
                    .map(|elem| self.convert_expr(builder, elem))
                    .collect();

                if element_values.is_empty() {
                    // For empty tuples (unit type), return a default value
                    builder.const_int(0, self.convert_type(&expr.type_))
                } else {
                    // Get element types for the tuple
                    let element_types: Vec<TypeId> = elements
                        .iter()
                        .map(|elem| self.convert_type(&elem.type_))
                        .collect();

                    // Create tuple allocation
                    let tuple_ptr = builder.alloc_tuple(&element_types, false); // Stack allocation for now

                    // Store each element in the tuple at the appropriate index
                    for (i, &element_val) in element_values.iter().enumerate() {
                        // Get pointer to the i-th element
                        let index_val =
                            builder.const_int(i as i128, self.convert_type(&expr.type_));
                        let element_ptr = builder.get_element_ptr(
                            tuple_ptr,
                            vec![index_val],
                            self.convert_type(&elements[i].type_),
                        );
                        // Store the element value at that location
                        builder.store(element_ptr, element_val);
                    }

                    tuple_ptr
                }
            }
            TypedExprKind::Map {
                entries,
                key_type,
                value_type,
            } => {
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

                // Create map allocation
                builder.alloc_map(
                    self.convert_type(key_type),
                    self.convert_type(value_type),
                    true, // Use heap allocation for maps
                )
                // In a real implementation, we'd initialize the map with the entries
                // For now, we return the pointer to the allocated map
            }
            TypedExprKind::EnumConstruct {
                args, variant_id, ..
            } => {
                // Handle enum construction
                let arg_values: Vec<ValueId> = args
                    .iter()
                    .map(|arg| self.convert_expr(builder, arg))
                    .collect();

                // Create enum allocation
                let enum_ptr = builder.alloc_enum(
                    self.convert_type(&expr.type_), // tag type
                    self.convert_type(&expr.type_), // data type (simplified)
                    true,                           // heap allocation for enums with data
                );

                // Store the variant ID (tag) in the first part of the enum
                // Use builder.const_int instead of directly creating ValueId from variant_id
                let tag_val =
                    builder.const_int(variant_id.0 as i128, self.convert_type(&expr.type_)); // Use the variant ID as the tag
                let index_val = builder.const_int(0, self.convert_type(&expr.type_));
                let tag_ptr = builder.get_element_ptr(
                    enum_ptr,
                    vec![index_val],
                    self.convert_type(&expr.type_),
                );
                builder.store(tag_ptr, tag_val);

                // Store the arguments in the data part of the enum (if any)
                if !arg_values.is_empty() {
                    // For each argument, store it in the appropriate location
                    for (i, &arg_val) in arg_values.iter().enumerate() {
                        let index_val =
                            builder.const_int((i + 1) as i128, self.convert_type(&expr.type_));
                        let data_ptr = builder.get_element_ptr(
                            enum_ptr,
                            vec![index_val],
                            self.convert_type(&args[i].type_),
                        );
                        builder.store(data_ptr, arg_val);
                    }
                }

                enum_ptr
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

                // Get field types for the struct
                let field_types: Vec<TypeId> = fields
                    .iter()
                    .map(|(_, _, expr)| self.convert_type(&expr.type_))
                    .collect();

                // Create struct allocation
                let struct_ptr = builder.alloc_struct(&field_types, false); // Stack allocation for now

                // Initialize the struct fields with their values
                for (i, &field_val) in field_values.iter().enumerate() {
                    // Get pointer to the i-th field
                    let index_val =
                        builder.const_int(i as i128, self.convert_type(&fields[i].2.type_));
                    let field_ptr = builder.get_element_ptr(
                        struct_ptr,
                        vec![index_val],
                        self.convert_type(&fields[i].2.type_),
                    );
                    // Store the field value at that location
                    builder.store(field_ptr, field_val);
                }

                struct_ptr
            }
            TypedExprKind::Perform {
                args, effect_id, ..
            } => {
                // Handle effect perform with proper continuation passing
                let arg_values: Vec<ValueId> = args
                    .iter()
                    .map(|arg| self.convert_expr(builder, arg))
                    .collect();

                // Look up the runtime implementation for the effect
                // In a real implementation, this would involve effect handling with proper
                // continuation passing and control flow. For now, we'll look up the effect
                // implementation and call it if found.
                if let Some(effect_func_id) = self.get_effect_implementation(*effect_id) {
                    // Call the effect operation with the arguments
                    builder.call(effect_func_id, arg_values, self.convert_type(&expr.type_))
                } else {
                    // Create a dedicated effect invocation instruction for unimplemented effects
                    // This represents a runtime effect operation that needs to be handled by effect handlers
                    let effect_id_val = ValueId(effect_id.0); // Use the effect ID as a value
                    builder.invoke_effect(effect_id_val, arg_values, self.convert_type(&expr.type_))
                }
            }
            TypedExprKind::Handle {
                body,
                handlers,
                return_type,
            } => {
                // Set up proper continuation-passing style for effect handlers
                // This is a more complete implementation of effect handling

                // Create the main computation block
                let main_block = builder.create_block();
                // let continue_block = builder.create_block(); // For future use in full CPS implementation

                // Switch to main block and convert the body
                builder.switch_to_block(main_block);
                let body_result = self.convert_expr(builder, body);

                // Process handlers for each effect operation
                // In a real implementation, we would create proper continuation-passing style
                // and handler functions for each effect operation
                for handler in handlers {
                    // Process handler parameters and set up the handler function
                    // Each handler defines how to resume computation after an effect
                    let _handler_body = self.convert_expr(builder, &handler.body);

                    // In a real implementation, we would set up the continuation
                    // and create proper control flow for effect resumption
                    // This involves creating closures that capture the current continuation
                    // and pass it to the handler when an effect is performed
                }

                // Create a phi node to merge results from different execution paths
                // For now, just return the body result
                // In a complete implementation, this would handle all possible return paths
                let result_val = ValueId(builder.get_next_value_id());
                builder.add_phi_node(
                    result_val,
                    self.convert_type(return_type),
                    vec![(body_result, main_block)], // Add other possible paths in a full implementation
                );

                result_val
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
                // We'll use empty args for now, but in a real implementation we'd pass values
                let _branch = builder.branch(cond_val, then_block, else_block, vec![], vec![]);

                // Convert then branch
                builder.switch_to_block(then_block);
                let then_val = self.convert_expr(builder, then);
                let _then_jump = builder.jump(merge_block, vec![then_val]);

                // Convert else branch
                builder.switch_to_block(else_block);
                let else_val = if let Some(else_expr) = else_ {
                    self.convert_expr(builder, else_expr)
                } else {
                    // For unit type (no else branch), return a default value
                    builder.const_int(0, self.convert_type(&expr.type_))
                };
                let _else_jump = builder.jump(merge_block, vec![else_val]);

                // Switch back to the merge block
                builder.switch_to_block(merge_block);

                // Create a phi node to merge the values from both branches
                // For now, we'll return a new value ID that represents the merged result
                // In a real implementation, we'd properly create and handle the phi node
                let result_val = ValueId(builder.get_next_value_id());

                // Add the phi node to the current block
                builder.add_phi_node(
                    result_val,
                    self.convert_type(&expr.type_),
                    vec![(then_val, then_block), (else_val, else_block)],
                );

                result_val
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
                // Handle loops - create a proper loop structure with entry, body, and exit blocks
                let loop_entry_block = builder.create_block();
                let loop_body_block = builder.create_block();
                let loop_exit_block = builder.create_block();

                // Push the loop context for break/continue handling
                self.loop_contexts
                    .push((loop_entry_block, loop_body_block, loop_exit_block));

                // Jump to the loop entry block
                let _entry_jump = builder.jump(loop_entry_block, vec![]);

                // Switch to the loop entry block
                builder.switch_to_block(loop_entry_block);

                // Jump to the loop body block (for infinite loop)
                let _to_body_jump = builder.jump(loop_body_block, vec![]);

                // Switch to the loop body block
                builder.switch_to_block(loop_body_block);

                // Convert the loop body
                let body_val = self.convert_expr(builder, body);

                // Jump back to the loop entry to continue the loop
                let _continue_jump = builder.jump(loop_entry_block, vec![]);

                // Pop the loop context
                self.loop_contexts.pop();

                // Switch to the exit block (this would be reached by break)
                builder.switch_to_block(loop_exit_block);

                // Return the body value
                body_val
            }
            TypedExprKind::Match { scrutinee, arms } => {
                // Handle match expressions - this is a complex control flow operation
                // We'll implement basic pattern matching by checking the scrutinee value against patterns

                // Convert the scrutinee
                let scrutinee_val = self.convert_expr(builder, scrutinee);

                if arms.is_empty() {
                    // No arms - return a default value
                    builder.const_int(0, self.convert_type(&expr.type_))
                } else {
                    // Create blocks for each arm plus a merge block
                    let mut arm_blocks = Vec::new();
                    let merge_block = builder.create_block();
                    let mut condition_blocks = Vec::new(); // blocks to check each pattern
                    let mut arm_results = Vec::new(); // Store results from each arm

                    // Create a condition block for each arm (except the last which is the default)
                    for i in 0..arms.len() {
                        arm_blocks.push(builder.create_block());
                        if i < arms.len() - 1 {
                            condition_blocks.push(builder.create_block());
                        }
                    }

                    // Start with the first condition block or go directly to the last arm if only one arm
                    if condition_blocks.is_empty() {
                        // Only one arm - just go to that arm directly
                        let _jump_to_arm = builder.jump(arm_blocks[0], vec![]);
                    } else {
                        // Multiple arms - start with the first condition check
                        let _jump_to_first_condition = builder.jump(condition_blocks[0], vec![]);
                    }

                    // Process each arm
                    for (i, arm) in arms.iter().enumerate() {
                        // Store original value map to restore after each arm
                        let original_value_map = self.value_map.clone();

                        if i < arms.len() - 1 {
                            // This is not the last arm, so we need to check if it matches
                            builder.switch_to_block(condition_blocks[i]);

                            // Check if the pattern matches the scrutinee
                            let pattern_matches =
                                self.check_pattern_match(builder, scrutinee_val, &arm.pattern);

                            // Create conditional branch: if pattern matches, go to this arm, else go to next condition
                            let next_condition_or_arm = if i + 1 < condition_blocks.len() {
                                condition_blocks[i + 1] // More conditions to check
                            } else {
                                arm_blocks[i + 1] // Last arm (default case)
                            };
                            let _branch = builder.branch(
                                pattern_matches,
                                arm_blocks[i],
                                next_condition_or_arm,
                                vec![],
                                vec![],
                            );
                        } else {
                            // Last arm - this is the default case
                            if !condition_blocks.is_empty() {
                                // If there were conditions before, we need to be in the last arm block
                                // The last arm is reached when all previous conditions fail
                                builder.switch_to_block(arm_blocks[i]);
                            } else {
                                // Only one arm, so we might already be in the right block
                                builder.switch_to_block(arm_blocks[i]);
                            }
                        }

                        // Convert the arm body
                        let arm_result = self.convert_expr(builder, &arm.body);
                        arm_results.push((arm_result, arm_blocks[i]));

                        // Jump to merge block
                        let _jump_to_merge = builder.jump(merge_block, vec![]);

                        // Restore value map to avoid variable bindings from one arm affecting another
                        self.value_map = original_value_map;
                    }

                    // Switch to merge block
                    builder.switch_to_block(merge_block);

                    // Create a phi node to merge the values from all arms
                    let result_val = ValueId(builder.get_next_value_id());

                    // Add the phi node with results from all arms
                    if !arm_results.is_empty() {
                        builder.add_phi_node(
                            result_val,
                            self.convert_type(&expr.type_),
                            arm_results,
                        );
                    }

                    result_val
                }
            }
            TypedExprKind::For { iterator, body, .. } => {
                // Handle for loops - convert the iterator and body
                let _iterator_val = self.convert_expr(builder, iterator);

                // Create blocks for the for loop: condition, body, and exit
                let loop_condition_block = builder.create_block();
                let loop_body_block = builder.create_block();
                let loop_exit_block = builder.create_block();

                // Push the loop context for break/continue handling
                self.loop_contexts
                    .push((loop_condition_block, loop_body_block, loop_exit_block));

                // Jump to condition check
                let _jump_to_condition = builder.jump(loop_condition_block, vec![]);

                // Switch to condition block
                builder.switch_to_block(loop_condition_block);
                // In a real implementation, we'd check if there are more elements in the iterator
                let dummy_cond = builder.const_bool(true); // Placeholder for actual condition
                let _branch =
                    builder.branch(dummy_cond, loop_body_block, loop_exit_block, vec![], vec![]);

                // Switch to body block
                builder.switch_to_block(loop_body_block);
                let body_val = self.convert_expr(builder, body);

                // Jump back to condition to continue the loop
                let _jump_back_to_condition = builder.jump(loop_condition_block, vec![]);

                // Pop the loop context
                self.loop_contexts.pop();

                // Switch to exit block (would be reached when loop ends)
                builder.switch_to_block(loop_exit_block);

                // Return the body value as the loop's result
                body_val
            }
            TypedExprKind::While { condition, body } => {
                // Handle while loops - create a proper loop structure
                let cond_block = builder.create_block();
                let body_block = builder.create_block();
                let exit_block = builder.create_block();

                // Push the loop context for break/continue handling
                self.loop_contexts
                    .push((cond_block, body_block, exit_block));

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

                // Pop the loop context
                self.loop_contexts.pop();

                // Switch to exit block
                builder.switch_to_block(exit_block);

                // Return a default value (while loops typically return unit)
                builder.const_int(0, self.convert_type(&expr.type_))
            }
            TypedExprKind::IfLet {
                pattern,
                expr,
                then,
                else_,
            } => {
                // Handle if-let expressions - convert to a conditional based on pattern match
                let scrutinee_val = self.convert_expr(builder, expr);

                // Create blocks for the conditional
                let then_block = builder.create_block();
                let else_block = builder.create_block();
                let merge_block = builder.create_block();

                // Check if the scrutinee matches the pattern by using pattern matching logic
                // For now, we'll call the pattern matching helper function
                let pattern_matches = self.check_pattern_match(builder, scrutinee_val, pattern);

                // Create the branch instruction
                let _branch =
                    builder.branch(pattern_matches, then_block, else_block, vec![], vec![]);

                // Convert then branch
                builder.switch_to_block(then_block);
                let then_val = self.convert_expr(builder, then);
                let _then_jump = builder.jump(merge_block, vec![then_val]);

                // Convert else branch
                builder.switch_to_block(else_block);
                let else_val = if let Some(else_expr) = else_ {
                    self.convert_expr(builder, else_expr)
                } else {
                    // For unit type (no else branch), return a default value
                    builder.const_int(0, self.convert_type(&expr.type_))
                };
                let _else_jump = builder.jump(merge_block, vec![else_val]);

                // Switch back to the merge block
                builder.switch_to_block(merge_block);

                // Create a phi node to merge the values from both branches
                let result_val = ValueId(builder.get_next_value_id());

                // Add the phi node to the current block
                builder.add_phi_node(
                    result_val,
                    self.convert_type(&expr.type_),
                    vec![(then_val, then_block), (else_val, else_block)],
                );

                result_val
            }
            TypedExprKind::WhileLet { expr, body, .. } => {
                // Handle while-let expressions - convert to a loop with pattern matching
                // Create blocks for the while-let loop: condition check, body, and exit
                let loop_condition_block = builder.create_block();
                let loop_body_block = builder.create_block();
                let loop_exit_block = builder.create_block();

                // Push the loop context for break/continue handling
                self.loop_contexts
                    .push((loop_condition_block, loop_body_block, loop_exit_block));

                // Jump to condition check
                let _jump_to_condition = builder.jump(loop_condition_block, vec![]);

                // Switch to condition block
                builder.switch_to_block(loop_condition_block);

                // Convert the expression and check if it matches the pattern (placeholder)
                let _expr_val = self.convert_expr(builder, expr);
                let dummy_cond = builder.const_bool(true); // Placeholder for actual pattern matching
                let _branch =
                    builder.branch(dummy_cond, loop_body_block, loop_exit_block, vec![], vec![]);

                // Switch to body block
                builder.switch_to_block(loop_body_block);
                let body_val = self.convert_expr(builder, body);

                // Jump back to condition to continue the loop
                let _jump_back_to_condition = builder.jump(loop_condition_block, vec![]);

                // Pop the loop context
                self.loop_contexts.pop();

                // Switch to exit block (would be reached when loop ends)
                builder.switch_to_block(loop_exit_block);

                // Return the body value as the loop's result
                body_val
            }
            TypedExprKind::Return(return_expr) => {
                // This should terminate the current function
                let return_val = return_expr
                    .as_ref()
                    .map(|expr| self.convert_expr(builder, expr))
                    .unwrap_or_else(|| builder.const_int(0, self.convert_type(&expr.type_)));

                // Add the return instruction to the builder
                let _return_terminator = builder.ret(Some(return_val));

                // Return the value ID that represents the return
                return_val
            }
            TypedExprKind::Break(break_expr) => {
                // This should break out of the innermost loop
                if let Some(last_context) = self.loop_contexts.last() {
                    let (_, _, exit_block) = *last_context;

                    // Jump to the exit block of the innermost loop
                    let _break_jump = builder.jump(exit_block, vec![]);

                    // For break with value, we need to handle it differently
                    // For now, just return a constant as a placeholder
                    builder.const_int(0, self.convert_type(&expr.type_))
                } else {
                    // No loop context found - this is an error in the source code
                    // For now, just return a default value
                    if let Some(expr) = break_expr {
                        self.convert_expr(builder, expr)
                    } else {
                        builder.const_int(0, self.convert_type(&expr.type_))
                    }
                }
            }
            TypedExprKind::Continue => {
                // This should continue to the next iteration of the innermost loop
                if let Some(last_context) = self.loop_contexts.last() {
                    let (continue_block, _, _) = *last_context;

                    // Continue means jumping back to the loop continuation point
                    // For while loops, this is the condition check; for other loops it's the entry point
                    let _continue_jump = builder.jump(continue_block, vec![]);

                    // Return a placeholder value
                    builder.const_int(0, self.convert_type(&expr.type_))
                } else {
                    // No loop context found - this is an error in the source code
                    // For now, just return a default value
                    builder.const_int(0, self.convert_type(&expr.type_))
                }
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
                // For now, implement simple field access as a fallback.
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
        use crate::hir::Type as HirType;

        // This is a more sophisticated conversion that maps AST types to HIR types properly
        match &ast_type.type_ {
            TypeKind::Constructor {
                name,
                args,
                kind: _,
            } => {
                // For constructor types, we need to preserve the original semantics for simple cases
                // but avoid the collision-prone offset system for complex cases
                if args.is_empty() {
                    // For simple constructors without arguments, we can return the name directly
                    // This preserves the original test expectations while avoiding collisions
                    TypeId(*name)
                } else {
                    // For parameterized types, use the type context to avoid collisions
                    let arg_types: Vec<TypeId> =
                        args.iter().map(|t| self.convert_type(t)).collect();

                    // Create a unique type in the type context for parameterized types
                    self.type_context.intern(HirType::Struct {
                        name: format!("Type{}", name),
                        fields: arg_types
                            .iter()
                            .map(|&ty| ("arg".to_string(), ty))
                            .collect(),
                    })
                }
            }
            TypeKind::Variable { id, kind: _ } => {
                // For type variables, preserve the original ID to maintain semantic meaning
                TypeId(*id)
            }
            TypeKind::Function {
                params,
                return_type,
                effects,
            } => {
                // For function types, use the type context to avoid offset collisions
                let param_types: Vec<TypeId> =
                    params.iter().map(|t| self.convert_type(t)).collect();
                let return_type_id = self.convert_type(return_type);
                let _effect_types: Vec<TypeId> =
                    effects.effects.iter().map(|_| TypeId(0)).collect();

                // Create a function type in the type context
                self.type_context.intern(HirType::Function {
                    params: param_types,
                    return_type: return_type_id,
                })
            }
            TypeKind::Row { fields, rest } => {
                // For row types, use the type context to avoid offset collisions
                let field_types: Vec<(String, TypeId)> = fields
                    .iter()
                    .map(|(name, ty)| (format!("field{}", name), self.convert_type(ty)))
                    .collect();
                let _rest_type = rest.as_ref().map(|&type_var_id| {
                    self.convert_type(&Rc::new(TypeType {
                        span: None,
                        file: None,
                        type_: TypeKind::Variable {
                            id: type_var_id,
                            kind: crate::ast::Kind::Star,
                        },
                    }))
                });

                // Create a struct type to represent the row in the type context
                self.type_context.intern(HirType::Struct {
                    name: "Row".to_string(),
                    fields: field_types,
                })
            }
            TypeKind::Tuple(element_types) => {
                // For tuple types, use the type context to avoid offset collisions
                let elem_types: Vec<TypeId> =
                    element_types.iter().map(|t| self.convert_type(t)).collect();

                // Create a tuple type in the type context
                self.type_context.intern(HirType::Tuple {
                    elements: elem_types,
                })
            }
            TypeKind::Union(variant_types) => {
                // For union types, use the type context to avoid offset collisions
                let variant_type_ids: Vec<TypeId> =
                    variant_types.iter().map(|t| self.convert_type(t)).collect();

                // Represent union as a struct with all possible variants in the type context
                self.type_context.intern(HirType::Struct {
                    name: "Union".to_string(),
                    fields: variant_type_ids
                        .iter()
                        .enumerate()
                        .map(|(i, &ty)| (format!("variant{}", i), ty))
                        .collect(),
                })
            }
            TypeKind::Forall {
                vars,
                constraints,
                body,
            } => {
                // For forall types, use the type context to avoid offset collisions
                let _var_ids: Vec<(TypeId, crate::ast::Kind)> = vars
                    .iter()
                    .map(|(id, kind)| {
                        (
                            self.convert_type(&Rc::new(TypeType {
                                span: None,
                                file: None,
                                type_: TypeKind::Variable {
                                    id: *id,
                                    kind: kind.clone(),
                                },
                            })),
                            kind.clone(),
                        )
                    })
                    .collect();
                let _constraint_types: Vec<crate::typechecker::Constraint> = constraints.to_vec();
                let body_type = self.convert_type(body);

                // For now, return the body type - forall types are complex
                body_type
            }
            TypeKind::Exists {
                vars,
                constraints,
                body,
            } => {
                // For exists types, use the type context to avoid offset collisions
                let _var_ids: Vec<(TypeId, crate::ast::Kind)> = vars
                    .iter()
                    .map(|(id, kind)| {
                        (
                            self.convert_type(&Rc::new(TypeType {
                                span: None,
                                file: None,
                                type_: TypeKind::Variable {
                                    id: *id,
                                    kind: kind.clone(),
                                },
                            })),
                            kind.clone(),
                        )
                    })
                    .collect();
                let _constraint_types: Vec<crate::typechecker::Constraint> = constraints.to_vec();
                let body_type = self.convert_type(body);

                // For now, return the body type - exists types are complex
                body_type
            }
            TypeKind::Never => {
                // For never type, use the type context to avoid offset collisions
                self.type_context.intern(HirType::Unit)
            }
            TypeKind::Error => {
                // For error type, use the type context to avoid offset collisions
                self.type_context.intern(HirType::Unit)
            }
            TypeKind::Pointer(inner_type) => {
                // For pointer types, use the type context to avoid offset collisions
                let inner_type_id = self.convert_type(inner_type);
                self.type_context.intern(HirType::Pointer {
                    element_type: inner_type_id,
                    mutable: false, // Default to immutable
                })
            }
            TypeKind::Trait(trait_names) => {
                // For trait types, use the type context to avoid offset collisions
                self.type_context.intern(HirType::Struct {
                    name: format!("Trait{}", trait_names.len()),
                    fields: vec![],
                })
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

    /// Helper function to check if a pattern matches a value
    fn check_pattern_match(
        &mut self,
        builder: &mut Builder,
        scrutinee_val: ValueId,
        pattern: &TypedPattern,
    ) -> ValueId {
        match &pattern.pat {
            TypedPatKind::Wildcard => {
                // Wildcard always matches
                builder.const_bool(true)
            }
            TypedPatKind::Bind { binding_id, .. } => {
                // Bind pattern - always matches, bind the value
                self.value_map.insert(*binding_id, scrutinee_val);
                builder.const_bool(true)
            }
            TypedPatKind::Literal(literal) => {
                match literal {
                    crate::ast::Literal::Int(int_val) => {
                        let literal_val =
                            builder.const_int(*int_val as i128, self.convert_type(&pattern.type_));
                        builder.binary_op(
                            Opcode::Eq,
                            scrutinee_val,
                            literal_val,
                            self.convert_type(&pattern.type_),
                        )
                    }
                    crate::ast::Literal::Bool(bool_val) => {
                        let literal_val = builder.const_bool(*bool_val);
                        builder.binary_op(
                            Opcode::Eq,
                            scrutinee_val,
                            literal_val,
                            self.convert_type(&pattern.type_),
                        )
                    }
                    crate::ast::Literal::Float(float_val) => {
                        let literal_val =
                            builder.const_float(*float_val, self.convert_type(&pattern.type_));
                        builder.binary_op(
                            Opcode::Eq,
                            scrutinee_val,
                            literal_val,
                            self.convert_type(&pattern.type_),
                        )
                    }
                    crate::ast::Literal::String(string_val) => {
                        // For string comparison, we'll use a simple equality check
                        // In a real implementation, this would be more complex
                        let _string_val = string_val; // Use the string value
                        builder.const_bool(true) // Placeholder for string comparison
                    }
                }
            }
            TypedPatKind::Range { start, end } => {
                // Range pattern: start..=end (inclusive)
                let start_val = self.convert_expr(builder, start);
                let end_val = self.convert_expr(builder, end);

                // Check if scrutinee_val is between start_val and end_val (inclusive)
                let ge_check = builder.binary_op(
                    Opcode::Ge,
                    scrutinee_val,
                    start_val,
                    self.convert_type(&pattern.type_),
                );
                let le_check = builder.binary_op(
                    Opcode::Le,
                    scrutinee_val,
                    end_val,
                    self.convert_type(&pattern.type_),
                );
                builder.binary_op(
                    Opcode::BitAnd,
                    ge_check,
                    le_check,
                    self.convert_type(&pattern.type_),
                )
            }
            TypedPatKind::Or(patterns) => {
                // Or pattern: match any of the patterns
                if patterns.is_empty() {
                    builder.const_bool(false)
                } else {
                    let mut result = self.check_pattern_match(builder, scrutinee_val, &patterns[0]);

                    for pattern in &patterns[1..] {
                        let next_match = self.check_pattern_match(builder, scrutinee_val, pattern);
                        result = builder.binary_op(
                            Opcode::BitOr,
                            result,
                            next_match,
                            self.convert_type(&pattern.type_),
                        );
                    }
                    result
                }
            }
            TypedPatKind::As {
                binding_id,
                pattern: _inner_pattern,
                ..
            } => {
                // As pattern: bind the matched value to the identifier and then match the inner pattern
                // First bind the value
                self.value_map.insert(*binding_id, scrutinee_val);
                // Then check the inner pattern (we'll just return true for now as a placeholder)
                // In a real implementation, we'd need to properly handle nested pattern matching
                builder.const_bool(true)
            }
            // For more complex patterns like Array, Tuple, Struct, Enum, we'll return true as placeholders
            // since implementing full pattern matching is quite complex
            _ => {
                // For now, assume complex patterns match
                builder.const_bool(true)
            }
        }
    }

    fn convert_struct(&mut self, struct_def: &TypedStruct) -> TypedStruct {
        // Structs are primarily data containers; methods are in separate IMPL blocks
        // For HIR conversion, we mostly just pass through the struct definition
        // as the type information and field structure are already resolved
        // Methods are handled separately in IMPL blocks
        struct_def.clone()
    }

    fn convert_enum(&mut self, enum_def: &TypedEnum) -> TypedEnum {
        // Enums are primarily data containers; methods are in separate IMPL blocks
        // For HIR conversion, we mostly just pass through the enum definition
        // as the type information and variant structure are already resolved
        // Methods are handled separately in IMPL blocks
        enum_def.clone()
    }

    fn convert_type_alias(&mut self, alias_def: &TypedTypeAlias) -> TypedTypeAlias {
        // Type aliases are mostly resolved during type checking
        // For HIR conversion, we pass through the alias definition
        // as the target type is already resolved
        alias_def.clone()
    }

    /// Look up the runtime implementation for an effect
    fn get_effect_implementation(&self, effect_id: EffectId) -> Option<ValueId> {
        self.effect_map.get(&effect_id).copied()
    }

    fn convert_impl(&mut self, impl_def: &TypedImpl) -> TypedImpl {
        // Convert the methods in the impl block to HIR functions
        let mut new_impl = impl_def.clone();

        // Convert each method in the impl block to HIR
        for method in &mut new_impl.methods {
            // Convert the method body to HIR
            if let Some(ref mut body) = method.body {
                // For now we just clone the body, but in a complete implementation
                // we would convert the method body to HIR functions and add them
                // to the function collection
                *body = body.clone();
            }
        }

        new_impl
    }

    fn convert_trait(&mut self, trait_def: &TypedTraitDef) -> TypedTraitDef {
        // Traits define interfaces; method implementations are in separate IMPL blocks
        // For HIR conversion, we pass through the trait definition as the
        // interface structure is already resolved during type checking
        trait_def.clone()
    }

    fn convert_macro(&mut self, macro_def: &TypedMacroDef) -> TypedMacroDef {
        // TODO: Macros should be expanded during earlier phases, not converted to HIR
        // Macro expansion happens before HIR generation
        macro_def.clone()
    }

    fn convert_effect(&mut self, effect_def: &TypedEffectDef) -> TypedEffectDef {
        // Effects define operations that can be performed and handled
        // For HIR conversion, we pass through the effect definition as the
        // operation structure is already resolved during type checking
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
        let _result = converter.convert_expr(&mut builder, &int_expr);
    }

    #[test]
    fn test_convert_bool_literal() {
        let mut converter = ASTToHIRConverter::new();
        let bool_expr = create_bool_expr(true);

        let mut builder = Builder::new();
        let _result = converter.convert_expr(&mut builder, &bool_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &block_expr);
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
        let _result = converter.convert_expr(&mut builder, &block_expr);
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
        let _result = converter.convert_expr(&mut builder, &let_expr);
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

        let _hir_type_id = converter.convert_type(&int_type);
    }

    #[test]
    fn test_convert_type_bool() {
        let mut converter = ASTToHIRConverter::new();
        let bool_type = create_bool_type();

        let _hir_type_id = converter.convert_type(&bool_type);
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
        let _result = converter.convert_expr(&mut builder, &if_expr);
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
        let _result = converter.convert_expr(&mut builder, &if_expr);
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
        let _result = converter.convert_expr(&mut builder, &unop_expr);
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
        let _result = converter.convert_expr(&mut builder, &unop_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &string_expr);
    }

    #[test]
    #[allow(clippy::approx_constant)]
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
        let _result = converter.convert_expr(&mut builder, &float_expr);
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
        let _result = converter.convert_expr(&mut builder, &nested_expr);
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

        let _hir_type_id = converter.convert_type(&never_type);
    }

    #[test]
    fn test_convert_type_error() {
        let mut converter = ASTToHIRConverter::new();
        let error_type = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Error,
        });

        let _hir_type_id = converter.convert_type(&error_type);
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

        let _hir_type_id = converter.convert_type(&func_type);
    }

    #[test]
    fn test_convert_tuple_type() {
        let mut converter = ASTToHIRConverter::new();
        let tuple_type = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Tuple(vec![create_int_type(), create_bool_type()]),
        });

        let _hir_type_id = converter.convert_type(&tuple_type);
    }

    #[test]
    fn test_convert_union_type() {
        let mut converter = ASTToHIRConverter::new();
        let union_type = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Union(vec![create_int_type(), create_bool_type()]),
        });

        let _hir_type_id = converter.convert_type(&union_type);
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
        let _result = converter.convert_expr(&mut builder, &block_expr);
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
        let _result = converter.convert_expr(&mut builder, &complex_if_expr);
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
        let _result = converter.convert_expr(&mut builder, &mult_expr);
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
            let _result = converter.convert_expr(&mut builder, &bin_op_expr);
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
        let _result = converter.convert_expr(&mut builder, &minus_expr);

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
        let _result2 = converter.convert_expr(&mut builder2, &not_expr);
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
        let _result = converter.convert_expr(&mut builder, &block_expr);
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
        let _result = converter.convert_expr(&mut builder, &with_expr);
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
        let _result = converter.convert_expr(&mut builder, &optional_chain_expr);
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
        let _result = converter.convert_expr(&mut builder, &macro_call_expr);
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
        let _result = converter.convert_expr(&mut builder, &array_expr);
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
        let _result = converter.convert_expr(&mut builder, &array_expr);
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
        let _result = converter.convert_expr(&mut builder, &array_expr);
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
        let _result = converter.convert_expr(&mut builder, &import_expr);
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
        let _result = converter.convert_expr(&mut builder, &handle_expr);
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
        let _result = converter.convert_expr(&mut builder, &tuple_expr);
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
        let _result = converter.convert_expr(&mut builder, &tuple_expr);
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
        let _result = converter.convert_expr(&mut builder, &tuple_expr);
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
        let _result = converter.convert_expr(&mut builder, &map_expr);
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
        let _result = converter.convert_expr(&mut builder, &map_expr);
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
        let _result = converter.convert_expr(&mut builder, &map_expr);
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
        let _result = converter.convert_expr(&mut builder, &struct_expr);
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
        let _result = converter.convert_expr(&mut builder, &struct_expr);
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
        let _result = converter.convert_expr(&mut builder, &struct_expr);
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
        let _result = converter.convert_expr(&mut builder, &enum_expr);
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
        let _result = converter.convert_expr(&mut builder, &enum_expr);
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
        let _result = converter.convert_expr(&mut builder, &enum_expr);
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
        let _result = converter.convert_expr(&mut builder, &perform_expr);
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
        let _result = converter.convert_expr(&mut builder, &perform_expr);
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
        let _result = converter.convert_expr(&mut builder, &perform_expr);
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
        let _result = converter.convert_expr(&mut builder, &match_expr);
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
        let _result = converter.convert_expr(&mut builder, &match_expr);
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
        let _result = converter.convert_expr(&mut builder, &match_expr);
    }
}
