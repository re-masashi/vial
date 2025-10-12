//! Type system for the HIR

use std::collections::HashMap;

/// A unique identifier for a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

/// Represents a type in the HIR
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Primitive integer types
    Int(IntType),
    /// Primitive float types
    Float(FloatType),
    /// Boolean type
    Bool,
    /// Character type
    Char,
    /// String type
    String,
    /// Pointer to another type
    Pointer { element_type: TypeId, mutable: bool },
    /// Array with fixed size
    Array { element_type: TypeId, size: usize },
    /// Slice (dynamic array)
    Slice { element_type: TypeId },
    /// Tuple type
    Tuple { elements: Vec<TypeId> },
    /// Struct type
    Struct {
        name: String,
        fields: Vec<(String, TypeId)>,
    },
    /// Function type
    Function {
        params: Vec<TypeId>,
        return_type: TypeId,
    },
    /// Reference type (similar to pointer but with borrowing rules)
    Reference { element_type: TypeId, mutable: bool },
    /// Void type (for functions that don't return anything)
    Void,
    /// Unit type (empty tuple)
    Unit,
}

/// Integer type variants
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
}

/// Float type variants
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatType {
    F32,
    F64,
}

/// A type context that maps TypeIds to Type definitions
#[derive(Debug)]
pub struct TypeContext {
    types: Vec<Type>,
    type_map: HashMap<Type, TypeId>,
}

impl TypeContext {
    pub fn new() -> Self {
        TypeContext {
            types: Vec::new(),
            type_map: HashMap::new(),
        }
    }

    /// Intern a type and return its ID
    pub fn intern(&mut self, ty: Type) -> TypeId {
        if let Some(&id) = self.type_map.get(&ty) {
            return id;
        }

        let id = TypeId(self.types.len());
        self.types.push(ty.clone());
        self.type_map.insert(ty, id);
        id
    }

    /// Get a type by its ID
    pub fn get(&self, id: TypeId) -> Option<&Type> {
        self.types.get(id.0)
    }

    /// Get the size of a type in bytes
    pub fn size_of(&self, id: TypeId) -> Option<usize> {
        match self.get(id) {
            Some(Type::Int(int_type)) => Some(match int_type {
                IntType::I8 | IntType::U8 => 1,
                IntType::I16 | IntType::U16 => 2,
                IntType::I32 | IntType::U32 => 4,
                IntType::I64 | IntType::U64 => 8,
                IntType::I128 | IntType::U128 => 16,
                IntType::ISize | IntType::USize => std::mem::size_of::<usize>(),
            }),
            Some(Type::Float(float_type)) => Some(match float_type {
                FloatType::F32 => 4,
                FloatType::F64 => 8,
            }),
            Some(Type::Bool) => Some(1),
            Some(Type::Char) => Some(std::mem::size_of::<char>()),
            Some(Type::String) => Some(std::mem::size_of::<String>()),
            Some(Type::Pointer { .. }) | Some(Type::Reference { .. }) => {
                Some(std::mem::size_of::<*const ()>())
            }
            Some(Type::Array { element_type, size }) => self
                .size_of(*element_type)
                .map(|elem_size| elem_size * size),
            Some(Type::Slice { element_type: _ }) => Some(std::mem::size_of::<&[()]>()),
            Some(Type::Tuple { elements }) => {
                let total: usize = elements
                    .iter()
                    .map(|&ty| self.size_of(ty).unwrap_or(0))
                    .sum();
                Some(total)
            }
            Some(Type::Struct { fields, .. }) => {
                let total: usize = fields
                    .iter()
                    .map(|(_, ty)| self.size_of(*ty).unwrap_or(0))
                    .sum();
                Some(total)
            }
            Some(Type::Function { .. }) => Some(std::mem::size_of::<*const ()>()),
            Some(Type::Void) | Some(Type::Unit) => Some(0),
            None => None,
        }
    }

    /// Check if two types are equal by ID
    pub fn types_equal(&self, id1: TypeId, id2: TypeId) -> bool {
        id1 == id2 || self.get(id1) == self.get(id2)
    }

    /// Check if a type is a pointer type
    pub fn is_pointer(&self, id: TypeId) -> bool {
        matches!(self.get(id), Some(Type::Pointer { .. }))
    }

    /// Check if a type is a reference type
    pub fn is_reference(&self, id: TypeId) -> bool {
        matches!(self.get(id), Some(Type::Reference { .. }))
    }

    /// Get the element type of a pointer or reference
    pub fn element_type(&self, id: TypeId) -> Option<TypeId> {
        match self.get(id) {
            Some(Type::Pointer { element_type, .. })
            | Some(Type::Reference { element_type, .. })
            | Some(Type::Slice { element_type }) => Some(*element_type),
            Some(Type::Array { element_type, .. }) => Some(*element_type),
            _ => None,
        }
    }
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::new()
    }
}
