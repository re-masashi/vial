use std::collections::HashMap;

use super::value::{HeapPtr, Value};
use crate::ir::{EnumId, FieldId, FunctionId, StructId, VariantId};

#[derive(Debug)]
pub enum HeapObject {
    Struct {
        struct_id: StructId,
        fields: HashMap<FieldId, Value>,
    },
    Enum {
        enum_id: EnumId,
        variant_id: VariantId,
        data: Vec<Value>,
    },
    Array {
        elements: Vec<Value>,
    },
    Tuple {
        elements: Vec<Value>,
    },
    Closure {
        function_id: FunctionId,
        captures: Vec<Value>,
    },
}

#[derive(Debug, Default)]
pub struct Heap {
    objects: Vec<Option<HeapObject>>,
}

impl Heap {
    /// Creates a new, empty heap.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::heap::*;
    /// use vial::interpreter::value::Value;
    ///
    /// let mut heap = Heap::new();
    /// let ptr = heap.allocate(HeapObject::Array { elements: vec![] });
    /// // first allocation is stored at index 0
    /// assert_eq!(ptr.id, 0);
    /// ```
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
        }
    }

    /// Allocates a heap object and returns a pointer to it.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::heap::*;
    /// use vial::interpreter::value::Value;
    ///
    /// let mut heap = Heap::new();
    /// let ptr = heap.allocate(HeapObject::Array { elements: vec![] });
    /// assert_eq!(ptr.id, 0);
    /// ```
    ///
    /// # Returns
    ///
    /// A `HeapPtr` that references the newly stored object.
    pub fn allocate(&mut self, object: HeapObject) -> HeapPtr {
        let id = self.objects.len();
        self.objects.push(Some(object));
        HeapPtr { id }
    }

    /// Retrieves an immutable reference to the heap object identified by `ptr`.
    ///
    /// Returns `&HeapObject` when `ptr` refers to an allocated object, or
    /// `InterpreterError::InvalidHeapAccess` if the pointer is out of bounds or points to a freed slot.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::heap::*;
    /// use vial::interpreter::value::Value;
    ///
    /// let mut heap = Heap::new();
    /// let ptr = heap.allocate(HeapObject::Array { elements: vec![] });
    /// let obj = heap.get(ptr).unwrap();
    /// match obj {
    ///     HeapObject::Array { elements } => assert!(elements.is_empty()),
    ///     _ => panic!("expected array"),
    /// }
    /// ```
    pub fn get(&self, ptr: HeapPtr) -> Result<&HeapObject, super::error::InterpreterError> {
        self.objects
            .get(ptr.id)
            .and_then(|obj| obj.as_ref())
            .ok_or(super::error::InterpreterError::InvalidHeapAccess)
    }

    /// Returns a mutable reference to the heap object pointed to by `ptr`.
    ///
    /// # Returns
    ///
    /// `Ok(&mut HeapObject)` with a mutable reference to the object when `ptr` refers to a valid, live object; `Err(InterpreterError::InvalidHeapAccess)` if `ptr` is out of bounds or the object has been deallocated.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::heap::*;
    /// use vial::interpreter::value::Value;
    ///
    /// let mut heap = Heap::new();
    /// let ptr = heap.allocate(HeapObject::Array { elements: vec![Value::Int(1)] });
    ///
    /// {
    ///     let obj = heap.get_mut(ptr).unwrap();
    ///     if let HeapObject::Array { elements } = obj {
    ///         elements.push(Value::Int(2));
    ///     }
    /// }
    ///
    /// let obj = heap.get(ptr).unwrap();
    /// if let HeapObject::Array { elements } = obj {
    ///     assert_eq!(elements.len(), 2);
    /// }
    /// ```
    pub fn get_mut(
        &mut self,
        ptr: HeapPtr,
    ) -> Result<&mut HeapObject, super::error::InterpreterError> {
        self.objects
            .get_mut(ptr.id)
            .and_then(|obj| obj.as_mut())
            .ok_or(super::error::InterpreterError::InvalidHeapAccess)
    }

    /// Removes and returns the heap object at the specified pointer.
    ///
    /// Returns the removed `HeapObject` on success. Returns `InterpreterError::InvalidHeapAccess` if the pointer's index is out of bounds or the entry has already been deallocated.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::heap::*;
    /// use vial::interpreter::value::Value;
    ///
    /// let mut heap = Heap::new();
    /// let ptr = heap.allocate(HeapObject::Array { elements: vec![] });
    /// let obj = heap.deallocate(ptr).unwrap();
    /// match obj {
    ///     HeapObject::Array { elements } => assert!(elements.is_empty()),
    ///     _ => panic!("expected array"),
    /// }
    /// ```
    pub fn deallocate(
        &mut self,
        ptr: HeapPtr,
    ) -> Result<HeapObject, super::error::InterpreterError> {
        self.objects
            .get_mut(ptr.id)
            .and_then(|obj| obj.take())
            .ok_or(super::error::InterpreterError::InvalidHeapAccess)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::FieldId;

    #[test]
    fn test_heap_creation() {
        let heap = Heap::new();
        assert_eq!(heap.objects.len(), 0);
    }

    #[test]
    fn test_heap_allocation() {
        let mut heap = Heap::new();

        // Test array allocation
        let array_obj = HeapObject::Array {
            elements: vec![Value::Int(1), Value::Int(2)],
        };
        let ptr = heap.allocate(array_obj);

        assert_eq!(ptr.id, 0);
        assert_eq!(heap.objects.len(), 1);

        // Test retrieval
        let retrieved = heap.get(ptr).unwrap();
        match retrieved {
            HeapObject::Array { elements } => {
                assert_eq!(elements.len(), 2);
                assert_eq!(elements[0], Value::Int(1));
            }
            _ => panic!("Expected array object"),
        }
    }

    #[test]
    fn test_heap_struct_allocation() {
        let mut heap = Heap::new();

        let mut fields = HashMap::new();
        fields.insert(FieldId(0), Value::Int(42));
        fields.insert(FieldId(1), Value::String("test".to_string()));

        let struct_obj = HeapObject::Struct {
            struct_id: StructId(1),
            fields,
        };

        let ptr = heap.allocate(struct_obj);
        let retrieved = heap.get(ptr).unwrap();

        match retrieved {
            HeapObject::Struct { struct_id, fields } => {
                assert_eq!(*struct_id, StructId(1));
                assert_eq!(fields.get(&FieldId(0)), Some(&Value::Int(42)));
                assert_eq!(
                    fields.get(&FieldId(1)),
                    Some(&Value::String("test".to_string()))
                );
            }
            _ => panic!("Expected struct object"),
        }
    }

    #[test]
    fn test_heap_mut_operations() {
        let mut heap = Heap::new();

        let array_obj = HeapObject::Array {
            elements: vec![Value::Int(1)],
        };
        let ptr = heap.allocate(array_obj);

        // Modify the heap object
        {
            let obj = heap.get_mut(ptr).unwrap();
            match obj {
                HeapObject::Array { elements } => {
                    elements.push(Value::Int(2));
                }
                _ => panic!("Expected array object"),
            }
        }

        // Verify the change
        let retrieved = heap.get(ptr).unwrap();
        match retrieved {
            HeapObject::Array { elements } => {
                assert_eq!(elements.len(), 2);
                assert_eq!(elements[1], Value::Int(2));
            }
            _ => panic!("Expected array object"),
        }
    }

    #[test]
    fn test_heap_error_handling() {
        let heap = Heap::new();
        let invalid_ptr = HeapPtr { id: 999 };

        let result = heap.get(invalid_ptr);
        assert!(result.is_err());

        match result.unwrap_err() {
            crate::interpreter::error::InterpreterError::InvalidHeapAccess => (), // Expected
            _ => panic!("Expected InvalidHeapAccess error"),
        }
    }
}
