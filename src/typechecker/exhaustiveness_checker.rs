use crate::typechecker::*;

// Pattern Matrix for Exhaustiveness Checking
#[derive(Debug, Clone)]
enum Constructor {
    Int(i64),
    Bool(bool),
    String(String),
    // Tuple(usize),           // arity
    Array(usize), // length
    Struct(StructId),
    Enum(EnumId, VariantId), // enum_id, variant_id
    Wildcard,
}

#[derive(Debug, Clone)]
struct PatternMatrix {
    rows: Vec<Vec<TypedPattern>>,
}

#[derive(Debug)]
pub struct ExhaustivenessChecker {
    interner: Interner,
    env: TypeEnv,
}

impl ExhaustivenessChecker {
    pub fn new(interner: Interner, env: TypeEnv) -> Self {
        Self { interner, env }
    }

    // Main entry point: check if match is exhaustive
    pub fn check_match(
        &self,
        scrutinee_type: &Rc<Type>,
        arms: &[TypedMatchArm],
        span: &Range<usize>,
        diagnostics: &mut Diagnostics,
    ) {
        let patterns: Vec<_> = arms.iter().map(|arm| arm.pattern.clone()).collect();

        // Check for redundant patterns
        for (i, pattern) in patterns.iter().enumerate() {
            if !self.is_useful(&patterns[..i], pattern) {
                // diagnostics.add_validation_error(ValidationError {
                //     span: pattern.span.clone(),
                //     file: pattern.file.clone(),
                //     kind: ValidationErrorKind::UnreachableCode,
                // });
            }
        }

        // Check for exhaustiveness
        let missing = self.compute_missing_patterns(scrutinee_type, &patterns);
        if !missing.is_empty() {
            diagnostics.add_type_error(TypeError {
                span: span.clone(),
                file: String::new(),
                kind: TypeErrorKind::NonExhaustiveMatch {
                    missing_patterns: missing,
                },
            });
        }
    }

    // Check if pattern is useful given previous patterns
    fn is_useful(&self, previous: &[TypedPattern], pattern: &TypedPattern) -> bool {
        let matrix = PatternMatrix {
            rows: previous.iter().map(|p| vec![p.clone()]).collect(),
        };

        #[allow(clippy::cloned_ref_to_slice_refs)]
        self.is_useful_inner(&matrix, &[pattern.clone()])
    }

    fn is_useful_inner(&self, matrix: &PatternMatrix, row: &[TypedPattern]) -> bool {
        // Base case: no patterns in row
        if row.is_empty() {
            return matrix.rows.is_empty();
        }

        let first_pat = &row[0];
        let rest = &row[1..];

        match &first_pat.pat {
            TypedPatKind::Wildcard | TypedPatKind::Bind { .. } => {
                // Wildcard/bind matches anything
                // Check all constructors
                let constructors = self.list_constructors(&first_pat.type_);

                if constructors.is_empty() {
                    // No constructors - just wildcard
                    let new_matrix = self.specialize_wildcard(matrix);
                    return self.is_useful_inner(&new_matrix, rest);
                }

                // Check if useful for any constructor
                for constructor in constructors {
                    let specialized = self.specialize_matrix(matrix, &constructor);
                    let specialized_row = self.specialize_row(row, &constructor);

                    if self.is_useful_inner(&specialized, &specialized_row) {
                        return true;
                    }
                }

                false
            }

            TypedPatKind::Literal(lit) => {
                let constructor = match lit {
                    Literal::Int(n) => Constructor::Int(*n),
                    Literal::Bool(b) => Constructor::Bool(*b),
                    Literal::String(s) => Constructor::String(s.clone()),
                    _ => Constructor::Wildcard,
                };

                let specialized = self.specialize_matrix(matrix, &constructor);
                let specialized_row = self.specialize_row(row, &constructor);
                self.is_useful_inner(&specialized, &specialized_row)
            }

            // TypedPatKind::Tuple(patterns) => {
            //     let constructor = Constructor::Tuple(patterns.len());
            //     let specialized = self.specialize_matrix(matrix, &constructor);
            //     let specialized_row = self.specialize_row(row, &constructor);
            //     self.is_useful_inner(&specialized, &specialized_row)
            // }
            TypedPatKind::Array { patterns, .. } => {
                let constructor = Constructor::Array(patterns.len());
                let specialized = self.specialize_matrix(matrix, &constructor);
                let specialized_row = self.specialize_row(row, &constructor);
                self.is_useful_inner(&specialized, &specialized_row)
            }

            TypedPatKind::Struct { struct_id, .. } => {
                // let field_names: Vec<_> = fields.iter().map(|(name, _, _)| *name).collect();
                let constructor = Constructor::Struct(*struct_id);
                let specialized = self.specialize_matrix(matrix, &constructor);
                let specialized_row = self.specialize_row(row, &constructor);
                self.is_useful_inner(&specialized, &specialized_row)
            }

            TypedPatKind::Enum {
                enum_id,
                variant_id,
                ..
            } => {
                let constructor = Constructor::Enum(*enum_id, *variant_id);
                let specialized = self.specialize_matrix(matrix, &constructor);
                let specialized_row = self.specialize_row(row, &constructor);
                self.is_useful_inner(&specialized, &specialized_row)
            }

            TypedPatKind::Or(patterns) => {
                // A pattern is useful if any of its sub-patterns is useful
                patterns.iter().any(|p| {
                    #[allow(clippy::cloned_ref_to_slice_refs)]
                    let new_row = [&[p.clone()], rest].concat();
                    self.is_useful_inner(matrix, &new_row)
                })
            }

            _ => true, // Conservative: assume useful
        }
    }

    // List all constructors for a type
    fn list_constructors(&self, ty: &Rc<Type>) -> Vec<Constructor> {
        match &ty.type_ {
            TypeKind::Constructor { name, .. } => {
                let type_name = self.interner.resolve(Symbol(*name));

                match type_name {
                    "bool" => vec![Constructor::Bool(true), Constructor::Bool(false)],
                    "int" | "float" | "string" => {
                        // Infinite constructors, can't enumerate
                        vec![]
                    }
                    _ => {
                        if let Some((enum_id, enum_info)) =
                            self.env.enums.iter().find(|(_, info)| info.name.0 == *name)
                        {
                            if enum_info.variants.is_empty() {
                                return vec![];
                            }

                            return enum_info
                                .variants
                                .iter()
                                .map(|(_, (variant_id, _))| {
                                    Constructor::Enum(*enum_id, *variant_id)
                                })
                                .collect();
                        }

                        // Check if it's a struct
                        if let Some((struct_id, _)) = self
                            .env
                            .structs
                            .iter()
                            .find(|(_, info)| info.name.0 == *name)
                        {
                            return vec![Constructor::Struct(*struct_id)];
                        }

                        // Unknown type, return empty (can't check exhaustiveness)
                        vec![]
                    }
                }
            }
            _ => vec![],
        }
    }

    // Specialize matrix for a constructor
    fn specialize_matrix(
        &self,
        matrix: &PatternMatrix,
        constructor: &Constructor,
    ) -> PatternMatrix {
        let mut new_rows = Vec::new();

        for row in &matrix.rows {
            if row.is_empty() {
                continue;
            }

            let first = &row[0];
            let rest = &row[1..];

            match &first.pat {
                TypedPatKind::Wildcard | TypedPatKind::Bind { .. } => {
                    // Add wildcards for constructor args
                    let mut new_row = self.constructor_arity_wildcards(constructor, &first.type_);
                    new_row.extend_from_slice(rest);
                    new_rows.push(new_row);
                }

                TypedPatKind::Literal(lit) => {
                    if self.constructor_matches(constructor, lit) {
                        new_rows.push(rest.to_vec());
                    }
                }

                TypedPatKind::Enum {
                    variant_id, params, ..
                } => {
                    if let Constructor::Enum(_, c_variant) = constructor
                        && variant_id == c_variant
                    {
                        let mut new_row = params.clone();
                        new_row.extend_from_slice(rest);
                        new_rows.push(new_row);
                    }
                }

                // TypedPatKind::Tuple(patterns) => {
                //     if let Constructor::Tuple(_) = constructor {
                //         let mut new_row = patterns.clone();
                //         new_row.extend_from_slice(rest);
                //         new_rows.push(new_row);
                //     }
                // }
                TypedPatKind::Struct { fields, .. } => {
                    if let Constructor::Struct(_) = constructor {
                        let mut new_row: Vec<_> =
                            fields.iter().map(|(_, _, p)| p.clone()).collect();
                        new_row.extend_from_slice(rest);
                        new_rows.push(new_row);
                    }
                }

                _ => {}
            }
        }

        PatternMatrix { rows: new_rows }
    }

    fn specialize_row(&self, row: &[TypedPattern], constructor: &Constructor) -> Vec<TypedPattern> {
        if row.is_empty() {
            return vec![];
        }

        let first = &row[0];
        let rest = &row[1..];

        match &first.pat {
            TypedPatKind::Wildcard | TypedPatKind::Bind { .. } => {
                let mut result = self.constructor_arity_wildcards(constructor, &first.type_);
                result.extend_from_slice(rest);
                result
            }

            TypedPatKind::Enum { params, .. } => {
                let mut result = params.clone();
                result.extend_from_slice(rest);
                result
            }

            // TypedPatKind::Tuple(patterns) => {
            //     let mut result = patterns.clone();
            //     result.extend_from_slice(rest);
            //     result
            // }
            TypedPatKind::Struct { fields, .. } => {
                let mut result: Vec<_> = fields.iter().map(|(_, _, p)| p.clone()).collect();
                result.extend_from_slice(rest);
                result
            }

            _ => rest.to_vec(),
        }
    }

    fn specialize_wildcard(&self, matrix: &PatternMatrix) -> PatternMatrix {
        let new_rows: Vec<_> = matrix
            .rows
            .iter()
            .filter_map(|row| {
                if row.is_empty() {
                    return None;
                }

                match &row[0].pat {
                    TypedPatKind::Wildcard | TypedPatKind::Bind { .. } => Some(row[1..].to_vec()),
                    _ => None,
                }
            })
            .collect();

        PatternMatrix { rows: new_rows }
    }

    fn constructor_arity_wildcards(
        &self,
        constructor: &Constructor,
        ty: &Rc<Type>,
    ) -> Vec<TypedPattern> {
        match constructor {
            // Constructor::Tuple(arity) => {
            //     (0..*arity).map(|_| self.wildcard_pattern(ty)).collect()
            // }
            Constructor::Array(len) => (0..*len).map(|_| self.wildcard_pattern(ty)).collect(),
            Constructor::Enum(enum_id, variant_id) => {
                if let Some(enum_info) = self.env.enums.get(enum_id) {
                    for (vid, types) in enum_info.variants.values() {
                        if vid == variant_id {
                            return types.iter().map(|t| self.wildcard_pattern(t)).collect();
                        }
                    }
                }
                vec![]
            }
            Constructor::Struct(struct_id) => {
                if let Some(struct_info) = self.env.structs.get(struct_id) {
                    struct_info
                        .fields
                        .values()
                        .map(|(_, ty, _)| self.wildcard_pattern(ty))
                        .collect()
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    }

    fn wildcard_pattern(&self, ty: &Rc<Type>) -> TypedPattern {
        TypedPattern {
            span: 0..0,
            file: String::new(),
            pat: TypedPatKind::Wildcard,
            type_: ty.clone(),
        }
    }

    fn constructor_matches(&self, constructor: &Constructor, lit: &Literal) -> bool {
        match (constructor, lit) {
            (Constructor::Int(n1), Literal::Int(n2)) => n1 == n2,
            (Constructor::Bool(b1), Literal::Bool(b2)) => b1 == b2,
            (Constructor::String(s1), Literal::String(s2)) => s1 == s2,
            _ => false,
        }
    }

    // Compute missing patterns for exhaustiveness
    fn compute_missing_patterns(
        &self,
        scrutinee_type: &Rc<Type>,
        patterns: &[TypedPattern],
    ) -> Vec<String> {
        let matrix = PatternMatrix {
            rows: patterns.iter().map(|p| vec![p.clone()]).collect(),
        };

        let wildcard = self.wildcard_pattern(scrutinee_type);

        if self.is_useful_inner(&matrix, std::slice::from_ref(&wildcard)) {
            // Wildcard is useful, so patterns are not exhaustive
            self.construct_witnesses(scrutinee_type, &matrix)
        } else {
            vec![]
        }
    }

    fn construct_witnesses(&self, ty: &Rc<Type>, matrix: &PatternMatrix) -> Vec<String> {
        let constructors = self.list_constructors(ty);

        if constructors.is_empty() {
            return vec!["_".to_string()];
        }

        let mut missing = Vec::new();

        for constructor in constructors {
            let specialized = self.specialize_matrix(matrix, &constructor);

            if specialized.rows.is_empty() {
                // This constructor is missing
                missing.push(self.format_constructor(&constructor));
            }
        }

        if missing.is_empty() && !matrix.rows.is_empty() {
            // Check for partial coverage
            missing.push("_".to_string());
        }

        missing
    }

    fn format_constructor(&self, constructor: &Constructor) -> String {
        match constructor {
            Constructor::Int(n) => n.to_string(),
            Constructor::Bool(b) => b.to_string(),
            Constructor::String(s) => format!("\"{}\"", s),
            // Constructor::Tuple(arity) => {
            //     format!("({})", vec!["_"; *arity].join(", "))
            // }
            Constructor::Enum(_, variant_id) => {
                format!("Variant{}", variant_id.0)
            }
            Constructor::Struct(struct_id) => {
                format!("Struct{} {{ .. }}", struct_id.0)
            }
            Constructor::Array(len) => {
                format!("[{}]", vec!["_"; *len].join(", "))
            }
            Constructor::Wildcard => "_".to_string(),
        }
    }
}
