use super::errors::*;
use crate::ast::*;
use crate::lexer::Token;
use crate::parser;
use chumsky::prelude::*;
use logos::Logos;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub enum ImportSource {
    Std(Vec<String>),
    Local(PathBuf),
    Relative(PathBuf),
    External(String, Vec<String>),
}

// Internal representation for import items
#[derive(Debug, Clone)]
enum ImportItemKind {
    All,
    Name(String),
    Alias { name: String, alias: String },
}

#[derive(Debug)]
pub struct ModuleResolver {
    src_root: PathBuf,
    packages_dir: PathBuf,
    loaded_modules: HashMap<PathBuf, Vec<ASTNode>>,
    loading_stack: Vec<PathBuf>,
    pub diagnostics: ValidationDiagnostics,
}

impl ModuleResolver {
    pub fn new(project_root: &Path) -> Self {
        Self {
            src_root: project_root.join("src"),
            packages_dir: project_root.join("packages"),
            loaded_modules: HashMap::new(),
            loading_stack: Vec::new(),
            diagnostics: ValidationDiagnostics::new(),
        }
    }

    pub fn resolve_imports(&mut self, nodes: Vec<ASTNode>, current_file: &Path) -> Vec<ASTNode> {
        let mut result = Vec::new();

        for node in nodes {
            match &node.node {
                ASTNodeKind::Expr(expr) => {
                    if let ExprKind::Import(import) = &expr.expr {
                        match self.resolve_single_import(import, current_file) {
                            Ok(imported_nodes) => {
                                result.extend(imported_nodes);
                            }
                            Err(e) => {
                                self.diagnostics.add_error(ValidationError {
                                    span: import.span.clone(),
                                    file: import.file.clone(),
                                    kind: e,
                                });
                            }
                        }
                    } else {
                        result.push(node);
                    }
                }
                _ => result.push(node),
            }
        }

        result
    }

    fn resolve_single_import(
        &mut self,
        import: &Import,
        current_file: &Path,
    ) -> Result<Vec<ASTNode>, ValidationErrorKind> {
        // Join path parts: ["std", "io"] -> "std/io"
        let path_str = import.path.join("/");
        let source = self.parse_import_path(&path_str);

        let module_path = self.resolve_import_source(source, current_file)?;

        if self.loading_stack.contains(&module_path) {
            let mut cycle = self.loading_stack.clone();
            cycle.push(module_path.clone());
            return Err(ValidationErrorKind::CircularImport {
                cycle: cycle.iter().map(|p| p.display().to_string()).collect(),
            });
        }

        let module_nodes = self.load_module(&module_path)?;

        // Convert your AST's import format to internal format
        let items = self.extract_import_items(import);
        let filtered = self.filter_imported_items(&module_nodes, &items, &import.alias);

        Ok(filtered)
    }

    // Adapt to whatever format YOUR Import struct has
    fn extract_import_items(&self, import: &Import) -> Vec<ImportItemKind> {
        // If items is empty, import everything (wildcard)
        if import.items.is_empty() {
            return vec![ImportItemKind::All];
        }

        // Convert (String, Option<String>) to ImportItemKind
        import
            .items
            .iter()
            .map(|(name, alias)| match alias {
                None => ImportItemKind::Name(name.clone()),
                Some(alias_name) => ImportItemKind::Alias {
                    name: name.clone(),
                    alias: alias_name.clone(),
                },
            })
            .collect()
    }

    fn parse_import_path(&self, path: &str) -> ImportSource {
        if path.starts_with("std/") {
            let parts: Vec<_> = path
                .strip_prefix("std/")
                .unwrap()
                .split('/')
                .map(String::from)
                .collect();
            ImportSource::Std(parts)
        } else if path.starts_with("./") || path.starts_with("../") {
            ImportSource::Relative(PathBuf::from(path))
        } else if let Some(without_at) = path.strip_prefix('@') {
            let parts: Vec<_> = without_at.split('/').collect();

            if parts.is_empty() {
                return ImportSource::External(String::new(), vec![]);
            }

            let package = parts[0].to_string();
            let module_path = parts[1..].iter().map(|s| s.to_string()).collect();

            ImportSource::External(package, module_path)
        } else {
            ImportSource::Local(PathBuf::from(
                path.replace('/', std::path::MAIN_SEPARATOR_STR),
            ))
        }
    }

    fn resolve_import_source(
        &self,
        source: ImportSource,
        current_file: &Path,
    ) -> Result<PathBuf, ValidationErrorKind> {
        match source {
            ImportSource::Std(parts) => Ok(PathBuf::from(format!("__std__/{}", parts.join("/")))),

            ImportSource::Local(path) => {
                let full_path = self.src_root.join(&path);
                self.find_module_file(&full_path).ok_or_else(|| {
                    ValidationErrorKind::ImportNotFound {
                        path: path.display().to_string(),
                    }
                })
            }

            ImportSource::Relative(path) => {
                let current_dir =
                    current_file
                        .parent()
                        .ok_or_else(|| ValidationErrorKind::ImportNotFound {
                            path: path.display().to_string(),
                        })?;

                let full_path = current_dir.join(&path);
                self.find_module_file(&full_path).ok_or_else(|| {
                    ValidationErrorKind::ImportNotFound {
                        path: path.display().to_string(),
                    }
                })
            }

            ImportSource::External(package, module_path) => {
                if package.is_empty() {
                    return Err(ValidationErrorKind::ImportNotFound {
                        path: "@???".to_string(),
                    });
                }

                let pkg_dir = self.packages_dir.join(&package);

                if !pkg_dir.exists() {
                    return Err(ValidationErrorKind::ImportNotFound {
                        path: format!("@{}", package),
                    });
                }

                let module_file = if module_path.is_empty() {
                    pkg_dir.join("lib.vi")
                } else {
                    pkg_dir.join(module_path.join(std::path::MAIN_SEPARATOR_STR))
                };

                self.find_module_file(&module_file).ok_or_else(|| {
                    ValidationErrorKind::ImportNotFound {
                        path: format!("@{}/{}", package, module_path.join("/")),
                    }
                })
            }
        }
    }

    fn find_module_file(&self, path: &Path) -> Option<PathBuf> {
        let vi_file = path.with_extension("vi");
        if vi_file.exists() && vi_file.is_file() {
            return Some(vi_file);
        }

        if path.is_dir() {
            let mod_file = path.join("mod.vi");
            if mod_file.exists() && mod_file.is_file() {
                return Some(mod_file);
            }
        }

        None
    }

    fn load_module(&mut self, path: &Path) -> Result<Vec<ASTNode>, ValidationErrorKind> {
        if path.starts_with("__std__") {
            return self.load_std_module(path);
        }

        if let Some(cached) = self.loaded_modules.get(path) {
            return Ok(cached.clone());
        }

        let source = fs::read_to_string(path).map_err(|_| ValidationErrorKind::ImportNotFound {
            path: path.display().to_string(),
        })?;

        let token_iter = Token::lexer(&source).spanned();
        let mut tokens = Vec::new();

        for (token_result, _span) in token_iter {
            match token_result {
                Ok(token) => tokens.push(token),
                Err(_) => {
                    return Err(ValidationErrorKind::InvalidSyntax {
                        message: format!("Lexing error in {}", path.display()),
                    });
                }
            }
        }

        let (ast, errors) = parser::parser()
            .parse(tokens.as_slice())
            .into_output_errors();

        if !errors.is_empty() {
            return Err(ValidationErrorKind::InvalidSyntax {
                message: format!("Parse errors in {}", path.display()),
            });
        }

        let nodes = ast.ok_or_else(|| ValidationErrorKind::InvalidSyntax {
            message: format!("No AST produced for {}", path.display()),
        })?;

        self.loading_stack.push(path.to_path_buf());
        let resolved_nodes = self.resolve_imports(nodes, path);
        self.loading_stack.pop();

        self.loaded_modules
            .insert(path.to_path_buf(), resolved_nodes.clone());

        Ok(resolved_nodes)
    }

    fn load_std_module(&self, path: &Path) -> Result<Vec<ASTNode>, ValidationErrorKind> {
        // For now, return empty. In a real implementation, this would load standard library modules
        // based on the requested path (e.g., std/io, std/fs, etc.)
        // The standard library would be pre-compiled or built-in to the compiler
        let module_name = path
            .strip_prefix("__std__")
            .map_err(|_| ValidationErrorKind::ImportNotFound {
                path: path.display().to_string(),
            })?
            .to_str()
            .ok_or_else(|| ValidationErrorKind::ImportNotFound {
                path: path.display().to_string(),
            })?;

        // In a real implementation, we would load the appropriate standard library module
        // For now, return empty - this is a placeholder for the real implementation
        println!("Loading standard library module: {}", module_name);
        Ok(vec![])
    }

    fn filter_imported_items(
        &self,
        nodes: &[ASTNode],
        items: &[ImportItemKind],
        _alias: &Option<String>,
    ) -> Vec<ASTNode> {
        let import_all = items.iter().any(|item| matches!(item, ImportItemKind::All));

        if import_all {
            return nodes
                .iter()
                .filter(|n| self.is_public_item(n))
                .cloned()
                .collect();
        }

        let mut result = Vec::new();
        for node in nodes {
            let node_name = self.get_node_name(node);

            for item in items {
                match item {
                    ImportItemKind::Name(name) => {
                        if name == &node_name && self.is_public_item(node) {
                            result.push(node.clone());
                        }
                    }
                    ImportItemKind::Alias { name, alias } => {
                        if name == &node_name && self.is_public_item(node) {
                            let mut aliased = node.clone();
                            self.apply_alias(&mut aliased, alias);
                            result.push(aliased);
                        }
                    }
                    ImportItemKind::All => {}
                }
            }
        }

        result
    }

    fn is_public_item(&self, node: &ASTNode) -> bool {
        match &node.node {
            ASTNodeKind::Function(f) => f.vis == Visibility::Public,
            ASTNodeKind::Struct(s) => s.vis == Visibility::Public,
            ASTNodeKind::Enum(e) => e.vis == Visibility::Public,
            ASTNodeKind::TypeAlias(_) => true, // Type aliases are always public by default
            ASTNodeKind::Trait(_) => true,
            ASTNodeKind::EffectDef(e) => e.vis == Visibility::Public,
            _ => false,
        }
    }

    fn get_node_name(&self, node: &ASTNode) -> String {
        match &node.node {
            ASTNodeKind::Function(f) => f.name.clone(),
            ASTNodeKind::Struct(s) => s.name.clone(),
            ASTNodeKind::Enum(e) => e.name.clone(),
            ASTNodeKind::TypeAlias(t) => t.name.clone(),
            ASTNodeKind::Trait(t) => t.name.clone(),
            ASTNodeKind::EffectDef(e) => e.name.clone(),
            _ => String::new(),
        }
    }

    fn apply_alias(&self, node: &mut ASTNode, alias: &str) {
        match &mut node.node {
            ASTNodeKind::Function(f) => f.name = alias.to_string(),
            ASTNodeKind::Struct(s) => s.name = alias.to_string(),
            ASTNodeKind::Enum(e) => e.name = alias.to_string(),
            ASTNodeKind::TypeAlias(t) => t.name = alias.to_string(),
            ASTNodeKind::Trait(t) => t.name = alias.to_string(),
            ASTNodeKind::EffectDef(e) => e.name = alias.to_string(),
            _ => {}
        }
    }
}
