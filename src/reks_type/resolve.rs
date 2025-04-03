use crate::reks_parse::operators::*;
use crate::reks_parse::utnode::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable {
        name: String,
        mutable: bool,
        type_info: Option<TypeInfo>, // We'll add the type during type inference
    },
    Function {
        name: String,
        params: Vec<(String, String)>, // (name, type)
        return_type: String,
    },
    Struct {
        name: String,
        fields: Vec<(String, String)>, // (name, type)
    },
    BuiltinType {
        name: String,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    // Built-in primitive types
    Int,
    Float,
    Bool,
    String,

    // User-defined struct type
    Struct(String), // The name of the struct

    // For when we don't know the type yet (will be filled in during type inference)
    Unknown,

    // For representing type variables in Hindley-Milner inference
    TypeVar(usize),
}

#[derive(Debug, Default)]
pub struct Scope {
    pub(crate) symbols: HashMap<String, Symbol>,
}

// #[derive(Debug)]
// pub struct NameResolver {
//     // Stack of scopes, with innermost scope at the back
//     scopes: Vec<Scope>,
// }

#[derive(Debug)]
pub enum ResolverError {
    UndefinedVariable { name: String },
    UndefinedFunction { name: String },
    UndefinedType { name: String },
    Redefinition { name: String },
    AssignToImmutable { name: String },
    // Add other error types as needed
}

// Use a unique identifier for AST nodes
pub type NodeId = usize;

// Information about a declaration
#[derive(Debug, Clone)]
pub struct DeclarationInfo {
    pub(crate) name: String,
    pub(crate) symbol: Symbol,
    pub(crate) scope_level: usize,
}

// The resolution map that stores all the binding information
#[derive(Debug, Clone)]
pub struct NameResolutionMap {
    // Maps from an expression node to its declaration
    pub(crate) bindings: HashMap<NodeId, NodeId>,

    // Maps from a declaration node to its information
    pub(crate) declarations: HashMap<NodeId, DeclarationInfo>,

    // Counter for generating unique IDs
    pub(crate) next_id: NodeId,
}

impl NameResolutionMap {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            declarations: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn get_id(&mut self) -> NodeId {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    // Record a new declaration
    fn record_declaration(
        &mut self,
        expr_id: NodeId,
        name: String,
        symbol: Symbol,
        scope_level: usize,
    ) {
        self.declarations.insert(
            expr_id,
            DeclarationInfo {
                name,
                symbol,
                scope_level,
            },
        );
    }

    // Record that a reference points to a declaration
    fn record_binding(&mut self, ref_id: NodeId, decl_id: NodeId) {
        self.bindings.insert(ref_id, decl_id);
    }

    // Look up the declaration for a reference
    pub(crate) fn resolve_reference(&self, ref_id: &NodeId) -> Option<&DeclarationInfo> {
        self.bindings
            .get(ref_id)
            .and_then(|decl_id| self.declarations.get(decl_id))
    }

    pub fn resolve_name(&self, name: &str) -> Option<NodeId> {
        // Look through bindings to find the declaration for this name
        for (decl_id, decl_info) in &self.declarations {
            if decl_info.name == name {
                return Some(*decl_id);
            }
        }
        None
    }

    pub fn get_declaration_id(&self, name: &str) -> Option<NodeId> {
        // Similar to resolve_name but for declarations
        for (decl_id, info) in &self.declarations {
            if info.name == name {
                return Some(*decl_id);
            }
        }
        None
    }

    pub fn get_declarations(&self) -> &HashMap<NodeId, DeclarationInfo> {
        &self.declarations
    }
}

// Modify the NameResolver struct to include the resolution map
#[derive(Debug)]
pub struct NameResolver {
    // Stack of scopes, with innermost scope at the back
    scopes: Vec<Scope>,
    // Resolution map to store binding information
    resolution_map: NameResolutionMap,
    // Map from variable names to their declaration IDs in each scope
    name_to_id: Vec<HashMap<String, NodeId>>,
}

impl NameResolver {
    // Create a new resolver with a global scope containing built-in types
    pub fn new() -> Self {
        let mut resolver = Self {
            scopes: Vec::new(),
            resolution_map: NameResolutionMap::new(),
            name_to_id: Vec::new(),
        };

        // Create global scope
        resolver.enter_scope();

        // Register built-in types
        resolver.declare_builtin("i64".to_string());
        resolver.declare_builtin("i32".to_string());
        resolver.declare_builtin("i16".to_string());
        resolver.declare_builtin("i8".to_string());
        resolver.declare_builtin("u64".to_string());
        resolver.declare_builtin("u32".to_string());
        resolver.declare_builtin("u16".to_string());
        resolver.declare_builtin("u8".to_string());
        resolver.declare_builtin("f32".to_string());
        resolver.declare_builtin("f64".to_string());
        resolver.declare_builtin("bool".to_string());
        resolver.declare_builtin("string".to_string());

        resolver
    }

    // Helper method to declare built-in types
    fn declare_builtin(&mut self, name: String) {
        let symbol = Symbol::BuiltinType { name: name.clone() };
        self.declare(name, symbol);
    }

    // Enter a new scope (when starting a block, function, etc.)
    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::default());
        self.name_to_id.push(HashMap::new());
    }

    // Exit the current scope
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
        self.name_to_id.pop();
    }

    // Declare a symbol in the current scope
    // Declare a symbol in the current scope
    pub fn declare(&mut self, name: String, symbol: Symbol) -> NodeId {
        // Calculate scope level first (immutable borrow)
        let scope_level = self.scopes.len() - 1;

        // Now get mutable reference to current scope
        let scope = self.scopes.last_mut().unwrap();

        // Generate ID for the declaration
        let id = self.resolution_map.get_id();

        // Add to current scope
        scope.symbols.insert(name.clone(), symbol.clone());

        // Record declaration in resolution map
        self.resolution_map
            .record_declaration(id, name.clone(), symbol, scope_level);

        // Store name to ID mapping in the current scope
        self.name_to_id.last_mut().unwrap().insert(name, id);

        id
    }

    // Resolve a name by searching scopes from innermost to outermost
    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.symbols.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    // Resolve a name to its declaration ID
    pub fn resolve_to_id(&self, name: &str) -> Option<NodeId> {
        for scope_map in self.name_to_id.iter().rev() {
            if let Some(id) = scope_map.get(name) {
                return Some(*id);
            }
        }
        None
    }

    // Modified to return the node ID
    pub fn resolve_expr(&mut self, expr: &UntypedExpr) -> NodeId {
        let node_id = self.resolution_map.get_id();

        match expr {
            UntypedExpr::Value(Value::Identifier(name)) => {
                // Resolve variable reference
                if let Some(decl_id) = self.resolve_to_id(name) {
                    // Record the binding
                    self.resolution_map.record_binding(node_id, decl_id);
                }
            }
            UntypedExpr::Index { expr, index } => {
                self.resolve_expr(expr);
                self.resolve_expr(index);
            }
            UntypedExpr::Value(_) => {
                // Literal values don't need name resolution
            }

            UntypedExpr::Let {
                id,
                pat,
                expr: init_expr,
                constness,
            } => {
                // Resolve the initializer expression first
                self.resolve_expr(init_expr);

                // Declare the variable
                if let Value::Identifier(var_name) = id {
                    let mutable = match constness {
                        Const::Yes => false,
                        Const::No => true,
                    };

                    let type_info = match pat {
                        TypePath::Typed {
                            ident: Value::Identifier(type_name),
                        } => {
                            // Map the type name to the appropriate TypeInfo
                            match *type_name {
                                "i64" | "i32" | "i16" | "i8" | "u64" | "u32" | "u16" | "u8" => {
                                    Some(TypeInfo::Int)
                                }
                                "f32" | "f64" => Some(TypeInfo::Float),
                                "bool" => Some(TypeInfo::Bool),
                                "string" => Some(TypeInfo::String),
                                // For struct or other user-defined types
                                _ => Some(TypeInfo::Struct(type_name.to_string())),
                            }
                        }
                        _ => None,
                    };

                    self.declare(
                        var_name.to_string(),
                        Symbol::Variable {
                            name: var_name.to_string(),
                            mutable,
                            type_info,
                        },
                    );
                }
            }

            UntypedExpr::Block { statements } => {
                // Create a new scope for the block
                self.enter_scope();

                // Resolve each statement in the block
                for stmt in statements {
                    self.resolve_expr(stmt);
                }

                // Exit the block scope
                self.exit_scope();
            }

            UntypedExpr::Fn {
                name,
                params,
                retty,
                body,
            } => {
                // Resolve return type
                self.resolve_expr(retty);

                // Create a new scope for function parameters and body
                self.enter_scope();

                // Declare parameters in the function scope
                for param in params {
                    if let Value::Identifier(param_name) = &param.name {
                        if let Value::Identifier(type_name) = &param.ty {
                            let type_info = match *type_name {
                                "i64" | "i32" | "i16" | "i8" | "u64" | "u32" | "u16" | "u8" => {
                                    Some(TypeInfo::Int)
                                }
                                "f32" | "f64" => Some(TypeInfo::Float),
                                "bool" => Some(TypeInfo::Bool),
                                "string" => Some(TypeInfo::String),
                                // For struct or other user-defined types
                                _ => Some(TypeInfo::Struct(type_name.to_string())),
                            };

                            self.declare(
                                param_name.to_string(),
                                Symbol::Variable {
                                    name: param_name.to_string(),
                                    mutable: false, // Parameters are immutable by default
                                    type_info,
                                },
                            );
                        }
                    }
                }

                // Resolve the function body
                self.resolve_expr(body);

                // Exit the function scope
                self.exit_scope();
            }

            UntypedExpr::Call { name, args, .. } => {
                // Resolve the function name
                let name_id = self.resolve_expr(name);

                // If this is a direct function reference, we can mark it
                if let UntypedExpr::Value(Value::Identifier(fn_name)) = &**name {
                    if let Some(decl_id) = self.resolve_to_id(fn_name) {
                        self.resolution_map.record_binding(name_id, decl_id);
                    }
                }

                // Resolve arguments
                for arg in args {
                    self.resolve_expr(arg);
                }
            }

            UntypedExpr::BinOp { left, op: _, right } => {
                // Resolve operands
                self.resolve_expr(left);
                self.resolve_expr(right);
            }

            UntypedExpr::UnaryOp { op: _, operand } => {
                // Resolve operand
                self.resolve_expr(operand);
            }

            UntypedExpr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Resolve condition
                self.resolve_expr(condition);

                // Resolve branches
                self.resolve_expr(then_branch);
                self.resolve_expr(else_branch);
            }

            UntypedExpr::FieldAccess { id, field: _ } => {
                // Resolve the object being accessed
                self.resolve_expr(id);

                // Field access resolution would typically need type information
                // We'll handle this more completely during type inference
            }

            UntypedExpr::Assign { left, right } => {
                // Resolve both sides of the assignment
                self.resolve_expr(left);
                self.resolve_expr(right);

                // Later we might want to check if the left side is assignable
            }

            UntypedExpr::List { items } => {
                // Resolve each item in the list
                for item in items {
                    self.resolve_expr(item);
                }
            }
            UntypedExpr::StructInit { id, fields } => {
                // Resolve the struct type name
                if let Value::Identifier(struct_name) = id {
                    if let Some(decl_id) = self.resolve_to_id(struct_name) {
                        self.resolution_map.record_binding(node_id, decl_id);
                    } else {
                        println!("Warning: Struct type '{}' not found", struct_name);
                        // Could add an error here if strict checking is desired
                    }
                }

                // Resolve field value expressions
                for (_, value_expr) in fields {
                    self.resolve_expr(value_expr);
                }
            }
            UntypedExpr::Struct { .. } => {
                // Struct declarations are handled in the first pass of resolve_program
                // Nothing to do here for name resolution
            }
            UntypedExpr::While { guard, body } => {
                // Resolve the guard expression (no new scope needed)
                self.resolve_expr(guard);
                // Enter a new scope for the body
                self.enter_scope();
                self.resolve_expr(body);
                self.exit_scope();
            }
            UntypedExpr::For {
                var,
                iterable,
                body,
            } => {
                // Resolve the iterable expression (outside the loop scope)
                self.resolve_expr(iterable);
                // Enter a new scope for the loop body
                self.enter_scope();
                // Declare the loop variable
                if let Value::Identifier(var_name) = var {
                    let decl_id = self.declare(
                        var_name.to_string(),
                        Symbol::Variable {
                            name: var_name.to_string(),
                            mutable: false,  // Loop vars are typically immutable
                            type_info: None, // Type inferred later
                        },
                    );
                    // Optionally bind the variable to its declaration (if needed for later phases)
                    self.resolution_map.record_binding(node_id, decl_id);
                }
                // Resolve the body within the new scope
                self.resolve_expr(body);
                self.exit_scope();
            }
        }

        node_id
    }

    // Modified to populate the resolution map
    pub fn resolve_program(&mut self, program: &Vec<UntypedExpr<'_>>) -> &NameResolutionMap {
        // First pass: register all top-level declarations
        for expr in program {
            match expr {
                UntypedExpr::Fn { name, params, .. } => {
                    if let Value::Identifier(fn_name) = name {
                        // Register function symbol
                        self.declare(
                            fn_name.to_string(),
                            Symbol::Function {
                                name: fn_name.to_string(),
                                params: params
                                    .iter()
                                    .map(|p| {
                                        if let Value::Identifier(param_name) = &p.name {
                                            if let Value::Identifier(type_name) = &p.ty {
                                                return (
                                                    param_name.to_string(),
                                                    type_name.to_string(),
                                                );
                                            }
                                        }
                                        // Fallback - should never happen with valid AST
                                        ("".to_string(), "".to_string())
                                    })
                                    .collect(),
                                return_type: "".to_string(), // Will be filled after parsing return type
                            },
                        );
                    }
                }
                UntypedExpr::Struct { id, fields } => {
                    if let Value::Identifier(struct_name) = id {
                        // Register struct symbol
                        self.declare(
                            struct_name.to_string(),
                            Symbol::Struct {
                                name: struct_name.to_string(),
                                fields: fields
                                    .iter()
                                    .map(|f| {
                                        if let Value::Identifier(field_name) = &f.name {
                                            if let Value::Identifier(type_name) = &f.ty {
                                                return (
                                                    field_name.to_string(),
                                                    type_name.to_string(),
                                                );
                                            }
                                        }
                                        // Fallback
                                        ("".to_string(), "".to_string())
                                    })
                                    .collect(),
                            },
                        );
                    }
                }
                _ => {}
            }
        }

        // Second pass: resolve all expressions
        for expr in program {
            self.resolve_expr(expr);
        }

        // Return the resolution map for use in later phases
        &self.resolution_map
    }

    // Get the resolution map
    pub fn get_resolution_map(&self) -> &NameResolutionMap {
        &self.resolution_map
    }
}
