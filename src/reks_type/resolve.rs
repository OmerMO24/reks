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
    symbols: HashMap<String, Symbol>,
}

#[derive(Debug)]
pub struct NameResolver {
    // Stack of scopes, with innermost scope at the back
    scopes: Vec<Scope>,
}

#[derive(Debug)]
pub enum ResolverError {
    UndefinedVariable { name: String },
    UndefinedFunction { name: String },
    UndefinedType { name: String },
    Redefinition { name: String },
    AssignToImmutable { name: String },
    // Add other error types as needed
}

impl NameResolver {
    // Create a new resolver with a global scope containing built-in types
    pub fn new() -> Self {
        let mut resolver = Self { scopes: Vec::new() };

        // Create global scope
        resolver.enter_scope();

        // Register built-in types
        resolver.declare(
            "i64".to_string(),
            Symbol::BuiltinType {
                name: "i64".to_string(),
            },
        );

        resolver.declare(
            "i32".to_string(),
            Symbol::BuiltinType {
                name: "i32".to_string(),
            },
        );
        resolver.declare(
            "i16".to_string(),
            Symbol::BuiltinType {
                name: "i16".to_string(),
            },
        );
        resolver.declare(
            "i8".to_string(),
            Symbol::BuiltinType {
                name: "i8".to_string(),
            },
        );
        resolver.declare(
            "u64".to_string(),
            Symbol::BuiltinType {
                name: "u64".to_string(),
            },
        );
        resolver.declare(
            "u32".to_string(),
            Symbol::BuiltinType {
                name: "u32".to_string(),
            },
        );

        resolver.declare(
            "u16".to_string(),
            Symbol::BuiltinType {
                name: "u16".to_string(),
            },
        );

        resolver.declare(
            "u8".to_string(),
            Symbol::BuiltinType {
                name: "u8".to_string(),
            },
        );
        resolver.declare(
            "f32".to_string(),
            Symbol::BuiltinType {
                name: "f32".to_string(),
            },
        );
        resolver.declare(
            "f64".to_string(),
            Symbol::BuiltinType {
                name: "f64".to_string(),
            },
        );
        resolver.declare(
            "bool".to_string(),
            Symbol::BuiltinType {
                name: "bool".to_string(),
            },
        );
        resolver.declare(
            "string".to_string(),
            Symbol::BuiltinType {
                name: "string".to_string(),
            },
        );

        resolver
    }

    // Enter a new scope (when starting a block, function, etc.)
    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    // Exit the current scope
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    // Declare a symbol in the current scope
    pub fn declare(&mut self, name: String, symbol: Symbol) {
        let scope = self.scopes.last_mut().unwrap();

        // Just overwrite any existing declarations for now
        scope.symbols.insert(name, symbol);
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

    // Main method to resolve names in an expression
    pub fn resolve_expr(&mut self, expr: &UntypedExpr) {
        match expr {
            UntypedExpr::Value(Value::Identifier(_name)) => {
                // Just check if it exists, no error handling for now
                // We'll add more logic here later
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

                    self.declare(
                        var_name.to_string(),
                        Symbol::Variable {
                            name: var_name.to_string(),
                            mutable,
                            type_info: None, // Will be filled during type inference
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

            // Add cases for other expression types...
            _ => {
                // For now, handle other cases with a placeholder
                // We'll implement them in subsequent steps
            }
        }
    }

    // Resolve names in a complete program
    pub fn resolve_program(&mut self, program: &[UntypedExpr]) {
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
    }
}

pub fn test_name_resolver() {
    // Create a simple program to test
    let program = vec![
        // A struct declaration
        UntypedExpr::Struct {
            id: Value::Identifier("Point"),
            fields: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("f32"),
                },
                Param {
                    name: Value::Identifier("y"),
                    ty: Value::Identifier("f32"),
                },
            ],
        },
        // A function declaration
        UntypedExpr::Fn {
            name: Value::Identifier("distance"),
            params: vec![
                Param {
                    name: Value::Identifier("p1"),
                    ty: Value::Identifier("Point"),
                },
                Param {
                    name: Value::Identifier("p2"),
                    ty: Value::Identifier("Point"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("f32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // Let declaration
                    UntypedExpr::Let {
                        id: Value::Identifier("dx"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::FieldAccess {
                                id: Box::new(UntypedExpr::Value(Value::Identifier("p2"))),
                                field: Value::Identifier("x"),
                            }),
                            op: InfixOpKind::Sub,
                            right: Box::new(UntypedExpr::FieldAccess {
                                id: Box::new(UntypedExpr::Value(Value::Identifier("p1"))),
                                field: Value::Identifier("x"),
                            }),
                        }),
                        constness: Const::Yes,
                    },
                    // Variable usage
                    UntypedExpr::Value(Value::Identifier("dx")),
                ],
            }),
        },
    ];

    // Create a resolver and resolve the program
    let mut resolver = NameResolver::new();
    resolver.resolve_program(&program);

    // Add debug prints to verify the resolver state
    println!("Global scope contents:");
    for (name, symbol) in &resolver.scopes[0].symbols {
        println!("  {}: {:?}", name, symbol);
    }

    // Test specific resolution queries
    println!("\nResolution tests:");
    println!("  'Point' resolves to: {:?}", resolver.resolve("Point"));
    println!(
        "  'distance' resolves to: {:?}",
        resolver.resolve("distance")
    );
    println!("  'unknown' resolves to: {:?}", resolver.resolve("unknown"));
}
