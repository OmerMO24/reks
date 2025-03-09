use crate::reks_parse::{operators::*, utnode::*};
use crate::reks_type::resolve::{NameResolutionMap, NameResolver, NodeId, Symbol};
use std::collections::HashMap;

// Typed AST node
#[derive(Debug, Clone)]
pub struct TypedExpr<'src> {
    pub kind: UntypedExpr<'src>, // Original untyped node
    pub type_info: TypeInfo,     // Type information
    pub node_id: NodeId,         // Reference to node ID from name resolution
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Known(Type),    // We already know the type
    Unknown,        // Type needs to be inferred
    Inferred(Type), // Type has been determined by inference
}

// Core type representation
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Struct(String),
    TypeVar(usize),                 // For unknown types
    Function(Vec<Type>, Box<Type>), // For function types (params, return)
    Unit,                           // For expressions that don't return a value
}

// Type inference error
#[derive(Debug)]
pub enum TypeError {
    UnificationFail(Type, Type),
    InfiniteType(usize, Type), // Occurs check failure
    UnknownVariable(String),
    NotAFunction(Type),
    // Other error types as needed
}

// Type environment - maps variable names to their types
type TypeEnv = HashMap<NodeId, Type>;

// Substitution - maps type variables to types
type Substitution = HashMap<usize, Type>;

impl Type {
    // Check if type variable occurs in type (occurs check)
    fn occurs_in(&self, var: usize) -> bool {
        match self {
            Type::TypeVar(v) => *v == var,
            Type::Function(params, ret) => {
                params.iter().any(|p| p.occurs_in(var)) || ret.occurs_in(var)
            }
            Type::Struct(_) => false, // For now, structs don't contain type variables
            _ => false,               // Primitive types don't contain variables
        }
    }

    // Apply substitution to a type
    fn apply_subst(&self, subst: &Substitution) -> Type {
        match self {
            Type::TypeVar(v) => {
                if let Some(t) = subst.get(v) {
                    t.clone().apply_subst(subst) // Follow substitution chain
                } else {
                    self.clone()
                }
            }
            Type::Function(params, ret) => {
                let new_params = params.iter().map(|p| p.apply_subst(subst)).collect();
                let new_ret = ret.apply_subst(subst);
                Type::Function(new_params, Box::new(new_ret))
            }
            _ => self.clone(), // Primitive types unchanged
        }
    }
}

// The unification algorithm
fn unify(t1: &Type, t2: &Type, subst: &Substitution) -> Result<Substitution, TypeError> {
    let t1 = t1.apply_subst(subst);
    let t2 = t2.apply_subst(subst);

    match (&t1, &t2) {
        // Same primitive types unify trivially
        (Type::Int, Type::Int)
        | (Type::Float, Type::Float)
        | (Type::Bool, Type::Bool)
        | (Type::String, Type::String)
        | (Type::Unit, Type::Unit) => Ok(subst.clone()),

        // Same struct types unify
        (Type::Struct(n1), Type::Struct(n2)) if n1 == n2 => Ok(subst.clone()),

        // Function types unify if parameters and return types unify
        (Type::Function(p1, r1), Type::Function(p2, r2)) => {
            if p1.len() != p2.len() {
                return Err(TypeError::UnificationFail(t1.clone(), t2.clone()));
            }

            let mut new_subst = subst.clone();

            // Unify parameters
            for (param1, param2) in p1.iter().zip(p2.iter()) {
                new_subst = unify(param1, param2, &new_subst)?;
            }

            // Unify return types
            unify(r1, r2, &new_subst)
        }

        // Type variable unifies with anything (with occurs check)
        (Type::TypeVar(v), _) => {
            if let Type::TypeVar(v2) = t2 {
                if *v == v2 {
                    return Ok(subst.clone());
                }
            }

            if t2.occurs_in(*v) {
                return Err(TypeError::InfiniteType(*v, t2.clone()));
            }

            let mut new_subst = subst.clone();
            new_subst.insert(*v, t2.clone());
            Ok(new_subst)
        }

        (_, Type::TypeVar(v)) => unify(&Type::TypeVar(*v), &t1, subst),

        // Otherwise, types don't unify
        _ => Err(TypeError::UnificationFail(t1.clone(), t2.clone())),
    }
}

pub struct TypeInferencer {
    // Type environment
    env: TypeEnv,
    // Current substitution
    subst: Substitution,
    // Counter for generating fresh type variables
    next_var_id: usize,
    // Name resolution map from the previous phase
    resolution_map: NameResolutionMap,
}

impl TypeInferencer {
    pub fn new(resolution_map: NameResolutionMap) -> Self {
        Self {
            env: HashMap::new(),
            subst: HashMap::new(),
            next_var_id: 0,
            resolution_map,
        }
    }

    // Generate a fresh type variable
    fn fresh_type_var(&mut self) -> Type {
        let var_id = self.next_var_id;
        self.next_var_id += 1;
        Type::TypeVar(var_id)
    }

    // Convert untyped AST to typed AST
    pub fn convert_to_typed_ast<'a>(&mut self, expr: &UntypedExpr<'a>) -> TypedExpr<'a> {
        // Generate a node ID for this expression
        let node_id = self.resolution_map.get_id();

        // Determine the initial type information
        let type_info = match expr {
            UntypedExpr::Value(Value::Num(_)) => TypeInfo::Known(Type::Int),
            UntypedExpr::Value(Value::Float(_)) => TypeInfo::Known(Type::Float),
            UntypedExpr::Value(Value::String(_)) => TypeInfo::Known(Type::String),
            UntypedExpr::Value(Value::Identifier(_)) => {
                // For identifiers, we'll check if we know the type from name resolution
                TypeInfo::Unknown // For now, assume unknown
            }
            UntypedExpr::Let { pat, .. } => {
                // Check if there's a type annotation
                match pat {
                    TypePath::Typed {
                        ident: Value::Identifier(type_name),
                    } => match self.type_from_annotation(type_name) {
                        Ok(t) => TypeInfo::Known(t),
                        Err(_) => TypeInfo::Unknown,
                    },
                    _ => TypeInfo::Unknown,
                }
            }
            _ => TypeInfo::Unknown,
        };

        let typed_expr = TypedExpr {
            kind: expr.clone(),
            type_info,
            node_id,
        };

        typed_expr
    }

    pub fn convert_program<'a>(&mut self, program: &[UntypedExpr<'a>]) -> Vec<TypedExpr<'a>> {
        let mut result = Vec::with_capacity(program.len());
        for expr in program {
            result.push(self.convert_to_typed_ast(expr));
        }
        result
    }

    // Helper to convert type annotations to Type
    fn type_from_annotation(&self, type_name: &str) -> Result<Type, TypeError> {
        match type_name {
            "i64" | "i32" | "i16" | "i8" | "u64" | "u32" | "u16" | "u8" => Ok(Type::Int),
            "f64" | "f32" => Ok(Type::Float),
            "bool" => Ok(Type::Bool),
            "string" => Ok(Type::String),
            _ => Ok(Type::Struct(type_name.to_string())), // Assume it's a struct type
        }
    }

    // Fix 1: Enhanced function type inference
    fn infer_expr(&mut self, expr: &mut TypedExpr) -> Result<Type, TypeError> {
        // If we already know the type, return it
        if let TypeInfo::Known(t) | TypeInfo::Inferred(t) = &expr.type_info {
            return Ok(t.clone());
        }

        let inferred_type = match &expr.kind {
            UntypedExpr::Value(Value::Num(_)) => Type::Int,
            UntypedExpr::Value(Value::Float(_)) => Type::Float,
            UntypedExpr::Value(Value::String(_)) => Type::String,

            UntypedExpr::Value(Value::Identifier(name)) => {
                // Use name resolution to find the declaration
                if let Some(decl_id) = self.resolution_map.resolve_name(name) {
                    if let Some(t) = self.env.get(&decl_id) {
                        t.clone()
                    } else {
                        return Err(TypeError::UnknownVariable(name.to_string()));
                    }
                } else {
                    return Err(TypeError::UnknownVariable(name.to_string()));
                }
            }

            UntypedExpr::BinOp { left, op, right } => {
                // Convert operands to typed expressions
                let mut left_typed = self.convert_to_typed_ast(left);
                let mut right_typed = self.convert_to_typed_ast(right);

                // Infer types of operands
                let t_left = self.infer_expr(&mut left_typed)?;
                let t_right = self.infer_expr(&mut right_typed)?;

                match op {
                    // Arithmetic operations
                    InfixOpKind::Add | InfixOpKind::Sub | InfixOpKind::Mul | InfixOpKind::Div => {
                        // For simplicity, allow numeric operations only between same types
                        let new_subst = unify(&t_left, &t_right, &self.subst)?;
                        self.subst = new_subst;

                        // Type depends on operands
                        let final_type = t_left.apply_subst(&self.subst);
                        if final_type == Type::Int || final_type == Type::Float {
                            final_type
                        } else {
                            // Try to unify with numeric types
                            let num_type = Type::Int; // Default to Int for now
                            let new_subst = unify(&final_type, &num_type, &self.subst)?;
                            self.subst = new_subst;
                            num_type
                        }
                    }

                    // Comparison operations
                    InfixOpKind::Less
                    | InfixOpKind::Greater
                    | InfixOpKind::LessOrEq
                    | InfixOpKind::GreaterOrEq
                    | InfixOpKind::Equals
                    | InfixOpKind::NotEq => {
                        // Operands must be the same type
                        let new_subst = unify(&t_left, &t_right, &self.subst)?;
                        self.subst = new_subst;
                        Type::Bool
                    }

                    // Logical operations
                    InfixOpKind::And | InfixOpKind::Or => {
                        // Both operands must be boolean
                        let new_subst = unify(&t_left, &Type::Bool, &self.subst)?;
                        self.subst = new_subst;
                        let new_subst = unify(&t_right, &Type::Bool, &self.subst)?;
                        self.subst = new_subst;
                        Type::Bool
                    }

                    // Handle other operators as needed
                    _ => self.fresh_type_var(),
                }
            }

            UntypedExpr::Let {
                id,
                pat,
                expr: init_expr,
                ..
            } => {
                // Infer the type of the initializer
                let mut init_typed = self.convert_to_typed_ast(init_expr);
                let init_type = self.infer_expr(&mut init_typed)?;

                // If there's a type annotation, unify with it
                let var_type = match pat {
                    TypePath::Typed {
                        ident: Value::Identifier(type_name),
                    } => {
                        // Convert the type annotation to a Type
                        let ann_type = self.type_from_annotation(type_name)?;
                        let new_subst = unify(&init_type, &ann_type, &self.subst)?;
                        self.subst = new_subst;
                        ann_type
                    }
                    _ => init_type,
                };

                // Add the variable to the environment
                if let Value::Identifier(name) = id {
                    // Use the declaration ID from name resolution
                    if let Some(decl_id) = self.resolution_map.get_declaration_id(name) {
                        self.env.insert(decl_id, var_type.clone());
                    }
                }

                var_type
            }

            UntypedExpr::Call { name, args } => {
                // Infer the function type
                let mut name_typed = self.convert_to_typed_ast(name);
                let func_type = self.infer_expr(&mut name_typed)?;

                // Infer types of arguments
                let mut arg_types = Vec::new();
                for arg in args {
                    let mut arg_typed = self.convert_to_typed_ast(arg);
                    arg_types.push(self.infer_expr(&mut arg_typed)?);
                }

                // Fresh type variable for the return type
                let ret_type = self.fresh_type_var();

                // Create a function type and unify with the inferred function type
                let expected_func_type = Type::Function(arg_types, Box::new(ret_type.clone()));

                match unify(&func_type, &expected_func_type, &self.subst) {
                    Ok(new_subst) => {
                        self.subst = new_subst;
                        // Return the (potentially updated) return type
                        ret_type.apply_subst(&self.subst)
                    }
                    Err(_) => {
                        return Err(TypeError::NotAFunction(func_type));
                    }
                }
            }

            UntypedExpr::Block { statements } => {
                // Handle a block of expressions
                let mut block_type = Type::Unit; // Default for empty blocks

                for stmt in statements {
                    let mut stmt_typed = self.convert_to_typed_ast(stmt);
                    block_type = self.infer_expr(&mut stmt_typed)?;
                }

                // The type of a block is the type of its last expression
                block_type
            }

            UntypedExpr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Infer type of condition - must be boolean
                let mut cond_typed = self.convert_to_typed_ast(condition);
                let cond_type = self.infer_expr(&mut cond_typed)?;
                let new_subst = unify(&cond_type, &Type::Bool, &self.subst)?;
                self.subst = new_subst;

                // Infer types of branches - must be the same
                let mut then_typed = self.convert_to_typed_ast(then_branch);
                let then_type = self.infer_expr(&mut then_typed)?;

                let mut else_typed = self.convert_to_typed_ast(else_branch);
                let else_type = self.infer_expr(&mut else_typed)?;

                // Unify branch types
                let new_subst = unify(&then_type, &else_type, &self.subst)?;
                self.subst = new_subst;

                // Return the unified type
                then_type.apply_subst(&self.subst)
            }

            UntypedExpr::Fn {
                name,
                params,
                retty,
                body,
            } => {
                // Infer return type from annotation
                let mut ret_typed = self.convert_to_typed_ast(retty);
                let declared_ret_type = self.infer_expr(&mut ret_typed)?;

                // Create a new scope for function parameters
                //self.enter_scope();

                // Process parameters
                let mut param_types = Vec::new();
                for param in params {
                    if let Value::Identifier(param_name) = &param.name {
                        if let Value::Identifier(type_name) = &param.ty {
                            let param_type = self.type_from_annotation(type_name)?;
                            param_types.push(param_type.clone());

                            // Add parameter to environment
                            if let Some(decl_id) =
                                self.resolution_map.get_declaration_id(param_name)
                            {
                                self.env.insert(decl_id, param_type);
                            }
                        }
                    }
                }

                // Infer body type
                let mut body_typed = self.convert_to_typed_ast(body);
                let body_type = self.infer_expr(&mut body_typed)?;

                // Exit function scope
                // self.exit_scope();

                // Unify body result with declared return type
                let new_subst = unify(&body_type, &declared_ret_type, &self.subst)?;
                self.subst = new_subst;

                // Create function type
                let fn_type = Type::Function(param_types, Box::new(declared_ret_type.clone()));

                // Update function type in environment
                if let Value::Identifier(fn_name) = name {
                    if let Some(decl_id) = self.resolution_map.get_declaration_id(fn_name) {
                        self.env.insert(decl_id, fn_type.clone());
                    }
                }

                fn_type
            }

            UntypedExpr::FieldAccess { id, field } => {
                // Infer type of the object
                let mut obj_typed = self.convert_to_typed_ast(id);
                let obj_type = self.infer_expr(&mut obj_typed)?;

                // For struct types, we would lookup the field type
                if let Type::Struct(struct_name) = &obj_type {
                    // Look up struct definition and field type
                    // For now, just return a fresh type variable
                    self.fresh_type_var()
                } else {
                    // Error: tried to access field on non-struct type
                    return Err(TypeError::UnificationFail(
                        obj_type,
                        Type::Struct("expected_struct".to_string()),
                    ));
                }
            }

            UntypedExpr::Assign { left, right } => {
                // Infer types of both sides
                let mut left_typed = self.convert_to_typed_ast(left);
                let mut right_typed = self.convert_to_typed_ast(right);

                let left_type = self.infer_expr(&mut left_typed)?;
                let right_type = self.infer_expr(&mut right_typed)?;

                // Left and right types must be compatible
                let new_subst = unify(&left_type, &right_type, &self.subst)?;
                self.subst = new_subst;

                // Assignment expressions have the type of the right side
                right_type
            }

            UntypedExpr::List { items } => {
                // For simplicity, assume all list items have the same type
                if items.is_empty() {
                    // Empty list - can't infer item type
                    self.fresh_type_var()
                } else {
                    // Infer type of first item
                    let mut first_typed = self.convert_to_typed_ast(&items[0]);
                    let item_type = self.infer_expr(&mut first_typed)?;

                    // Check that all other items have the same type
                    for item in &items[1..] {
                        let mut item_typed = self.convert_to_typed_ast(item);
                        let next_type = self.infer_expr(&mut item_typed)?;

                        let new_subst = unify(&item_type, &next_type, &self.subst)?;
                        self.subst = new_subst;
                    }

                    // The list type is determined by its items
                    // In a real implementation, you'd have a List(Type) variant
                    item_type
                }
            }

            // UntypedExpr::Struct { .. } => {
            //     // Struct declarations are handled in the first pass
            //     // Here we just return the struct type
            //     if let Value::Identifier(struct_name) = id {
            //         Type::Struct(struct_name.to_string())
            //     } else {
            //         self.fresh_type_var()
            //     }
            // }

            // Default for unhandled expression types
            _ => self.fresh_type_var(),
        };

        // Update the expression's type information
        expr.type_info = TypeInfo::Inferred(inferred_type.clone());

        Ok(inferred_type)
    }

    // Fix 2: Apply substitutions thoroughly in infer_program
    pub fn infer_program(&mut self, program: &mut [TypedExpr]) -> Result<(), Vec<TypeError>> {
        // Initialize environment with known types from declarations
        self.initialize_env_from_declarations();

        // Collect errors during inference
        let mut errors = Vec::new();

        // Infer types for each expression
        for expr in &mut *program {
            match self.infer_expr(expr) {
                Ok(_) => {}
                Err(err) => errors.push(err),
            }
        }

        // Print substitution for debugging
        println!("Substitution map size: {}", self.subst.len());
        for (var_id, typ) in &self.subst {
            println!("  TypeVar({}) => {:?}", var_id, typ);
        }

        // Apply the final substitution to all expression types
        // This is a deep recursive application to ensure all type variables are resolved
        self.apply_substitution_to_all_expressions(program);

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    // New helper function to thoroughly apply substitutions
    fn apply_substitution_to_all_expressions(&mut self, expressions: &mut [TypedExpr]) {
        let substitution = self.subst.clone();

        for expr in expressions {
            self.apply_substitution_to_expr(expr, &substitution);
        }
    }

    fn apply_substitution_to_expr(&mut self, expr: &mut TypedExpr, substitution: &Substitution) {
        // Apply substitution to this expression's type
        if let TypeInfo::Inferred(ref mut t) = &mut expr.type_info {
            *t = t.apply_subst(substitution);
        }

        // Recursively apply substitution to child expressions
        match &mut expr.kind {
            UntypedExpr::Block { statements } => {
                for stmt in statements {
                    let mut stmt_typed = self.convert_to_typed_ast(stmt);
                    self.apply_substitution_to_expr(&mut stmt_typed, substitution);
                }
            }

            UntypedExpr::Fn { body, .. } => {
                let mut body_typed = self.convert_to_typed_ast(body);
                self.apply_substitution_to_expr(&mut body_typed, substitution);
            }

            UntypedExpr::Let { expr, .. } => {
                let mut init_typed = self.convert_to_typed_ast(expr);
                self.apply_substitution_to_expr(&mut init_typed, substitution);
            }

            UntypedExpr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut cond_typed = self.convert_to_typed_ast(condition);
                self.apply_substitution_to_expr(&mut cond_typed, substitution);

                let mut then_typed = self.convert_to_typed_ast(then_branch);
                self.apply_substitution_to_expr(&mut then_typed, substitution);

                let mut else_typed = self.convert_to_typed_ast(else_branch);
                self.apply_substitution_to_expr(&mut else_typed, substitution);
            }

            UntypedExpr::BinOp { left, right, .. } => {
                let mut left_typed = self.convert_to_typed_ast(left);
                self.apply_substitution_to_expr(&mut left_typed, substitution);

                let mut right_typed = self.convert_to_typed_ast(right);
                self.apply_substitution_to_expr(&mut right_typed, substitution);
            }

            UntypedExpr::Call { name, args } => {
                let mut name_typed = self.convert_to_typed_ast(name);
                self.apply_substitution_to_expr(&mut name_typed, substitution);

                for arg in args {
                    let mut arg_typed = self.convert_to_typed_ast(arg);
                    self.apply_substitution_to_expr(&mut arg_typed, substitution);
                }
            }

            // Handle other expressions with child nodes
            _ => {}
        }
    }

    fn initialize_env_from_declarations(&mut self) {
        // First, collect all the information we need
        let mut declarations_to_process = Vec::new();

        for (id, decl_info) in self.resolution_map.get_declarations() {
            match &decl_info.symbol {
                Symbol::Variable { type_info, .. } => {
                    declarations_to_process.push((
                        *id,
                        "variable",
                        type_info.clone(),
                        Vec::<(String, String)>::new(),
                        String::new(),
                    ));
                }
                Symbol::Function {
                    params,
                    return_type,
                    ..
                } => {
                    declarations_to_process.push((
                        *id,
                        "function",
                        None,
                        params.clone(),
                        return_type.clone(),
                    ));
                }
                _ => {} // Other symbol types
            }
        }

        // Now process the collected data without borrowing conflicts
        for (id, kind, type_info, params, return_type) in declarations_to_process {
            match kind {
                "variable" => {
                    // Handle variable declarations
                    let var_type = match type_info {
                        Some(ti) => {
                            // Convert TypeInfo to Type
                            // (Implementation depends on your TypeInfo)
                            self.fresh_type_var() // Placeholder
                        }
                        None => self.fresh_type_var(),
                    };
                    self.env.insert(id, var_type);
                }
                "function" => {
                    // Create function type from parameters and return type
                    let param_types: Vec<Type> = params
                        .iter()
                        .map(|(_, type_name)| {
                            self.type_from_annotation(type_name)
                                .unwrap_or_else(|_| self.fresh_type_var())
                        })
                        .collect();

                    let ret_type = if return_type.is_empty() {
                        Type::Unit
                    } else {
                        self.type_from_annotation(&return_type)
                            .unwrap_or_else(|_| self.fresh_type_var())
                    };

                    let fn_type = Type::Function(param_types, Box::new(ret_type));
                    self.env.insert(id, fn_type);
                }
                _ => unreachable!(),
            }
        }
    }
}

pub fn test_type_inference() {
    // A simple program with variables, function call, and binary operations
    let test_program = vec![
        // Function declaration
        UntypedExpr::Fn {
            name: Value::Identifier("add"),
            params: vec![
                Param {
                    name: Value::Identifier("a"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("b"),
                    ty: Value::Identifier("i32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // a + b
                    UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                        op: InfixOpKind::Add,
                        right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
                    },
                ],
            }),
        },
        // Main function with variable declaration and function call
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // Variable without type annotation
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(5))),
                        constness: Const::Yes,
                    },
                    // Variable with type annotation
                    UntypedExpr::Let {
                        id: Value::Identifier("y"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("i32"),
                        },
                        expr: Box::new(UntypedExpr::Value(Value::Num(10))),
                        constness: Const::Yes,
                    },
                    // Function call
                    UntypedExpr::Call {
                        name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                        args: vec![
                            UntypedExpr::Value(Value::Identifier("x")),
                            UntypedExpr::Value(Value::Identifier("y")),
                        ],
                    },
                ],
            }),
        },
    ];

    // First, do name resolution
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);

    // Then do type inference
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    // Run inference
    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            println!("\nFinal substitution:");
            for (var_id, typ) in &inferencer.subst {
                println!("  TypeVar({}) => {:?}", var_id, typ);
            }

            println!("\nInferred types for top-level expressions:");
            for (i, expr) in typed_ast.iter().enumerate() {
                print_typed_expr(expr, 0, i);
            }
        }
        Err(errors) => {
            println!("Type errors:");
            for err in errors {
                println!("  {:?}", err);
            }
        }
    }
}

// Helper function to print a typed expression with indentation
pub fn print_typed_expr(expr: &TypedExpr, indent: usize, index: usize) {
    let indent_str = " ".repeat(indent);

    match &expr.type_info {
        TypeInfo::Known(t) => println!("{}Expression {}: Known type {:?}", indent_str, index, t),
        TypeInfo::Inferred(t) => {
            println!("{}Expression {}: Inferred type {:?}", indent_str, index, t)
        }
        TypeInfo::Unknown => println!("{}Expression {}: Unknown type", indent_str, index),
    }

    // Print kind of expression for clarity
    match &expr.kind {
        UntypedExpr::Value(Value::Identifier(name)) => {
            println!("{}  Kind: Identifier '{}'", indent_str, name);
        }
        UntypedExpr::Value(Value::Num(n)) => {
            println!("{}  Kind: Number literal {}", indent_str, n);
        }
        UntypedExpr::Let {
            id: Value::Identifier(name),
            ..
        } => {
            println!("{}  Kind: Let binding for '{}'", indent_str, name);
        }
        UntypedExpr::BinOp { .. } => {
            println!("{}  Kind: Binary operation", indent_str);
        }
        UntypedExpr::Call { .. } => {
            println!("{}  Kind: Function call", indent_str);
        }
        UntypedExpr::Fn {
            name: Value::Identifier(name),
            ..
        } => {
            println!("{}  Kind: Function definition '{}'", indent_str, name);
        }
        _ => {
            println!("{}  Kind: Other expression", indent_str);
        }
    }
}
