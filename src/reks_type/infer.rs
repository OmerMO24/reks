use std::collections::{HashMap, HashSet};

// Assuming these are defined in your existing code
use crate::reks_parse::operators::InfixOpKind; // Adjust path as needed
use crate::reks_parse::utnode::{Const, Param, TypePath, UntypedExpr, Value}; // Adjust path
use crate::reks_type::resolve::{NameResolutionMap, NodeId}; // Adjust path

// Typed AST Node
#[derive(Debug, Clone)]
pub struct TypedExpr<'src> {
    pub kind: UntypedExpr<'src>,
    pub type_info: TypeInfo,
    pub node_id: NodeId,
}

// Type Information
#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Known(Type),
    Unknown,
    Inferred(Type),
}

// Core Type Representation
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    TypeVar(usize),
    Struct(String, Vec<(String, Type)>),
    Function(Vec<Type>, Box<Type>),
    List(Box<Type>),
    Unit,
}

// Type Inference Errors
#[derive(Debug)]
pub enum TypeError {
    UnificationFail(Type, Type),
    InfiniteType(usize, Type),
    UnknownVariable(String),
    NotAFunction(Type),
    InvalidTypeAnnotation(String),
}

// Type Environment and Substitution
type TypeEnv = HashMap<NodeId, Type>;
type Substitution = HashMap<usize, Type>;

impl Type {
    fn occurs_in(&self, var: usize) -> bool {
        match self {
            Type::TypeVar(v) => *v == var,
            Type::Function(params, ret) => {
                params.iter().any(|p| p.occurs_in(var)) || ret.occurs_in(var)
            }
            Type::Struct(_, fields) => fields.iter().any(|(_, t)| t.occurs_in(var)),
            Type::List(elem_ty) => elem_ty.occurs_in(var),
            _ => false,
        }
    }

    fn apply_subst(&self, subst: &Substitution) -> Type {
        match self {
            Type::TypeVar(v) => subst.get(v).cloned().unwrap_or_else(|| self.clone()),
            Type::Function(params, ret) => Type::Function(
                params.iter().map(|p| p.apply_subst(subst)).collect(),
                Box::new(ret.apply_subst(subst)),
            ),
            Type::Struct(name, fields) => Type::Struct(
                name.clone(),
                fields
                    .iter()
                    .map(|(f_name, f_type)| (f_name.clone(), f_type.apply_subst(subst)))
                    .collect(),
            ), // Updated: Apply to field types
            Type::List(elem_ty) => Type::List(Box::new(elem_ty.apply_subst(subst))),
            _ => self.clone(),
        }
    }
}

fn unify(t1: &Type, t2: &Type, subst: &Substitution) -> Result<Substitution, TypeError> {
    let t1 = t1.apply_subst(subst);
    let t2 = t2.apply_subst(subst);

    match (&t1, &t2) {
        (Type::Int, Type::Int)
        | (Type::Float, Type::Float)
        | (Type::Bool, Type::Bool)
        | (Type::String, Type::String)
        | (Type::Unit, Type::Unit) => Ok(subst.clone()),
        (Type::Function(p1, r1), Type::Function(p2, r2)) if p1.len() == p2.len() => {
            let mut new_subst = subst.clone();
            for (p1, p2) in p1.iter().zip(p2.iter()) {
                new_subst = unify(p1, p2, &new_subst)?;
            }
            unify(r1, r2, &new_subst)
        }
        (Type::TypeVar(v), t) | (t, Type::TypeVar(v)) => {
            if t.occurs_in(*v) {
                return Err(TypeError::InfiniteType(*v, t.clone()));
            }
            let mut new_subst = subst.clone();
            new_subst.insert(*v, t.clone());
            Ok(new_subst)
        }
        (Type::Struct(n1, f1), Type::Struct(n2, f2)) if n1 == n2 && f1.len() == f2.len() => {
            let mut new_subst = subst.clone();
            for ((f1_name, f1_type), (f2_name, f2_type)) in f1.iter().zip(f2.iter()) {
                if f1_name != f2_name {
                    return Err(TypeError::UnificationFail(t1.clone(), t2.clone()));
                }
                new_subst = unify(f1_type, f2_type, &new_subst)?;
            }
            Ok(new_subst)
        }
        (Type::List(t1_elem), Type::List(t2_elem)) => unify(t1_elem, t2_elem, subst),
        _ => Err(TypeError::UnificationFail(t1.clone(), t2.clone())),
    }
}

pub struct TypeInferencer {
    env: TypeEnv,
    subst: Substitution,
    next_var_id: usize,
    resolution_map: NameResolutionMap,
    struct_names: HashSet<String>,
    struct_fields: HashMap<String, Vec<(String, Type)>>,
}

impl TypeInferencer {
    pub fn new(resolution_map: NameResolutionMap) -> Self {
        Self {
            env: HashMap::new(),
            subst: HashMap::new(),
            next_var_id: 0,
            resolution_map,
            struct_names: HashSet::new(),
            struct_fields: HashMap::new(),
        }
    }

    fn fresh_type_var(&mut self) -> Type {
        let id = self.next_var_id;
        self.next_var_id += 1;
        Type::TypeVar(id)
    }

    pub fn convert_to_typed_ast<'src>(&mut self, expr: &UntypedExpr<'src>) -> TypedExpr<'src> {
        let node_id = self.resolution_map.get_id();
        let type_info = match expr {
            UntypedExpr::Value(Value::Num(_)) => TypeInfo::Known(Type::Int),
            UntypedExpr::Value(Value::Float(_)) => TypeInfo::Known(Type::Float),
            UntypedExpr::Value(Value::String(_)) => TypeInfo::Known(Type::String),
            _ => TypeInfo::Unknown,
        };
        TypedExpr {
            kind: expr.clone(),
            type_info,
            node_id,
        }
    }

    fn infer_expr(&mut self, expr: &mut TypedExpr) -> Result<Type, TypeError> {
        if let TypeInfo::Known(t) | TypeInfo::Inferred(t) = &expr.type_info {
            return Ok(t.clone());
        }

        let inferred_type = match &mut expr.kind {
            UntypedExpr::Value(Value::Num(_)) => Type::Int,
            UntypedExpr::Value(Value::Float(_)) => Type::Float,
            UntypedExpr::Value(Value::String(_)) => Type::String,
            UntypedExpr::Value(Value::Identifier(name)) => {
                if let Some(decl_id) = self.resolution_map.resolve_name(name) {
                    self.env
                        .get(&decl_id)
                        .cloned()
                        .ok_or_else(|| TypeError::UnknownVariable(name.to_string()))?
                } else {
                    return Err(TypeError::UnknownVariable(name.to_string()));
                }
            }
            UntypedExpr::Let {
                id: Value::Identifier(name),
                pat,
                expr: init_expr,
                ..
            } => {
                let mut init_typed = self.convert_to_typed_ast(init_expr);
                let init_type = self.infer_expr(&mut init_typed)?;
                *init_expr = Box::new(init_typed.kind);

                let var_type = match pat {
                    TypePath::Typed {
                        ident: Value::Identifier(ty_name),
                    } => {
                        let ann_type = self.type_from_annotation(ty_name)?;
                        self.subst = unify(&init_type, &ann_type, &self.subst)?;
                        ann_type
                    }
                    _ => init_type,
                };

                if let Some(decl_id) = self.resolution_map.get_declaration_id(name) {
                    self.env.insert(decl_id, var_type.clone());
                }
                Type::Unit
            }
            UntypedExpr::BinOp { left, op, right } => {
                let mut left_typed = self.convert_to_typed_ast(left);
                let mut right_typed = self.convert_to_typed_ast(right);
                let t_left = self.infer_expr(&mut left_typed)?;
                let t_right = self.infer_expr(&mut right_typed)?;
                *left = Box::new(left_typed.kind);
                *right = Box::new(right_typed.kind);

                match op {
                    InfixOpKind::Add | InfixOpKind::Sub | InfixOpKind::Mul | InfixOpKind::Div => {
                        self.subst = unify(&t_left, &t_right, &self.subst)?;
                        self.subst = unify(&t_left, &Type::Int, &self.subst)?; // Assuming Int for simplicity
                        Type::Int
                    }
                    InfixOpKind::Equals | InfixOpKind::NotEq => {
                        self.subst = unify(&t_left, &t_right, &self.subst)?;
                        Type::Bool
                    }
                    _ => unimplemented!("Operator {:?}", op),
                }
            }
            UntypedExpr::Call { name, args } => {
                let mut name_typed = self.convert_to_typed_ast(name);
                let func_type = self.infer_expr(&mut name_typed)?;
                *name = Box::new(name_typed.kind);

                let mut arg_types = Vec::new();
                for arg in args.iter_mut() {
                    let mut arg_typed = self.convert_to_typed_ast(arg);
                    arg_types.push(self.infer_expr(&mut arg_typed)?);
                    *arg = arg_typed.kind;
                }

                let ret_type = self.fresh_type_var();
                let expected_type = Type::Function(arg_types, Box::new(ret_type.clone()));
                self.subst = unify(&func_type, &expected_type, &self.subst)?;
                ret_type.apply_subst(&self.subst)
            }
            UntypedExpr::Block { statements } => {
                let mut block_type = Type::Unit;
                for stmt in statements.iter_mut() {
                    let mut stmt_typed = self.convert_to_typed_ast(stmt);
                    block_type = self.infer_expr(&mut stmt_typed)?;
                    *stmt = stmt_typed.kind;
                }
                block_type
            }
            UntypedExpr::Fn {
                name,
                params,
                retty,
                body,
            } => {
                // Treat retty as a type annotation, not an expression to infer
                let ret_type = match retty.as_ref() {
                    UntypedExpr::Value(Value::Identifier(ty_name)) => {
                        self.type_from_annotation(ty_name)?
                    }
                    _ => {
                        return Err(TypeError::InvalidTypeAnnotation(
                            "Invalid return type".to_string(),
                        ))
                    }
                };

                let mut param_types = Vec::new();
                for param in params {
                    if let (Value::Identifier(p_name), Value::Identifier(ty_name)) =
                        (&param.name, &param.ty)
                    {
                        let p_type = self.type_from_annotation(ty_name)?;
                        param_types.push(p_type.clone());
                        if let Some(decl_id) = self.resolution_map.get_declaration_id(p_name) {
                            self.env.insert(decl_id, p_type);
                        }
                    }
                }

                let mut body_typed = self.convert_to_typed_ast(body);
                let body_type = self.infer_expr(&mut body_typed)?;
                *body = Box::new(body_typed.kind);

                self.subst = unify(&body_type, &ret_type, &self.subst)?;
                let fn_type = Type::Function(param_types, Box::new(ret_type));

                if let Value::Identifier(fn_name) = name {
                    if let Some(decl_id) = self.resolution_map.get_declaration_id(fn_name) {
                        self.env.insert(decl_id, fn_type.clone());
                    }
                }
                fn_type
            }
            UntypedExpr::Struct {
                id: Value::Identifier(name),
                fields,
            } => {
                let mut field_types = Vec::new();
                for param in fields {
                    if let (Value::Identifier(f_name), Value::Identifier(ty_name)) =
                        (&param.name, &param.ty)
                    {
                        let f_type = self.type_from_annotation(ty_name)?;
                        field_types.push((f_name.to_string(), f_type));
                    }
                }
                self.struct_names.insert(name.to_string());
                self.struct_fields
                    .insert(name.to_string(), field_types.clone()); // Store fields
                let struct_type = Type::Struct(name.to_string(), field_types);
                if let Some(decl_id) = self.resolution_map.get_declaration_id(name) {
                    self.env.insert(decl_id, struct_type.clone());
                }
                Type::Unit
            }
            UntypedExpr::FieldAccess { id, field } => {
                let mut expr_typed = self.convert_to_typed_ast(id);
                let expr_type = self.infer_expr(&mut expr_typed)?;
                *id = Box::new(expr_typed.kind);

                match field {
                    Value::Identifier(field_name) => {
                        match expr_type {
                            Type::Struct(struct_name, fields) => {
                                for (f_name, f_type) in &fields {
                                    if f_name == *field_name {
                                        return Ok(f_type.clone());
                                    }
                                }
                                Err(TypeError::InvalidTypeAnnotation(format!(
                                    "Field {} not found in struct {}",
                                    field_name, struct_name
                                )))
                            }
                            _ => Err(TypeError::UnificationFail(
                                expr_type,
                                Type::Struct("unknown".to_string(), vec![]), // Dummy struct type for error
                            )),
                        }
                    }
                    _ => Err(TypeError::InvalidTypeAnnotation(
                        "Field must be an identifier".to_string(),
                    )),
                }?
            }
            UntypedExpr::List { items } => {
                if items.is_empty() {
                    // Empty list: polymorphic type List(a)
                    Type::List(Box::new(self.fresh_type_var()))
                } else {
                    // Non-empty list: unify all elements to the same type
                    let mut elem_type = None;
                    for elem in items.iter_mut() {
                        let mut elem_typed = self.convert_to_typed_ast(elem);
                        let t = self.infer_expr(&mut elem_typed)?;
                        *elem = elem_typed.kind;
                        match elem_type {
                            None => elem_type = Some(t),
                            Some(ref prev_t) => {
                                self.subst = unify(prev_t, &t, &self.subst)?;
                            }
                        }
                    }
                    Type::List(Box::new(elem_type.unwrap())) // Safe since non-empty
                }
            }
            UntypedExpr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Infer condition type and unify with Bool
                let mut cond_typed = self.convert_to_typed_ast(condition);
                let cond_type = self.infer_expr(&mut cond_typed)?;
                *condition = Box::new(cond_typed.kind);
                self.subst = unify(&cond_type, &Type::Bool, &self.subst)?;

                // Infer then and else branch types and unify them
                let mut then_typed = self.convert_to_typed_ast(then_branch);
                let then_type = self.infer_expr(&mut then_typed)?;
                *then_branch = Box::new(then_typed.kind);

                let mut else_typed = self.convert_to_typed_ast(else_branch);
                let else_type = self.infer_expr(&mut else_typed)?;
                *else_branch = Box::new(else_typed.kind);

                self.subst = unify(&then_type, &else_type, &self.subst)?;
                then_type // Could use else_type too, since they're unified
            }
            _ => self.fresh_type_var(),
        };

        expr.type_info = TypeInfo::Inferred(inferred_type.clone());
        Ok(inferred_type)
    }

    fn type_from_annotation(&self, type_name: &str) -> Result<Type, TypeError> {
        match type_name {
            "i8" | "i32" | "i64" => Ok(Type::Int),
            "f64" => Ok(Type::Float),
            "bool" => Ok(Type::Bool),
            "string" => Ok(Type::String),
            name if self.struct_names.contains(name) => {
                let fields = self.struct_fields.get(name).cloned().unwrap_or(vec![]); // Should always exist due to struct_names
                Ok(Type::Struct(name.to_string(), fields))
            }
            _ => Err(TypeError::InvalidTypeAnnotation(type_name.to_string())),
        }
    }

    pub fn infer_program(&mut self, program: &mut [TypedExpr]) -> Result<(), Vec<TypeError>> {
        let mut errors = Vec::new();
        for expr in program.iter_mut() {
            if let Err(e) = self.infer_expr(expr) {
                errors.push(e);
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn convert_program<'src>(&mut self, program: &[UntypedExpr<'src>]) -> Vec<TypedExpr<'src>> {
        program
            .iter()
            .map(|expr| self.convert_to_typed_ast(expr))
            .collect()
    }
}

// Test Function
pub fn test_type_inference() {
    let test_program = vec![
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
                statements: vec![UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
                }],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(5))),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("y"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("i32"),
                        },
                        expr: Box::new(UntypedExpr::Value(Value::Num(10))),
                        constness: Const::Yes,
                    },
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

    use crate::reks_type::resolve::NameResolver; // Adjust path
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
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

// New Test for Unannotated Let Expressions
pub fn test_unannotated_let_inference() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                // const x = 55;
                UntypedExpr::Let {
                    id: Value::Identifier("x"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Num(55))),
                    constness: Const::Yes,
                },
                // // const y: i8 = 55;
                // UntypedExpr::Let {
                //     id: Value::Identifier("y"),
                //     pat: TypePath::Typed {
                //         ident: Value::Identifier("i8"),
                //     },
                //     expr: Box::new(UntypedExpr::Value(Value::Num(55))),
                //     constness: Const::Yes,
                // },
                // mut z = 42;
                UntypedExpr::Let {
                    id: Value::Identifier("z"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Num(42))),
                    constness: Const::No,
                },
                // Return x + z to use the variables
                UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("z"))),
                },
            ],
        }),
    }];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                // Print types of variables in the block for clarity
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

// Test for All Unknowns
pub fn test_all_unknowns_inference() {
    let test_program = vec![
        UntypedExpr::Fn {
            name: Value::Identifier("add"),
            params: vec![
                Param {
                    name: Value::Identifier("a"),
                    ty: Value::Identifier("i32"),
                }, // No type annotation
                Param {
                    name: Value::Identifier("b"),
                    ty: Value::Identifier("i32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))), // No return type annotation
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
                }],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))), // No return type annotation
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(5))),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("y"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Num(42)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Value(Value::Identifier("y")), // Return y
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

// Test for Function Calls with Mandatory Annotations
pub fn test_function_call_inference() {
    let test_program = vec![
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
                statements: vec![UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
                }],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(5))),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("y"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Num(42)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("z"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("y")),
                                UntypedExpr::Value(Value::Identifier("x")),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Value(Value::Identifier("z")), // Return z
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

// Test for Structs and Function Calls
pub fn test_struct_function_inference() {
    let test_program = vec![
        // struct Point { x: i32, y: i32 }
        UntypedExpr::Struct {
            id: Value::Identifier("Point"),
            fields: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("y"),
                    ty: Value::Identifier("i32"),
                },
            ],
        },
        // fn move_point(p: Point, dx: i32) -> Point { p }
        UntypedExpr::Fn {
            name: Value::Identifier("move_point"),
            params: vec![
                Param {
                    name: Value::Identifier("p"),
                    ty: Value::Identifier("Point"),
                },
                Param {
                    name: Value::Identifier("dx"),
                    ty: Value::Identifier("i32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("Point"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Value(Value::Identifier("p")), // Just return p
                ],
            }),
        },
        // fn main() -> i32 { ... }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // Assume p0 is a Point defined elsewhere (placeholder)
                    UntypedExpr::Let {
                        id: Value::Identifier("p0"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("Point"),
                        }, // Temporary annotation
                        expr: Box::new(UntypedExpr::Value(Value::Identifier("some_point"))), // Placeholder
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("p1"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("move_point"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("p0")),
                                UntypedExpr::Value(Value::Num(5)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("p2"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("move_point"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("p1")),
                                UntypedExpr::Value(Value::Num(10)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Value(Value::Num(42)), // Return 42 for i32
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_generic_struct_inference() {
    let test_program = vec![
        UntypedExpr::Struct {
            id: Value::Identifier("Point"),
            fields: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("y"),
                    ty: Value::Identifier("i32"),
                },
            ],
        },
        UntypedExpr::Struct {
            id: Value::Identifier("Vector"),
            fields: vec![
                Param {
                    name: Value::Identifier("dx"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("dy"),
                    ty: Value::Identifier("i32"),
                },
            ],
        },
        UntypedExpr::Fn {
            name: Value::Identifier("move_point"),
            params: vec![
                Param {
                    name: Value::Identifier("p"),
                    ty: Value::Identifier("Point"),
                },
                Param {
                    name: Value::Identifier("v"),
                    ty: Value::Identifier("Vector"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("Point"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Value(Value::Identifier("p")), // Return the parameter p
                ],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![
                Param {
                    name: Value::Identifier("initial_p"),
                    ty: Value::Identifier("Point"),
                },
                Param {
                    name: Value::Identifier("initial_v"),
                    ty: Value::Identifier("Vector"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("p1"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("move_point"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("initial_p")),
                                UntypedExpr::Value(Value::Identifier("initial_v")),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Value(Value::Num(42)), // Return 42
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_list_inference() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                UntypedExpr::Let {
                    id: Value::Identifier("xs"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::List {
                        items: vec![
                            UntypedExpr::Value(Value::Num(1)),
                            UntypedExpr::Value(Value::Num(2)),
                            UntypedExpr::Value(Value::Num(3)),
                        ],
                    }),
                    constness: Const::Yes,
                },
                UntypedExpr::Let {
                    id: Value::Identifier("ys"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::List {
                        items: vec![], // Empty list
                    }),
                    constness: Const::Yes,
                },
                UntypedExpr::Value(Value::Num(42)), // Return 42
            ],
        }),
    }];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_if_statement_inference() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                UntypedExpr::Let {
                    id: Value::Identifier("x"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Num(5))),
                    constness: Const::Yes,
                },
                UntypedExpr::Let {
                    id: Value::Identifier("y"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::If {
                        condition: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                            op: InfixOpKind::Equals,
                            right: Box::new(UntypedExpr::Value(Value::Num(5))),
                        }),
                        then_branch: Box::new(UntypedExpr::Value(Value::Num(10))),
                        else_branch: Box::new(UntypedExpr::Value(Value::Num(20))),
                    }),
                    constness: Const::Yes,
                },
                UntypedExpr::Value(Value::Identifier("y")), // Return y
            ],
        }),
    }];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_field_access_inference() {
    let test_program = vec![
        UntypedExpr::Struct {
            id: Value::Identifier("Point"),
            fields: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("y"),
                    ty: Value::Identifier("i32"),
                },
            ],
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("p"),
                ty: Value::Identifier("Point"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("x_coord"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                            field: Value::Identifier("x"),
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Value(Value::Identifier("x_coord")), // Return x_coord
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_nested_struct_field_access() {
    let test_program = vec![
        // struct Inner { a: i32 }
        UntypedExpr::Struct {
            id: Value::Identifier("Inner"),
            fields: vec![Param {
                name: Value::Identifier("a"),
                ty: Value::Identifier("i32"),
            }],
        },
        // struct Outer { inner: Inner }
        UntypedExpr::Struct {
            id: Value::Identifier("Outer"),
            fields: vec![Param {
                name: Value::Identifier("inner"),
                ty: Value::Identifier("Inner"),
            }],
        },
        // fn main(outer: Outer) -> i32 { ... }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("outer"),
                ty: Value::Identifier("Outer"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // const inner = outer.inner
                    UntypedExpr::Let {
                        id: Value::Identifier("inner"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("outer"))),
                            field: Value::Identifier("inner"),
                        }),
                        constness: Const::Yes,
                    },
                    // const a = inner.a
                    UntypedExpr::Let {
                        id: Value::Identifier("a"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("inner"))),
                            field: Value::Identifier("a"),
                        }),
                        constness: Const::Yes,
                    },
                    // return a
                    UntypedExpr::Value(Value::Identifier("a")),
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_unannotated_let_with_field_access() {
    let test_program = vec![
        // struct Point { x: i32, y: i32 }
        UntypedExpr::Struct {
            id: Value::Identifier("Point"),
            fields: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("y"),
                    ty: Value::Identifier("i32"),
                },
            ],
        },
        // fn main(p: Point) -> i32 { ... }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("p"),
                ty: Value::Identifier("Point"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // const x = p.y
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Empty, // Unannotated
                        expr: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                            field: Value::Identifier("y"),
                        }),
                        constness: Const::Yes,
                    },
                    // return x
                    UntypedExpr::Value(Value::Identifier("x")),
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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
