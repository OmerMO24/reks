use std::collections::{HashMap, HashSet};

// Assuming these are defined in your existing code
use crate::reks_parse::operators::InfixOpKind; // Adjust path as needed
use crate::reks_parse::utnode::{Const, Param, TypePath, UntypedExpr, Value}; // Adjust path
use crate::reks_type::resolve::{NameResolutionMap, NodeId, Symbol}; // Adjust path

// In src/reks_type/infer.rs (or wherever TypedExpr is defined)

// Remove the old TypedExpr definition and replace with:
#[derive(Debug, Clone)]
pub struct TypedExpr<'src> {
    pub kind: TypedExprKind<'src>,
    pub type_info: TypeInfo,
    pub node_id: NodeId,
}

#[derive(Debug, Clone)]
pub enum TypedExprKind<'src> {
    Value(Value<'src>),
    Let {
        id: Value<'src>,
        pat: TypePath<'src>,
        expr: Box<TypedExpr<'src>>,
        constness: Const,
    },
    Fn {
        name: Value<'src>,
        params: Vec<Param<'src>>,
        retty: Box<TypedExpr<'src>>,
        body: Box<TypedExpr<'src>>,
    },
    Call {
        name: Box<TypedExpr<'src>>,
        args: Vec<TypedExpr<'src>>,
    },
    BinOp {
        left: Box<TypedExpr<'src>>,
        op: InfixOpKind,
        right: Box<TypedExpr<'src>>,
    },
    Block {
        statements: Vec<TypedExpr<'src>>,
    },
    Struct {
        id: Value<'src>,
        fields: Vec<Param<'src>>,
    },
    List {
        elements: Vec<TypedExpr<'src>>,
    },
    If {
        condition: Box<TypedExpr<'src>>,
        then_branch: Box<TypedExpr<'src>>,
        else_branch: Box<TypedExpr<'src>>,
    },
    FieldAccess {
        expr: Box<TypedExpr<'src>>,
        field: Value<'src>,
    },
    Assign {
        target: Box<TypedExpr<'src>>,
        expr: Box<TypedExpr<'src>>,
    },
    // New variant for struct initialization
    StructInit {
        id: Value<'src>,                             // Struct name
        fields: Vec<(Value<'src>, TypedExpr<'src>)>, // (field_name, value)
    },
    Index {
        expr: Box<TypedExpr<'src>>,
        index: Box<TypedExpr<'src>>,
    },
}

// Type Information
#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Known(Type),
    Unknown,
    Inferred(Type),
}

impl TypeInfo {
    pub fn into_type(self) -> Option<Type> {
        match self {
            TypeInfo::Known(t) | TypeInfo::Inferred(t) => Some(t),
            TypeInfo::Unknown => None,
        }
    }

    pub fn as_type(&self) -> Option<&Type> {
        match self {
            TypeInfo::Known(t) | TypeInfo::Inferred(t) => Some(t),
            TypeInfo::Unknown => None,
        }
    }
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

use std::fmt;

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
            Type::String => write!(f, "String"),
            Type::TypeVar(id) => write!(f, "t{}", id),
            Type::Struct(name, _) => write!(f, "{}", name), // Simplified, could list fields
            Type::Function(params, ret) => {
                let param_str = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "fn({}) -> {}", param_str, ret)
            }
            Type::List(elem_ty) => write!(f, "[{}]", elem_ty),
            Type::Unit => write!(f, "Unit"),
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::UnificationFail(t1, t2) => write!(f, "Cannot unify {} with {}", t1, t2),
            TypeError::InvalidStructInit(msg) => write!(f, "Invalid struct init: {}", msg),
            TypeError::InfiniteType(id, ty) => write!(f, "Infinite type t{} in {}", id, ty),
            TypeError::UnknownVariable(name) => write!(f, "Unknown variable: {}", name),
            TypeError::NotAFunction(ty) => write!(f, "Not a function: {}", ty),
            TypeError::InvalidTypeAnnotation(msg) => write!(f, "Invalid type annotation: {}", msg),
            TypeError::ImmutableAssignment(name) => {
                write!(f, "Cannot assign to immutable variable: {}", name)
            }
            TypeError::UndefinedStruct(name) => write!(f, "Undefined struct: {}", name),
            TypeError::UnknownField(name) => write!(f, "Unknown field: {}", name),
            TypeError::NotAStruct(name) => write!(f, "Not a struct: {}", name),
            TypeError::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            _ => todo!(),
        }
    }
}

// Type Inference Errors
#[derive(Debug)]
pub enum TypeError {
    UnificationFail(Type, Type),
    InvalidStructInit(String),
    InfiniteType(usize, Type),
    UnknownVariable(String),
    NotAFunction(Type),
    InvalidTypeAnnotation(String),
    ImmutableAssignment(String),
    UndefinedStruct(String),
    UnknownField(String),
    NotAStruct(String),
    TypeMismatch { expected: String, found: String },
    TypeInferenceFailed,
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
    pub env: TypeEnv,
    pub subst: Substitution,
    pub next_var_id: usize,
    pub resolution_map: NameResolutionMap,
    pub struct_names: HashSet<String>,
    pub struct_fields: HashMap<String, Vec<(String, Type)>>,
    mutability: HashMap<NodeId, bool>, // New: Tracks if variables are mutable (true) or const (false)
}

impl<'src> TypeInferencer {
    pub fn new(resolution_map: NameResolutionMap) -> Self {
        Self {
            env: HashMap::new(),
            subst: HashMap::new(),
            next_var_id: 0,
            resolution_map,
            struct_names: HashSet::new(),
            struct_fields: HashMap::new(),
            mutability: HashMap::new(),
        }
    }

    fn fresh_type_var(&mut self) -> Type {
        let id = self.next_var_id;
        self.next_var_id += 1;
        Type::TypeVar(id)
    }

    pub fn convert_to_typed_ast(&mut self, expr: &UntypedExpr<'src>) -> TypedExpr<'src> {
        let node_id = self.resolution_map.get_id();
        let type_info = match expr {
            UntypedExpr::Value(Value::Num(_)) => TypeInfo::Known(Type::Int),
            UntypedExpr::Value(Value::Float(_)) => TypeInfo::Known(Type::Float),
            UntypedExpr::Value(Value::String(_)) => TypeInfo::Known(Type::String),
            UntypedExpr::Value(Value::Identifier(name)) => {
                // Handle type annotations like "i32" in retty or params
                match *name {
                    "i32" | "i64" | "i8" => TypeInfo::Known(Type::Int),
                    "f64" => TypeInfo::Known(Type::Float),
                    "bool" => TypeInfo::Known(Type::Bool),
                    "string" => TypeInfo::Known(Type::String),
                    _ => TypeInfo::Unknown, // Variables or structs to be resolved later
                }
            }
            _ => TypeInfo::Unknown,
        };

        let kind = match expr {
            UntypedExpr::Value(val) => TypedExprKind::Value(val.clone()),
            UntypedExpr::Let {
                id,
                pat,
                expr: init_expr,
                constness,
            } => TypedExprKind::Let {
                id: id.clone(),
                pat: pat.clone(),
                expr: Box::new(self.convert_to_typed_ast(init_expr)),
                constness: *constness,
            },
            UntypedExpr::Fn {
                name,
                params,
                retty,
                body,
            } => TypedExprKind::Fn {
                name: name.clone(),
                params: params.clone(),
                retty: Box::new(self.convert_to_typed_ast(retty)),
                body: Box::new(self.convert_to_typed_ast(body)),
            },
            UntypedExpr::Call { name, args } => TypedExprKind::Call {
                name: Box::new(self.convert_to_typed_ast(name)),
                args: args
                    .iter()
                    .map(|arg| self.convert_to_typed_ast(arg))
                    .collect(),
            },
            UntypedExpr::BinOp { left, op, right } => TypedExprKind::BinOp {
                left: Box::new(self.convert_to_typed_ast(left)),
                op: *op,
                right: Box::new(self.convert_to_typed_ast(right)),
            },
            UntypedExpr::Block { statements } => TypedExprKind::Block {
                statements: statements
                    .iter()
                    .map(|stmt| self.convert_to_typed_ast(stmt))
                    .collect(),
            },
            UntypedExpr::Struct { id, fields } => TypedExprKind::Struct {
                id: id.clone(),
                fields: fields.clone(),
            },
            UntypedExpr::List { items } => TypedExprKind::List {
                elements: items
                    .iter()
                    .map(|elem| self.convert_to_typed_ast(elem))
                    .collect(),
            },
            UntypedExpr::If {
                condition,
                then_branch,
                else_branch,
            } => TypedExprKind::If {
                condition: Box::new(self.convert_to_typed_ast(condition)),
                then_branch: Box::new(self.convert_to_typed_ast(then_branch)),
                else_branch: Box::new(self.convert_to_typed_ast(else_branch)),
            },
            UntypedExpr::FieldAccess { id, field } => TypedExprKind::FieldAccess {
                expr: Box::new(self.convert_to_typed_ast(id)),
                field: field.clone(),
            },
            UntypedExpr::Assign { left, right } => TypedExprKind::Assign {
                target: Box::new(self.convert_to_typed_ast(left)),
                expr: Box::new(self.convert_to_typed_ast(right)),
            },
            UntypedExpr::StructInit { id, fields } => TypedExprKind::StructInit {
                id: id.clone(),
                fields: fields
                    .iter()
                    .map(|(name, expr)| (name.clone(), self.convert_to_typed_ast(expr)))
                    .collect(),
            },
            UntypedExpr::Index { expr, index } => TypedExprKind::Index {
                expr: Box::new(self.convert_to_typed_ast(expr)),
                index: Box::new(self.convert_to_typed_ast(index)),
            },
            _ => todo!(),
        };

        TypedExpr {
            kind,
            type_info,
            node_id,
        }
    }

    fn infer_expr(&mut self, expr: TypedExpr<'src>) -> Result<TypedExpr<'src>, TypeError> {
        if let TypeInfo::Known(t) | TypeInfo::Inferred(t) = &expr.type_info {
            return Ok(expr); // Already typed, no need to re-infer
        }

        let (inferred_type, new_kind) = match expr.kind {
            TypedExprKind::Value(val) => match val {
                Value::Num(_) => (Type::Int, TypedExprKind::Value(val)),
                Value::Float(_) => (Type::Float, TypedExprKind::Value(val)),
                Value::String(_) => (Type::String, TypedExprKind::Value(val)),
                Value::Identifier(name) => {
                    if let Some(decl_id) = self.resolution_map.resolve_name(name) {
                        let ty = self
                            .env
                            .get(&decl_id)
                            .cloned()
                            .ok_or_else(|| TypeError::UnknownVariable(name.to_string()))?;
                        (ty, TypedExprKind::Value(val))
                    } else {
                        return Err(TypeError::UnknownVariable(name.to_string()));
                    }
                }
            },
            TypedExprKind::Let {
                id: Value::Identifier(name),
                pat,
                expr: init_expr,
                constness,
            } => {
                let init_typed = self.infer_expr(*init_expr)?;
                let init_type = init_typed.type_info.clone().into_type().unwrap();

                let var_type = match pat {
                    TypePath::Typed {
                        ident: Value::Identifier(ty_name),
                    } => {
                        let ann_type = self.type_from_annotation(ty_name)?;
                        self.subst = unify(&init_type, &ann_type, &self.subst)?;
                        ann_type
                    }
                    TypePath::Typed { ident: _ } => {
                        return Err(TypeError::InvalidTypeAnnotation(
                            "Type annotation must be an identifier".to_string(),
                        ));
                    }
                    TypePath::Empty => init_type,
                };

                if let Some(decl_id) = self.resolution_map.get_declaration_id(name) {
                    self.env.insert(decl_id, var_type.clone());
                    self.mutability.insert(decl_id, constness == Const::No);
                }
                (
                    Type::Unit,
                    TypedExprKind::Let {
                        id: Value::Identifier(name),
                        pat,
                        expr: Box::new(init_typed),
                        constness,
                    },
                )
            }
            TypedExprKind::Fn {
                name,
                params,
                retty,
                body,
            } => {
                let ret_typed = self.infer_expr(*retty)?;
                let ret_type = ret_typed.type_info.clone().into_type().unwrap();

                let mut param_types = Vec::new();
                for param in &params {
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

                let body_typed = self.infer_expr(*body)?;
                let body_type = body_typed.type_info.clone().into_type().unwrap();

                self.subst = unify(&body_type, &ret_type, &self.subst)?;
                let fn_type = Type::Function(param_types, Box::new(ret_type));

                if let Value::Identifier(fn_name) = name {
                    if let Some(decl_id) = self.resolution_map.get_declaration_id(fn_name) {
                        self.env.insert(decl_id, fn_type.clone());
                    }
                }
                (
                    fn_type,
                    TypedExprKind::Fn {
                        name,
                        params,
                        retty: Box::new(ret_typed),
                        body: Box::new(body_typed),
                    },
                )
            }
            TypedExprKind::Call { name, args } => {
                let name_typed = self.infer_expr(*name)?;
                let func_type = name_typed.type_info.clone().into_type().unwrap();

                let mut arg_types = Vec::new();
                let mut typed_args = Vec::new();
                for arg in args {
                    let arg_typed = self.infer_expr(arg)?;
                    arg_types.push(arg_typed.type_info.clone().into_type().unwrap());
                    typed_args.push(arg_typed);
                }

                let ret_type = self.fresh_type_var();
                let expected_type = Type::Function(arg_types, Box::new(ret_type.clone()));
                self.subst = unify(&func_type, &expected_type, &self.subst)?;
                (
                    ret_type.apply_subst(&self.subst),
                    TypedExprKind::Call {
                        name: Box::new(name_typed),
                        args: typed_args,
                    },
                )
            }
            TypedExprKind::BinOp { left, op, right } => {
                let left_typed = self.infer_expr(*left)?;
                let right_typed = self.infer_expr(*right)?;
                let t_left = left_typed.type_info.clone().into_type().unwrap();
                let t_right = right_typed.type_info.clone().into_type().unwrap();

                let ty = match op {
                    InfixOpKind::Add | InfixOpKind::Sub | InfixOpKind::Mul | InfixOpKind::Div => {
                        self.subst = unify(&t_left, &t_right, &self.subst)?;
                        self.subst = unify(&t_left, &Type::Int, &self.subst)?;
                        Type::Int
                    }
                    InfixOpKind::Equals
                    | InfixOpKind::NotEq
                    | InfixOpKind::Greater
                    | InfixOpKind::Less => {
                        self.subst = unify(&t_left, &t_right, &self.subst)?;
                        Type::Bool
                    }
                    _ => Type::Unit,
                };
                (
                    ty,
                    TypedExprKind::BinOp {
                        left: Box::new(left_typed),
                        op,
                        right: Box::new(right_typed),
                    },
                )
            }
            TypedExprKind::Block { statements } => {
                let mut block_type = Type::Unit;
                let mut typed_statements = Vec::new();
                for stmt in statements {
                    let stmt_typed = self.infer_expr(stmt)?;
                    block_type = stmt_typed.type_info.clone().into_type().unwrap();
                    typed_statements.push(stmt_typed);
                }
                (
                    block_type,
                    TypedExprKind::Block {
                        statements: typed_statements,
                    },
                )
            }
            TypedExprKind::Struct {
                id: Value::Identifier(name),
                fields,
            } => {
                let mut field_types = Vec::new();
                for param in &fields {
                    if let (Value::Identifier(f_name), Value::Identifier(ty_name)) =
                        (&param.name, &param.ty)
                    {
                        let f_type = self.type_from_annotation(ty_name)?;
                        field_types.push((f_name.to_string(), f_type));
                    }
                }
                self.struct_names.insert(name.to_string());
                self.struct_fields
                    .insert(name.to_string(), field_types.clone());
                let struct_type = Type::Struct(name.to_string(), field_types);
                if let Some(decl_id) = self.resolution_map.get_declaration_id(name) {
                    self.env.insert(decl_id, struct_type.clone());
                }
                (
                    Type::Unit,
                    TypedExprKind::Struct {
                        id: Value::Identifier(name),
                        fields,
                    },
                )
            }
            TypedExprKind::List { elements } => {
                let mut typed_elements = Vec::new();
                let ty = if elements.is_empty() {
                    Type::List(Box::new(self.fresh_type_var()))
                } else {
                    let mut elem_type = None;
                    for elem in elements {
                        let elem_typed = self.infer_expr(elem)?;
                        let t = elem_typed.type_info.clone().into_type().unwrap();
                        typed_elements.push(elem_typed);
                        match elem_type {
                            None => elem_type = Some(t),
                            Some(ref prev_t) => {
                                self.subst = unify(prev_t, &t, &self.subst)?;
                            }
                        }
                    }
                    Type::List(Box::new(elem_type.unwrap()))
                };
                (
                    ty,
                    TypedExprKind::List {
                        elements: typed_elements,
                    },
                )
            }
            TypedExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_typed = self.infer_expr(*condition)?;
                let cond_type = cond_typed.type_info.clone().into_type().unwrap();
                self.subst = unify(&cond_type, &Type::Bool, &self.subst)?;

                let then_typed = self.infer_expr(*then_branch)?;
                let then_type = then_typed.type_info.clone().into_type().unwrap();

                let else_typed = self.infer_expr(*else_branch)?;
                let else_type = else_typed.type_info.clone().into_type().unwrap();

                self.subst = unify(&then_type, &else_type, &self.subst)?;
                (
                    then_type,
                    TypedExprKind::If {
                        condition: Box::new(cond_typed),
                        then_branch: Box::new(then_typed),
                        else_branch: Box::new(else_typed),
                    },
                )
            }
            TypedExprKind::FieldAccess { expr, field } => {
                let expr_typed = self.infer_expr(*expr)?;
                let expr_type = expr_typed.type_info.clone().into_type().unwrap();

                let ty = match field {
                    Value::Identifier(field_name) => match expr_type {
                        Type::Struct(_, fields) => {
                            let mut found_type = None;
                            for (f_name, f_type) in fields {
                                if f_name == field_name {
                                    found_type = Some(f_type.clone());
                                    break;
                                }
                            }
                            match found_type {
                                Some(t) => Ok(t),
                                None => Err(TypeError::InvalidTypeAnnotation(format!(
                                    "Field {} not found in struct",
                                    field_name
                                ))),
                            }
                        }
                        _ => Err(TypeError::UnificationFail(
                            expr_type,
                            Type::Struct("unknown".to_string(), vec![]),
                        )),
                    },
                    _ => Err(TypeError::InvalidTypeAnnotation(
                        "Field must be an identifier".to_string(),
                    )),
                }?;
                (
                    ty,
                    TypedExprKind::FieldAccess {
                        expr: Box::new(expr_typed),
                        field,
                    },
                )
            }
            TypedExprKind::Assign { target, expr } => {
                let target_typed = self.infer_expr(*target)?;
                let target_type = target_typed.type_info.clone().into_type().unwrap();

                if let TypedExprKind::Value(Value::Identifier(name)) = &target_typed.kind {
                    if let Some(decl_id) = self.resolution_map.resolve_name(name) {
                        let is_mutable = *self.mutability.get(&decl_id).unwrap_or(&false);
                        if !is_mutable {
                            return Err(TypeError::ImmutableAssignment(name.to_string()));
                        }
                    } else {
                        return Err(TypeError::UnknownVariable(name.to_string()));
                    }
                } else {
                    return Err(TypeError::InvalidTypeAnnotation(
                        "Assignment target must be an identifier".to_string(),
                    ));
                }

                let expr_typed = self.infer_expr(*expr)?;
                let expr_type = expr_typed.type_info.clone().into_type().unwrap();

                self.subst = unify(&target_type, &expr_type, &self.subst)?;
                (
                    Type::Unit,
                    TypedExprKind::Assign {
                        target: Box::new(target_typed),
                        expr: Box::new(expr_typed),
                    },
                )
            }
            TypedExprKind::StructInit { id, fields } => {
                let struct_name = if let Value::Identifier(name) = id {
                    name
                } else {
                    panic!("StructInit id must be an identifier"); // Temporary panic
                };
                // Resolve struct declaration
                let struct_decl_id = self
                    .resolution_map
                    .resolve_name(struct_name)
                    .expect("Struct type not found"); // Temporary expect
                let struct_symbol = self
                    .resolution_map
                    .declarations
                    .get(&struct_decl_id)
                    .expect("Struct declaration not found");
                let (struct_type_name, struct_fields) = match &struct_symbol.symbol {
                    Symbol::Struct { name, fields } => (name.clone(), fields.clone()),
                    _ => panic!("Expected a struct symbol"), // Temporary panic
                };
                // Build expected field types
                let expected_field_types: HashMap<String, Type> = struct_fields
                    .iter()
                    .map(|(name, ty_str)| {
                        (
                            name.clone(),
                            match ty_str.as_str() {
                                "i32" => Type::Int,
                                "f32" => Type::Float,
                                "bool" => Type::Bool,
                                "string" => Type::String,
                                s => Type::Struct(s.to_string(), vec![]),
                            },
                        )
                    })
                    .collect();
                // Infer field values
                let mut typed_fields = Vec::new();
                for (field_name, value_expr) in fields {
                    let field_name_str = if let Value::Identifier(name) = field_name {
                        name.clone()
                    } else {
                        panic!("Field name must be an identifier"); // Temporary panic
                    };
                    let typed_value = self
                        .infer_expr(value_expr.clone())
                        .expect("Failed to infer field value"); // Temporary expect
                    let value_type = typed_value
                        .type_info
                        .as_type()
                        .expect("Field value type inference failed");
                    let expected_type = expected_field_types
                        .get(field_name_str.clone())
                        .expect("Field not found in struct definition");
                    self.subst = unify(expected_type, value_type, &self.subst)
                        .expect("Type unification failed for field");
                    typed_fields.push((field_name.clone(), typed_value));
                }
                let struct_type = Type::Struct(
                    struct_type_name,
                    struct_fields
                        .iter()
                        .map(|(name, ty_str)| {
                            (
                                name.clone(),
                                match ty_str.as_str() {
                                    "i32" => Type::Int,
                                    "f32" => Type::Float,
                                    "bool" => Type::Bool,
                                    "string" => Type::String,
                                    s => Type::Struct(s.to_string(), vec![]),
                                },
                            )
                        })
                        .collect(),
                );
                (
                    struct_type.clone(),
                    TypedExprKind::StructInit {
                        id: id.clone(),
                        fields: typed_fields,
                    },
                )
            }
            TypedExprKind::Index { expr, index } => {
                let expr_typed = self.infer_expr(*expr)?;
                let expr_type = expr_typed
                    .type_info
                    .clone()
                    .into_type()
                    .ok_or_else(|| TypeError::TypeInferenceFailed)?;
                let index_typed = self.infer_expr(*index)?;
                let index_type = index_typed
                    .type_info
                    .clone()
                    .into_type()
                    .ok_or_else(|| TypeError::TypeInferenceFailed)?;
                self.subst = unify(&index_type, &Type::Int, &self.subst)?; // Index must be Int
                let element_type = match expr_type {
                    Type::List(elem_ty) => *elem_ty,
                    _ => {
                        return Err(TypeError::TypeMismatch {
                            expected: "list".to_string(),
                            found: expr_type.to_string(),
                        })
                    }
                };
                (
                    element_type.clone(),
                    TypedExprKind::Index {
                        expr: Box::new(expr_typed),
                        index: Box::new(index_typed),
                    },
                )
            }
            _ => todo!(),
        };

        Ok(TypedExpr {
            kind: new_kind,
            type_info: TypeInfo::Inferred(inferred_type),
            node_id: expr.node_id,
        })
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

    pub fn infer_program(
        &mut self,
        program: &[UntypedExpr<'src>],
    ) -> Result<Vec<TypedExpr<'src>>, Vec<TypeError>> {
        let mut typed_ast = program
            .iter()
            .map(|expr| self.convert_to_typed_ast(expr))
            .collect::<Vec<_>>();
        let mut result_ast = Vec::new();
        let mut errors = Vec::new();

        for expr in typed_ast {
            match self.infer_expr(expr) {
                Ok(typed_expr) => result_ast.push(typed_expr),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(result_ast)
        } else {
            Err(errors)
        }
    }

    pub fn convert_program(&mut self, program: &[UntypedExpr<'src>]) -> Vec<TypedExpr<'src>> {
        program
            .iter()
            .map(|expr| self.convert_to_typed_ast(expr))
            .collect()
    }
}
